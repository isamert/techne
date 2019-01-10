{-# LANGUAGE FlexibleContexts         #-}
module Main where

import TechnePrelude
import Err
import Syntax
import Desugar
import Parser
import Infer
import Pretty
import Core
import Renamer
import Eval


import Text.Megaparsec hiding (setParserState)
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad.State.Strict
import Control.Monad.Except
import Options.Applicative
import Data.Void (Void)
import System.Console.Haskeline
import System.Console.Haskeline.History (addHistory)
import System.Directory (getHomeDirectory)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as Map

-- ----------------------------------------------------------------------------
-- Data declerations
-- ----------------------------------------------------------------------------

-- Commandline options
data Options = Options
  { interactive :: Bool
  , output :: Text
  , input :: Maybe String }

-- Repl monad
data ReplS = ReplS { parserState  :: ParserS
                   , renamerState :: RenamerS
                   , genEnv       :: GenEnv
                   , typeEnv      :: TypeEnv
                   , env          :: Env
                   } deriving (Show, Eq)

type ReplStateM = StateT ReplS (ExceptT (TechneErr, ReplS) IO)
type ReplM = InputT (ReplStateM)

-- ----------------------------------------------------------------------------
-- Main
-- ----------------------------------------------------------------------------

-- | Get commandline options and display information if necessary.
main :: IO ()
main = runOptions =<< execParser opts
  where
    opts = info (options <**> helper)
      (fullDesc
     <> progDesc "Compile files or run an interactive shell."
     <> header "Techne")

-- ----------------------------------------------------------------------------
-- Commandline options
-- ----------------------------------------------------------------------------

optInteractive :: Parser Bool
optInteractive = switch $
    long "interactive"
    <> short 'i'
    <> help "Start an interactive Techne shell."

optOutput :: Parser Text
optOutput = strOption $
    long "output"
    <> short 'o'
    <> value ""
    <> metavar "<file>"
    <> help "Place the output into <file>"

optInput :: Parser (Maybe String)
optInput = optional $ argument str (metavar "<file>")

options :: Parser Options
options = Options
    <$> optInteractive
    <*> optOutput
    <*> optInput

-- | Do stuff depending on commandline options.
runOptions :: Options -> IO ()
runOptions (Options _ output (Just input)) = runInputT defaultSettings $ do
    result <- parseFile parseModule input
    putStrLn $ case result of
      Right a -> tgroom a
      Left a -> tpack $ case a of
                          ParserErr x -> errorBundlePretty x
                          y           -> groom y

runOptions (Options True _ _) = runRepl initReplS
runOptions (Options i outf inf) = putStrLn $ tgroom i <> tgroom outf <> tgroom inf

-- ----------------------------------------------------------------------------
-- REPL
-- ----------------------------------------------------------------------------

runRepl :: ReplS -> IO ()
runRepl s = do
    result <- runExceptT (evalStateT (runInputT replSettings repl) s)
    case result of
      Right x -> return ()
      Left (_, s) -> runRepl s


initReplS :: ReplS
initReplS = ReplS { parserState  = initParserS
                  , renamerState = initRenamerS True
                  , genEnv       = emptyGenEnv
                  , typeEnv      = initTypeEnv
                  , env          = defaultEnv
                  }

replSettings :: Settings ReplStateM
replSettings = setComplete (completeWordWithPrev Nothing " \t" replComplete)
                       Settings { historyFile = Just histfile
                                , complete = completeFilename
                                , autoAddHistory = True
                                }
    where histfile = unsafePerformIO getHomeDirectory ++ "/.technehist" -- Am I a monster?
          -- TODO: I should probably use XDG cache directory

-- TODO: modularize
replComplete :: String -> String -> ReplStateM [Completion]
replComplete left_ word = do
    let left = reverse left_
    case left of
       str
         | ":loadfile " `isPrefixOf` left -> listFiles word
       (':':_) -> return []
       "" -> case word of
               (':':_) -> return $ searchCmds word
               _ -> codeComplete word
    where searchCmds word = map simpleCompletion
                              $ filter (word `isPrefixOf`)
                                       (map ((":" ++) . tunpack . head . fst) cmds)
          codeComplete :: String -> ReplStateM [Completion]
          codeComplete word = do
              namemap <- gets genEnv
              let names = map tunpack $ Map.keys namemap
              return $ map simpleCompletion
                         $ filter (word `isPrefixOf`) names

repl :: ReplM ()
repl = do
    linestr <- getInputLine "> "
    case sstrip <$> linestr of
        Nothing         -> repl
        Just ""         -> repl
        Just (':':rest) -> let line = tpack rest
                            in runCommand (getCmd line) (getLine line)
        Just line       -> runCommand "eval" $ tpack line
    where getCmd  = fst . cmdLinePair
          getLine = snd . cmdLinePair
          cmdLinePair xs = (head $ words xs, unwords $ tail $ words xs)

          cmdMap = concatMap (\(ts, f) -> map (,f) ts) cmds
          runCommand cmd line = case lookup cmd cmdMap of
                                  Just cmd -> cmd line >> repl
                                  Nothing  -> outputTextLn "Command not found." >> repl

outputText   str = outputStr   (tunpack str)
outputTextLn str = outputStrLn (tunpack str)

-- https://github.com/purescript/purescript/blob/b3e470deb302f8f400bbe140e600eba5c9e2c2b5/app/Command/REPL.hs#L108-L113
paste :: [Text] -> ReplM Text
paste ls = maybe (return . unlines $ reverse ls)
                 (paste . (:ls)) =<< (fmap tpack <$> getInputLine "… ")

instance MonadException m => MonadException (ExceptT e m) where
    controlIO f = ExceptT $ controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap ExceptT . run . runExceptT)
                    in runExceptT <$> f run'

-- ----------------------------------------------------------------------------
-- REPL commands
-- ----------------------------------------------------------------------------

cmds :: [([Text], Text -> ReplM ())]
cmds = [ (["eval"], cmdEval)
       , (["dumpState"], cmdDumpState)
       , (["dumpVanilla"], cmdDumpVanilla)
       , (["dumpDesugared"], cmdDumpDesugared)
       , (["dumpRenamed"], cmdDumpDesugared)
       , (["dumpCore"], cmdDumpCore)
       , (["dumpType"], cmdType True)
       , (["type", "t"], cmdType False)
       , (["loadfile", "lf"], cmdLoadFile)
       , (["paste", "p"], cmdPaste)
       ]

cmdPaste :: Text -> ReplM ()
cmdPaste line = do
    putStrLn "(Press Ctrl-D to end paste mode)"
    txt <- paste []
    modifyHistory (\h -> addHistory (tunpack $ tfilter (/= '\n') txt) h)
    cmdEval txt

cmdDumpState :: Text -> ReplM ()
cmdDumpState line = groom <$> lift get >>= outputStrLn >> repl

cmdDumpVanilla :: Text -> ReplM ()
cmdDumpVanilla line = do
    pstate <- lift $ gets parserState
    (result, pstate') <- liftE $ parseReplWithState pstate line
    outputStrLn . groom $ result

cmdDumpDesugared :: Text -> ReplM ()
cmdDumpDesugared line = do
    (result, pstate') <- replParse line
    case result of
      ReplExpr expr -> groomPut $ desugarExpr expr
      ReplDecl decl -> groomPut $ desugarDecl decl

cmdLoadFile :: Text -> ReplM ()
cmdLoadFile line = do
    result <- parseFile parseModule (tunpack line)
    (ast, pstate') <- liftE result
    let desugared = desugarModule ast
    let (renameresult, rstate') = evalRenamer' (renameModule desugared) emptyGenEnv (initRenamerS True)
    (renamed, genenv') <- liftE renameresult
    typeenv' <- liftE $ inferModule initTypeEnv renamed
    env' <- liftE $ runCore $ coreModule renamed

    setParserState pstate'
    setRenamerState rstate'
    let (Module _ decls) = ast
        (Module _ rdecls) = renamed
    setGenEnv genenv'
    setTypeEnv typeenv'
    env <- lift $ gets env
    setEnv (Map.union env' env)

cmdDumpCore :: Text -> ReplM ()
cmdDumpCore line = do
    env <- lift $ gets env
    (parsed, pstate') <- replParse line
    desugared <- replDesugar parsed
    (renamed, rstate', genenv') <- replRename desugared
    (mscheme, typeenv') <- replInfer renamed
    (mcexpr, env') <- replCore renamed
    case mcexpr of
      Just cexpr -> groomPut cexpr
      Nothing -> outputStrLn ""


cmdType :: Bool -> Text -> ReplM ()
cmdType dump line = do
    (parsed, _) <- replParse line
    desugared <- replDesugar parsed
    (renamed, _, _) <- replRename desugared
    (mscheme, _) <- replInfer renamed
    case mscheme of
      Just scheme -> do
          outputText line
          outputStr " :: "
          outputStrLn $ if dump
                          then show scheme
                          else show $ pretty scheme
      Nothing     -> return ()

cmdEval :: Name -> ReplM ()
cmdEval line = do
    env <- lift $ gets env
    (parsed, pstate') <- replParse line
    desugared <- replDesugar parsed
    (renamed, rstate', genenv') <- replRename desugared
    (mscheme, typeenv') <- replInfer renamed
    (mcexpr, env') <- replCore renamed
    case mcexpr of
      Just expr -> do
          resultio <- liftIO $ runEval env expr
          result <- liftE $ resultio
          groomPut result
      Nothing   -> return ()

    setParserState pstate'
    setRenamerState rstate'
    setGenEnv genenv'
    setTypeEnv typeenv'
    setEnv env'

replParse :: Text -> ReplM (Repl, ParserS)
replParse line = do
    pstate <- lift $ gets parserState
    (parsed, pstate') <- liftE $ parseReplWithState pstate line
    return $ case parsed of
      (ReplExpr _) -> (parsed, pstate')
      (ReplDecl _) -> (parsed, pstate')
      _            -> (ReplExpr $ LitExpr $ StrLit "", pstate')

replDesugar :: Repl -> ReplM Repl
replDesugar r = return $
    case r of
      ReplExpr ast -> ReplExpr $ desugarExpr ast
      ReplDecl ast -> ReplDecl $ desugarDecl ast

replInfer :: Repl -> ReplM (Maybe Scheme, TypeEnv)
replInfer r = do
    typeenv <- lift $ gets typeEnv
    case r of
      ReplExpr ast -> do
          scheme <- liftE $ inferExpr typeenv ast
          return (Just scheme, typeenv)
      ReplDecl ast -> do
          typeenv' <- liftE $ inferDecl typeenv ast
          return (Nothing, typeenv')

replRename :: Repl -> ReplM (Repl, RenamerS, GenEnv)
replRename r = do
    renamers <- lift $ gets renamerState
    genenv <- lift $ gets genEnv
    case r of
      ReplExpr ast -> do
          let desugaredExprAst = desugarExpr ast
              (r, rstate') = evalRenamer' (renameExpr desugaredExprAst) genenv renamers
          renamedast <- liftE r
          return (ReplExpr renamedast, rstate', genenv)
      ReplDecl ast -> do
          let desugaredDeclAst = desugarDecl ast
              (r, rstate') = evalRenamer' (renameDecls [desugaredDeclAst]) genenv renamers
          ([renamedast], _) <- liftE r
          return (ReplDecl renamedast
                 , rstate'
                 , Map.insert (declName ast) (declName renamedast) genenv)

replCore :: Repl -> ReplM (Maybe CExpr, Env)
replCore r = do
    env <- lift $ gets env
    case r of
      ReplExpr ast -> do
          coreast <- liftE $ runCore $ coreExpr ast
          return (Just coreast, env)
      ReplDecl ast -> do
          env' <- liftE $ runCore $ coreDecl ast
          return (Nothing, Map.union env' env)

groomPut :: (MonadIO m, Show a) => a -> InputT m ()
groomPut = outputStrLn . groom

-- ----------------------------------------------------------------------------
-- REPL cmd helpers
-- ----------------------------------------------------------------------------

setParserState :: ParserS -> ReplM ()
setParserState pstate = lift (modify (\s -> s { parserState = pstate }))

setRenamerState :: RenamerS -> ReplM ()
setRenamerState rstate = lift (modify (\s -> s { renamerState = rstate }))

setGenEnv :: GenEnv -> ReplM ()
setGenEnv genenv = lift (modify (\s -> s { genEnv = genenv }))

insertGenEnv :: Name -> GenName -> ReplM ()
insertGenEnv name gname = do
    genenv <- lift $ gets genEnv
    setGenEnv $ Map.insert name gname genenv

setTypeEnv :: TypeEnv -> ReplM ()
setTypeEnv typeenv = lift (modify (\s -> s { typeEnv = typeenv }))

setEnv :: Env -> ReplM ()
setEnv env = lift (modify (\s -> s { env = env }))

insertEnv :: Name -> CExpr -> ReplM ()
insertEnv name expr = do
    env <- lift $ gets env
    setEnv $ Map.insert name expr env

extendEnv :: Env -> ReplM ()
extendEnv nenv = do
    env <- lift $ gets env
    setEnv $ Map.union nenv env

liftE :: Either TechneErr a -> ReplM a
liftE (Right x) = return x
liftE (Left y) = do
    case y of
      ParserErr err -> outputStrLn (errorBundlePretty err)
      x -> outputStrLn $ groom x
    s <- lift get
    lift $ throwError (y, s)
