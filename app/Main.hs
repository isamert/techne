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

type ReplM = InputT (StateT ReplS IO)
type ReplM2 = InputT (StateT ReplS (ExceptT (TechneErr, ReplS) IO))

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
      Left a -> tpack $ errorBundlePretty a

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

replSettings :: MonadIO m => Settings m
replSettings = Settings { historyFile = Just histfile
                        , complete = completeWordWithPrev Nothing " \t" replComplete
                        , autoAddHistory = True
                        }
    where histfile = unsafePerformIO getHomeDirectory ++ "/.technehist" -- Am I a monster?
          -- TODO: I should probably use XDG cache directory

-- TODO: modularize
replComplete :: MonadIO m => String -> String -> m [Completion]
replComplete left_ word = do
    let left = reverse left_
    case left of
       str
         | ":load-file " `isPrefixOf` left -> listFiles word
       (':':_) -> return []
       "" -> case word of
               (':':_) -> return $ searchCmds word
               _ -> return [] -- TODO: code completion
    where searchCmds word = map simpleCompletion
                              $ filter (word `isPrefixOf`) (map ((":" ++) . tunpack . head . fst) cmds)

repl :: ReplM2 ()
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

-- ----------------------------------------------------------------------------
-- REPL commands
-- ----------------------------------------------------------------------------

cmds :: [([Text], Text -> ReplM2 ())]
cmds = [ (["eval"], cmdEval)
       , (["dumpVanilla"], cmdDumpVanilla)
       , (["dumpDesugared"], cmdDumpDesugared)
       , (["dumpRenamed"], cmdDumpDesugared)
       , (["dumpType"], cmdType True)
       , (["type", "t"], cmdType False)
       ]

cmdDumpVanilla :: Text -> ReplM2 ()
cmdDumpVanilla line = do
    pstate <- lift $ gets parserState
    (result, pstate') <- liftE $ parseReplWithState pstate line
    outputStrLn . groom $ result

cmdDumpDesugared :: Text -> ReplM2 ()
cmdDumpDesugared line = do
    (result, pstate') <- replParse line
    case result of
      ReplExpr expr -> groomPut $ desugarExpr expr
      ReplDecl decl -> groomPut $ desugarDecl decl


cmdType :: Bool -> Text -> ReplM2 ()
cmdType dump line = do
    (parsed, _) <- replParse line
    desugared <- replDesugar parsed
    (renamed, _, _) <- replRename desugared
    (mscheme, _) <- replInfer desugared
    case mscheme of
      Just scheme -> do
          outputText line
          outputStr " :: "
          outputStrLn $ if dump
                          then show scheme
                          else show $ pretty scheme
      Nothing     -> return ()

cmdEval :: Name -> ReplM2 ()
cmdEval line = do
    env <- lift $ gets env
    (parsed, pstate') <- replParse line
    desugared <- replDesugar parsed
    (renamed, rstate', genenv') <- replRename desugared
    (mscheme, typeenv') <- replInfer desugared
    (mcexpr, env') <- replCore renamed
    case mcexpr of
      Just expr -> do
          result <- liftE $ runEval env expr
          groomPut result
      Nothing   -> return ()

    setParserState pstate'
    setRenamerState rstate'
    setGenEnv genenv'
    setTypeEnv typeenv'
    setEnv env'

replParse :: Text -> ReplM2 (Repl, ParserS)
replParse line = do
    pstate <- lift $ gets parserState
    (parsed, pstate') <- liftE $ parseReplWithState pstate line
    return $ case parsed of
      (ReplExpr _) -> (parsed, pstate')
      (ReplDecl _) -> (parsed, pstate')
      _            -> (ReplExpr $ LitExpr $ StrLit "", pstate')

replDesugar :: Repl -> ReplM2 Repl
replDesugar r = return $
    case r of
      ReplExpr ast -> ReplExpr $ desugarExpr ast
      ReplDecl ast -> ReplDecl $ desugarDecl ast

replInfer :: Repl -> ReplM2 (Maybe Scheme, TypeEnv)
replInfer r = do
    typeenv <- lift $ gets typeEnv
    case r of
      ReplExpr ast -> do
          scheme <- liftE $ inferExpr typeenv ast
          return (Just scheme, typeenv)
      ReplDecl ast -> do
          typeenv' <- liftE $ inferDecl typeenv ast
          return (Nothing, typeenv')

replRename :: Repl -> ReplM2 (Repl, RenamerS, GenEnv)
replRename r = do
    renamers <- lift $ gets renamerState
    genenv <- lift $ gets genEnv
    case r of
      ReplExpr ast -> do
          let desugaredExprAst = desugarExpr ast
              (r, rstate') = evalRenamer' renameExpr desugaredExprAst genenv renamers
          renamedast <- liftE r
          return $ (ReplExpr renamedast, rstate', genenv)
      ReplDecl ast -> do
          let desugaredDeclAst = desugarDecl ast
              (r, rstate') = evalRenamer' renameDecls [desugaredDeclAst] genenv renamers
          [renamedast] <- liftE r
          return $ (ReplDecl renamedast
                   , rstate'
                   , Map.insert (declName ast) (declName renamedast) genenv)

replCore :: Repl -> ReplM2 (Maybe CExpr, Env)
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

setParserState :: ParserS -> ReplM2 ()
setParserState pstate = lift (modify (\s -> s { parserState = pstate }))

setRenamerState :: RenamerS -> ReplM2 ()
setRenamerState rstate = lift (modify (\s -> s { renamerState = rstate }))

setGenEnv :: GenEnv -> ReplM2 ()
setGenEnv genenv = lift (modify (\s -> s { genEnv = genenv }))

insertGenEnv :: Name -> GenName -> ReplM2 ()
insertGenEnv name gname = do
    genenv <- lift $ gets genEnv
    setGenEnv $ Map.insert name gname genenv

setTypeEnv :: TypeEnv -> ReplM2 ()
setTypeEnv typeenv = lift (modify (\s -> s { typeEnv = typeenv }))

setEnv :: Env -> ReplM2 ()
setEnv env = lift (modify (\s -> s { env = env }))

insertEnv :: Name -> CExpr -> ReplM2 ()
insertEnv name expr = do
    env <- lift $ gets env
    setEnv $ Map.insert name expr env

extendEnv :: Env -> ReplM2 ()
extendEnv nenv = do
    env <- lift $ gets env
    setEnv $ Map.union nenv env

liftE :: Either TechneErr a -> ReplM2 a
liftE (Right x) = return x
liftE (Left y) = do
    case y of
      ParserErr err -> outputStrLn (errorBundlePretty err)
      x -> outputStrLn $ groom x
    s <- lift get
    lift $ throwError (y, s)

instance MonadException m => MonadException (ExceptT e m) where
    controlIO f = ExceptT $ controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap ExceptT . run . runExceptT)
                    in fmap runExceptT $ f run'
