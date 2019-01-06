{-# LANGUAGE FlexibleContexts         #-}
module Main where

import TechnePrelude
import Syntax
import Desugar
import Parser
import Infer
import Pretty
import Core
import Renamer
import Eval


import Text.Megaparsec
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

runOptions (Options True _ _) = runRepl
runOptions (Options i outf inf) = putStrLn $ tgroom i <> tgroom outf <> tgroom inf

-- ----------------------------------------------------------------------------
-- REPL
-- ----------------------------------------------------------------------------

runRepl :: IO ()
runRepl = evalStateT (runInputT replSettings repl) initReplS

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

-- ----------------------------------------------------------------------------
-- REPL commands
-- ----------------------------------------------------------------------------

cmds :: [([Text], Text -> ReplM ())]
cmds = [ (["quit", "q"]          , \_ -> return () ) -- FIXME: system.exit
       , (["type", "t"]          , cmdType False)
       , (["dump-type"]          , cmdType True)
       , (["dump-state", "ds" ]  , cmdDumpState)
       , (["dump-ast-vanilla"]   , cmdDumpAst selVanilla)
       , (["dump-ast-desugared"] , cmdDumpAst selDesugared)
       , (["dump-ast-renamed"]   , cmdDumpAst selRenamed)
       , (["dump-ast-core"]      , cmdDumpAst selCore)
       , (["dump-file-ast"]      , cmdDumpAstFile)
       , (["load-file", "lf" ]   , cmdLoadFile)
       , (["eval"]               , cmdDefault)
       ]

cmdDumpState :: Text -> ReplM ()
cmdDumpState _ = groom <$> lift get >>= outputStrLn >> repl

cmdLoadFile line = do
    let files = swords $ tunpack line
    modules <- mapM (parseFile parseModule) files
    forM_ modules (\case
          Right (x, pstate) ->
              case runRenamer' renameModule (desugarModule x) of
                Right mod ->
                    let coreAst = coreModule mod in
                    outputStrLn $ groom coreAst
                Left y -> outputStrLn $ show y
          Left y            -> printErrBundle y)
{-
cmdDefault line = do
    pstate <- lift $ gets parserState
    case parseReplWithState pstate line of
      Right (x, pstate) -> case x of
        ReplExpr expr -> printAndUpdateState (desugarExpr expr) pstate
        ReplDecl decl -> do
            outputStrLn $ groom (desugarDecl decl)
            inferRepl inferDecl (desugarDecl decl)
        x -> printAndUpdateState x pstate
      Left y -> printErrBundle y
-}

cmdDefault :: Text -> ReplM ()
cmdDefault line = pdr line dumpExpr dumpDecl
    where dumpDecl tuple = do
            env <- lift $ gets env
            extendEnv (selCore tuple)

          dumpExpr tuple = do
            env <- lift $ gets env
            outputStrLn . groom $ eval env (selCore tuple)


cmdDumpAstFile = cmdLoadFile

--cmdDumpAst :: a -> Text -> ReplM ()
cmdDumpAst sel line = pdr line dumpExpr dumpDecl
    where dumpExpr tuple = outputStrLn (groom $ sel tuple)
          dumpDecl = undefined

cmdType :: Bool -> Text -> ReplM ()
cmdType dump line = do
    pstate  <- lift $ gets parserState
    typeenv <- lift $ gets typeEnv
    case parseExprWithState pstate line of
      Right (expr, pstate) -> if dump
                                 then (outputTextLn . tshow ) (inferExpr typeenv $ desugarExpr expr)
                                 else (outputTextLn . tshow . fmap pretty) (inferExpr typeenv $ desugarExpr expr)
      Left y               -> printErrBundle y

-- ----------------------------------------------------------------------------
-- REPL cmd helpers
-- ----------------------------------------------------------------------------

printErrBundle :: ParserE -> ReplM ()
printErrBundle err = outputStrLn (errorBundlePretty err)

printAndUpdateState :: Show a => a -> ParserS -> ReplM ()
printAndUpdateState x pstate = outputStrLn (groom x) >> Main.updateParserState pstate

updateParserState :: ParserS -> ReplM ()
updateParserState pstate = lift (modify (\s -> s { parserState = pstate }))

updateRenamerState :: RenamerS -> ReplM ()
updateRenamerState rstate = lift (modify (\s -> s { renamerState = rstate }))

updateGenEnv :: GenEnv -> ReplM ()
updateGenEnv genenv = lift (modify (\s -> s { genEnv = genenv }))

insertGenEnv :: Name -> GenName -> ReplM ()
insertGenEnv name gname = do
    genenv <- lift $ gets genEnv
    updateGenEnv $ Map.insert name gname genenv

updateTypeEnv :: TypeEnv -> ReplM ()
updateTypeEnv typeenv = lift (modify (\s -> s { typeEnv = typeenv }))

updateEnv :: Env -> ReplM ()
updateEnv env = lift (modify (\s -> s { env = env }))

insertEnv :: Name -> CExpr -> ReplM ()
insertEnv name expr = do
    env <- lift $ gets env
    updateEnv $ Map.insert name expr env

extendEnv :: Env -> ReplM ()
extendEnv nenv = do
    env <- lift $ gets env
    updateEnv $ Map.union nenv env


-- FIXME: I hate myself
pdr line fnExpr fnDecl = do
    pstate <- lift $ gets parserState
    renamers <- lift $ gets renamerState
    genenv <- lift $ gets genEnv
    typeenv <- lift $ gets typeEnv
    case parseReplWithState pstate line of
      Right (x, pstate) ->
          case x of
            ReplExpr exprAst -> do
                let desugaredExprAst = desugarExpr exprAst
                let (renamerResult, rstate) = evalRenamer' renameExpr
                                                           desugaredExprAst
                                                           genenv
                                                           renamers
                case renamerResult of
                  Right renamedExprAst ->
                      case inferExpr typeenv renamedExprAst of
                        Right scheme -> do
                          let coreAst = coreExpr renamedExprAst
                          fnExpr (exprAst, desugaredExprAst, renamedExprAst, coreAst)

                          Main.updateParserState pstate
                          updateRenamerState rstate

                        Left err -> outputStrLn $ show err
                  Left y -> outputStrLn $ show y
            ReplDecl declAst ->
                let desugaredDeclAst = desugarDecl declAst
                    (renamerResult, rstate) = evalRenamer' renameDecls
                                                           [desugaredDeclAst]
                                                           genenv
                                                           renamers in
                case renamerResult of
                  Right [renamedDeclAst] ->
                      case inferDecl typeenv renamedDeclAst of
                        Right scheme -> do
                          let coreAst = coreDecl renamedDeclAst
                          fnDecl (declAst, desugaredDeclAst, renamedDeclAst, coreAst)
                          updateTypeEnv scheme

                          Main.updateParserState pstate
                          updateRenamerState rstate
                          insertGenEnv (declName declAst) (declName renamedDeclAst)

                        Left err -> outputStrLn $ show err
                  Left y -> outputStrLn $ show y
            x -> do
                outputStrLn $ groom x
                Main.updateParserState pstate
      Left y -> printErrBundle y


data Sel = Vanilla | Desugared | Renamed | Core deriving (Show, Eq, Ord)
selVanilla   (a,_,_,_) = a
selDesugared (_,a,_,_) = a
selRenamed   (_,_,a,_) = a
selCore      (_,_,_,a) = a
