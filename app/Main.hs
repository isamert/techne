{-# LANGUAGE LambdaCase #-}
module Main where

import TechnePrelude
import Frontend.AST
import Frontend.Desugar
import Frontend.Parser

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
--
-- Commandline options
--
data Options = Options
  { interactive :: Bool
  , output :: Text
  , input :: Maybe String }

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

--
-- Main
--

-- | Get commandline options and display information if necessary.
main :: IO ()
main = runOptions =<< execParser opts
  where
    opts = info (options <**> helper)
      (fullDesc
     <> progDesc "Compile files or run an interactive shell."
     <> header "Techne")

-- | Do stuff depending on commandline options.
runOptions :: Options -> IO ()
runOptions (Options _ output (Just input)) = runInputT defaultSettings $ do
    result <- parseFile parseModule input
    putStrLn $ case result of
      Right a -> tgroom a
      Left a -> tpack $ errorBundlePretty a

runOptions (Options True _ _) = evalStateT (runInputT replSettings repl) initReplS
runOptions (Options i outf inf) = putStrLn $ tgroom i <> tgroom outf <> tgroom inf

-- ----------------------------------------------------------------------------
-- REPL
-- ----------------------------------------------------------------------------
data ReplS = ReplS { parserState :: ParserS
                   , replsCounter :: Int
                   } deriving (Show, Eq)

type ReplM = InputT (StateT ReplS IO)

initReplS :: ReplS
initReplS = ReplS { parserState = initParserS
                  , replsCounter = 0
                  }

replSettings :: MonadIO m => Settings m
replSettings = Settings { historyFile = Just histfile
                        , complete = completeWordWithPrev Nothing " \t" $ replComplete
                        , autoAddHistory = True
                        }
    where histfile = unsafePerformIO getHomeDirectory ++ "/.technehist" -- Am I a monster?
          -- TODO: I should probably use XDG cache directory

cmds :: [(String, String)]
cmds = [("quit", "q"), ("load-module", "lm"), ("load-file", "lf"), ("dump-state", "ds")]

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
                              $ filter (word `isPrefixOf`) (map ((":" ++) . fst) cmds)

repl :: ReplM ()
repl = do
    line <- getInputLine "> "
    pstate <- lift $ gets parserState
    case sstrip <$> line of
        Nothing      -> repl
        Just ""      -> repl
        Just ":q"    -> return ()
        Just ":quit" -> return ()
        Just ":dump-state" -> groom <$> (lift $ gets parserState) >>= outputStrLn >> repl
        Just str
          | ":load-file" `isPrefixOf` str -> do
              let files = drop 1 $ swords str
              modules <- mapM (parseFile parseModule) files
              forM modules (\case
                Right (x, pstate) -> printAndUpdateState x pstate
                Left y            -> printErrBundle y) >> return ()
        Just input -> case parseReplWithState pstate (tpack input) of
              Right (x, pstate) -> printAndUpdateState x pstate
              Left y -> printErrBundle y
    where printErrBundle err = outputStrLn (errorBundlePretty err) >> repl
          printAndUpdateState x pstate = outputStrLn (groom x)
            >> lift (modify (\s -> s { parserState = pstate})) >> repl

