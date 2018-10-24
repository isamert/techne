{-# LANGUAGE LambdaCase #-}

module Main where

import TechnePrelude
import Frontend.AST
import Frontend.Lexer
import qualified Frontend.Parser as P

import Text.Megaparsec
import Text.Megaparsec.Error

import Options.Applicative
import Data.Semigroup ((<>))
import System.Console.Haskeline

--
-- Commandline options
--
data Options = Options
  { interactive :: Bool
  , output :: Text
  , input :: Maybe Text }

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

optInput :: Parser (Maybe Text)
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
runOptions (Options True _ _) = runInputT defaultSettings $ repl initTState
runOptions _ = return ()


repl :: TState -> InputT IO ()
repl state = getInputLine "> " >>= \case
    Nothing    -> repl state
    Just ":q"  -> return ()
    Just input -> case pp input of
          Right (x, s) -> outputStrLn (unpack x) <* repl s
          Left y -> outputStrLn (errorBundlePretty y) <* repl state
    where pp str = parse (runStateT P.repl state) "" (pack str)
