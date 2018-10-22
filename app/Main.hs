{-# LANGUAGE LambdaCase #-}

module Main where

import TechnePrelude

import Options.Applicative
import Data.Semigroup ((<>))
import System.Console.Haskeline

--
-- Commandline options
--
data Options = Options
  { interactive :: Bool
  , output :: Text }

optInteractive = switch $
    long "interactive"
    <> short 'i'
    <> help "Start an interactive Techne shell."

optOutput = strOption $
    long "output"
    <> metavar "<file>"
    <> help "Place the output into <file>"

options :: Parser Options
options = Options
    <$> optInteractive
    <*> optOutput

--
-- Main
--

-- | Get commandline options and display information if necessary.
main :: IO ()
main = runOptions =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Compile files or run an interactive shell."
     <> header "Techne" )

-- | Do stuff depending on commandline options.
runOptions :: Options -> IO ()
runOptions (Options True _) = runInputT defaultSettings repl
runOptions _ = return ()


repl :: InputT IO ()
repl = getInputLine "> " >>= \case
    Nothing    -> repl
    Just ":q"  -> return ()
    Just input -> do { outputStrLn $ "Input was: " ++ input; repl }



