{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad (unless, (>=>))
import Control.Monad.Except (MonadIO (liftIO))
import Lisp
import Options.Applicative as Opt
import System.IO (hFlush, hPutStrLn, stderr, stdout)

data Params = Params
  { -- |
    file :: Maybe String,
    -- |
    loadPaths :: [String]
  }

mkParams :: Opt.Parser Params
mkParams =
  Params
    <$> optional (strOption (metavar "FILE" <> long "file" <> short 'f' <> help "File to load"))
    <*> many (strOption (metavar "PATH" <> long "load-paths" <> short 'l' <> help "Load paths"))

main :: IO ()
main = execParser opts >>= run
  where
    opts =
      info
        (mkParams <**> helper)
        (fullDesc <> progDesc "Scheme interpreter")

run :: Params -> IO ()
run Params {file=Nothing, ..} = bindings >>= interactive (Config loadPaths)
run Params {file=Just filename, ..} = do
  env <- bindings
  result <- evalString (Config loadPaths) env ("(load \"" ++ filename ++ "\")")
  hPutStrLn stderr result

interactive :: Config -> Env -> IO ()
interactive config env = do
  input <- readPrompt "Lisp>>> "
  unless
    (input == "quit")
    (evalAndPrint env input >> interactive config env)
  where
    flushStr :: String -> IO ()
    flushStr str = putStr str >> hFlush stdout
    readPrompt :: String -> IO String
    readPrompt prompt = flushStr prompt >> getLine
    evalAndPrint :: Env -> String -> IO ()
    evalAndPrint env expr = evalString config env expr >>= putStrLn

evalString :: Config -> Env -> String -> IO String
evalString config env expr = runEvalMPrintWith config (liftThrows (readExpr expr) >>= eval env)
