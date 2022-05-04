module Main where

import Options.Applicative as Opt
import Control.Monad (unless, (>=>))
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import Control.Monad.Except (MonadIO (liftIO))

import Lisp

data Params = Params (Maybe String)

mkParams :: Opt.Parser Params
mkParams = Params <$>
  optional (strOption (metavar "FILE" <> long "file" <> short 'f' <> help "File to load"))

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (mkParams <**> helper)
                (fullDesc <> progDesc "Scheme interpreter")

run :: Params -> IO ()
run (Params Nothing) = primitiveBindings >>= interactive
run (Params (Just filename)) = primitiveBindings >>=
  flip evalString ("(load \"" ++ filename ++ "\")") >>= hPutStrLn stderr

interactive :: Env -> IO ()
interactive env = do
  input <- readPrompt "Lisp>>> "
  unless (input == "quit")
    (evalAndPrint env input >> interactive env)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ show <$> (liftThrows (readExpr expr) >>= eval env)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn
