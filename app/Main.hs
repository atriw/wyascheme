module Main where

import Control.Monad (unless, (>=>))
import Lisp.Eval
import Lisp.Parse
import Lisp.Types
import System.IO (hFlush, stdout)
import Control.Monad.Except (MonadIO (liftIO))

main :: IO ()
main = run

run :: IO ()
run = do
  input <- readPrompt "Lisp>>> "
  unless (input == "quit")
    (nullEnv >>= flip evalAndPrint input >> run)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ show <$> (liftThrows (readExpr expr) >>= eval env)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn
