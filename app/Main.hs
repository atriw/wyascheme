module Main where

import Control.Monad (unless, (>=>))
import System.IO (hFlush, stdout)
import Control.Monad.Except (MonadIO (liftIO))

import Lisp

main :: IO ()
main = primitiveBindings >>= run

run :: Env -> IO ()
run env = do
  input <- readPrompt "Lisp>>> "
  unless (input == "quit")
    (evalAndPrint env input >> run env)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ show <$> (liftThrows (readExpr expr) >>= eval env)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn
