module Main where

import Control.Monad (unless, (>=>))
import Lisp.Eval
import Lisp.Parse
import Lisp.Types
import System.IO (hFlush, stdout)

main :: IO ()
main = run

run :: IO ()
run = do
  input <- readPrompt "Lisp>>> "
  unless (input == "quit")
    (evalAndPrint input >> run)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractVal (trapError $ show <$> (readExpr expr >>= eval))

evalAndPrint :: String -> IO ()
evalAndPrint = evalString >=> putStrLn
