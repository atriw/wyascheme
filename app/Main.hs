module Main where

import Control.Monad (unless)
import Lisp.Eval
import Lisp.Parse
import Lisp.Types

main :: IO ()
main = run

run :: IO ()
run = do
  putStrLn "input:"
  input <- getLine
  unless (input == "quit")
    (putStrLn (extractVal $ trapError $ show <$> (readExpr input >>= eval)) >> run)
