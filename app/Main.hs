module Main where

import Parse

main :: IO ()
main = run

run :: IO ()
run = do
  putStrLn "input:"
  input <- getLine
  case readExpr input of
    Left err -> print err
    Right val -> print val >> run
