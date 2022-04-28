module Main where
import System.Environment (getArgs)

main :: IO ()
main = do
  putStrLn "Your name plz: "
  name <- getLine
  putStrLn $ "Hello, " ++ name
