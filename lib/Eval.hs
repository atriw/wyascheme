-- |

module Eval where

import Parse

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval val = val

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", isSymbol),
              ("string?", isString),
              ("number?", isNumber),
              ("string->symbol", stringToSymbol),
              ("symbol->string", symbolToString)]

numericBinop :: (Integer -> Integer -> Integer) -> ([LispVal] -> LispVal)
numericBinop op = Number . foldl1 op . map unpackNum

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n in
  if null parsed then 0 else fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

isSymbol :: [LispVal] -> LispVal
isSymbol [Atom _] = Bool True
isSymbol _ = Bool False

isString :: [LispVal] -> LispVal
isString [String _] = Bool True
isString _ = Bool False

isNumber :: [LispVal] -> LispVal
isNumber [Number _] = Bool True
isNumber _ = Bool False

stringToSymbol :: [LispVal] -> LispVal
stringToSymbol [String s] = Atom s
stringToSymbol _ = Bool False

symbolToString :: [LispVal] -> LispVal
symbolToString [Atom s] = String s
symbolToString _ = Bool False
