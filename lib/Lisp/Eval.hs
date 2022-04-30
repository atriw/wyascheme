-- |

module Lisp.Eval where

import Lisp.Parse
import Lisp.Types
import Control.Monad.Except (MonadError(throwError))

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Atom _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = traverse eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized bad special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive fucntion args" func)
  ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op val@[_] = throwError $ NumArgs 2 val
numericBinop op params = Number . foldl1 op <$> traverse unpackNum params

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum val@(String n) = let parsed = reads n in
  if null parsed
  then throwError $ TypeMismatch "number" val
  else return $ fst $ head parsed
unpackNum notNum = throwError $ TypeMismatch "number" notNum

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [Atom _] = return $ Bool True
isSymbol [_] = return $ Bool False
isSymbol val = throwError $ NumArgs 1 val

isString :: [LispVal] -> ThrowsError LispVal
isString [String _] = return $ Bool True
isString [_] = return $ Bool False
isString val = throwError $ NumArgs 1 val

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [Number _] = return $ Bool True
isNumber [_] = return $ Bool False
isNumber val = throwError $ NumArgs 1 val

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [String s] = return $ Atom s
stringToSymbol [val] = throwError $ TypeMismatch "string" val
stringToSymbol val = throwError $ NumArgs 1 val

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [Atom s] = return $ String s
symbolToString [val] = throwError $ TypeMismatch "symbol" val
symbolToString val = throwError $ NumArgs 1 val
