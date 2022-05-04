{-# LANGUAGE ExistentialQuantification #-}
-- |

module Lisp.Primitive where

import Control.Applicative (Applicative (liftA2))
import Control.Monad.Except (MonadError (catchError, throwError), MonadIO (liftIO), unless)
import System.IO (IOMode(ReadMode, WriteMode), openFile, hClose, stdin, hGetLine, stdout, hPrint)
import Lisp.Types
import Lisp.Parse
import Lisp.Eval

bindings :: IO Env
bindings = nullEnv >>= flip bindVars ((fmap . fmap) PrimitiveFunc primitives ++ (fmap . fmap) IOFunc ioPrimitives)

ioPrimitives :: [(String, [LispVal] -> EvalM LispVal)]
ioPrimitives =
  [ ("apply", applyProc),
    ("open-input-file", makePort ReadMode),
    ("open-output-file", makePort WriteMode),
    ("close-input-port", closePort),
    ("close-output-port", closePort),
    ("read", readProc),
    ("write", writeProc),
    ("read-contents", readContents),
    ("read-all", readAll),
    ("display", display)
  ]

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop (+)),
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
    ("symbol->string", symbolToString),
    ("=", numBoolBinop (==)),
    ("/=", numBoolBinop (/=)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("<=", numBoolBinop (<=)),
    (">=", numBoolBinop (>=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string=?", strBoolBinop (==)),
    ("string/=?", strBoolBinop (/=)),
    ("string<?", strBoolBinop (<)),
    ("string>?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?", eqv),
    ("equal?", equal)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op val@[_] = throwError $ NumArgs 2 val
numericBinop op params = Number . foldl1 op <$> traverse unpackNum params

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = do
  unless (length args == 2) (throwError $ NumArgs 2 args)
  left <- unpacker $ head args
  right <- unpacker $ args !! 1
  return $ Bool $ op left right

numBoolBinop = boolBinop unpackNum

strBoolBinop = boolBinop unpackStr

boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum val@(String n) =
  let parsed = reads n
   in if null parsed
        then throwError $ TypeMismatch "number" val
        else return $ fst $ head parsed
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notStr = throwError $ TypeMismatch "string" notStr

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

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

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgs = throwError $ NumArgs 1 badArgs

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgs = throwError $ NumArgs 1 badArgs

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x : xs)
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgs = throwError $ NumArgs 2 badArgs

eqList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> [LispVal] -> Bool
eqList f a b =
  length a == length b
    && all eqvPair (zip a b)
  where
    eqvPair (a, b) = case f [a, b] of
      Right (Bool val) -> val
      _ -> False

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Atom a, Atom b] = return $ Bool $ a == b
eqv [Number a, Number b] = return $ Bool $ a == b
eqv [String a, String b] = return $ Bool $ a == b
eqv [Bool a, Bool b] = return $ Bool $ a == b
eqv [Char a, Char b] = return $ Bool $ a == b
eqv [Float a, Float b] = return $ Bool $ a == b
eqv [List a, List b] = return $ Bool $ eqList eqv a b
eqv [DottedList a alast, DottedList b blast] = eqv [List (a ++ [alast]), List (b ++ [blast])]
eqv [_, _] = return $ Bool False
eqv badArgs = throwError $ NumArgs 2 badArgs

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals a b (AnyUnpacker unpacker) =
  liftA2 (==) (unpacker a) (unpacker b)
    `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [List a, List b] = return $ Bool $ eqList equal a b
equal [DottedList a alast, DottedList b blast] = equal [List (a ++ [alast]), List (b ++ [blast])]
equal [a, b] = do
  primitiveEquals <- or <$> traverse (unpackEquals a b) [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [a, b]
  return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgs = throwError $ NumArgs 2 badArgs

applyProc :: [LispVal] -> EvalM LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args
applyProc _ = undefined

makePort :: IOMode -> [LispVal] -> EvalM LispVal
makePort mode [String filename] = Port <$> liftIO (openFile filename mode)
makePort _ [badArg] = throwError $ TypeMismatch "filepath" badArg
makePort _ badArgs = throwError $ NumArgs 1 badArgs

closePort :: [LispVal] -> EvalM LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> EvalM LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr
readProc [badArg] = throwError $ TypeMismatch "port" badArg
readProc badArgs = throwError $ NumArgs 1 badArgs

writeProc :: [LispVal] -> EvalM LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO (hPrint port obj) >> return (Bool True)
writeProc badArgs = throwError $ NumArgs 2 badArgs

readContents :: [LispVal] -> EvalM LispVal
readContents [String filename] = String <$> liftIO (readFile filename)
readContents [badArg] = throwError $ TypeMismatch "filepath" badArg
readContents badArgs = throwError $ NumArgs 1 badArgs

readAll :: [LispVal] -> EvalM LispVal
readAll [String filename] = List <$> load filename
readAll [badArg] = throwError $ TypeMismatch "filepath" badArg
readAll badArgs = throwError $ NumArgs 1 badArgs

display :: [LispVal] -> EvalM LispVal
display [String s] = writeProc [Atom s]
display [Char s] = writeProc [Atom [s]]
display v = writeProc v
