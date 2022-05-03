{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- |
module Lisp.Eval(eval, primitiveBindings) where

import Control.Applicative (Applicative (liftA2))
import Control.Monad.Except (MonadError (catchError, throwError), MonadIO (liftIO), unless, when, join)
import Data.Maybe (isNothing)
import Lisp.Parse
import Lisp.Types

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    (Bool False) -> eval env alt
    _ -> eval env conseq
eval env (List (Atom "cond" : clauses)) =
  eval env
    =<< liftThrows (foldr clauseToIf (return $ Bool False) clauses)
  where
    clauseToIf :: LispVal -> ThrowsError LispVal -> ThrowsError LispVal
    clauseToIf _ err@(Left _) = err
    clauseToIf (List [Atom "else", conseq]) _ = return conseq
    clauseToIf (List [pred, conseq]) (Right alt) = return $ List [Atom "if", pred, conseq, alt]
    clauseToIf clause _ = throwError $ BadSpecialForm "Bad cond clause" clause
eval env (List (Atom "case" : key : clauses)) =
  eval env
    =<< foldr clauseToIf (return $ Bool False) clauses
  where
    clauseToIf :: LispVal -> IOThrowsError LispVal -> IOThrowsError LispVal
    clauseToIf clause acc = do
      alt <- acc
      case clause of
        (List [Atom "else", conseq]) -> return conseq
        (List [datum, conseq]) -> do
          pred <- datumToPred datum
          return $ List [Atom "if", pred, conseq, alt]
        _ -> throwError $ BadSpecialForm "Bad case clause" clause
    datumToPred :: LispVal -> IOThrowsError LispVal
    datumToPred (List vals) = do
      kval <- eval env key
      return $ List (Atom "cond" : (datumToClause kval <$> vals))
    datumToPred datum = throwError $ BadSpecialForm "Bad case datum" datum
    datumToClause :: LispVal -> LispVal -> LispVal
    datumToClause kval v = List [List [Atom "eqv?", kval, v], Bool True]
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) vararg : body)) =
  makeVararg vararg env params body >>= defineVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) = makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params vararg : body)) = makeVararg vararg env params body
eval env (List (Atom "lambda" : vararg@(Atom _) : body)) = makeVararg vararg env [] body
eval env (List (func : args)) = do
  func <- eval env func
  args <- traverse (eval env) args
  apply func args
eval _ badForm = throwError $ BadSpecialForm "Unrecognized bad special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply Func {..} args = do
  when
    ((length params /= length args) && isNothing vararg)
    (throwError $ NumArgs (length params) args)
  env <- liftIO $ bindVars closure $ zip params args
  env <- bindVarArgs env vararg
  evalBody env
  where
    evalBody env = last <$> traverse (eval env) body
    remainingArgs = drop (length params) args
    bindVarArgs env = maybe (return env) (liftIO . bindVars env . (: []) . (,List remainingArgs))
apply _ _ = undefined

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc vararg closure vparams body = let params = show <$> vparams in return Func {..}

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVararg :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVararg = makeFunc . Just . show

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

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars ((fmap . fmap) PrimitiveFunc primitives)

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
