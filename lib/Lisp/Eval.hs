{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- |
module Lisp.Eval (eval, apply, load) where

import Control.Applicative (Applicative (liftA2))
import Control.Monad.Except (MonadError (throwError), MonadIO (liftIO), join, when)
import Data.Maybe (isNothing)
import Lisp.Parse
import Lisp.Types

eval :: Env -> LispVal -> EvalM LispVal
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
eval env (List (Atom "case" : key : clauses)) = do
  kval <- eval env key
  eval env
    =<< liftThrows (foldr (clauseToIf kval) (return $ Bool False) clauses)
  where
    clauseToIf :: LispVal -> LispVal -> ThrowsError LispVal -> ThrowsError LispVal
    clauseToIf _ _ err@(Left _) = err
    clauseToIf _ (List [Atom "else", conseq]) _ = return conseq
    clauseToIf kval (List [datum, conseq]) (Right alt) = do
      pred <- datumToPred kval datum
      return $ List [Atom "if", pred, conseq, alt]
    clauseToIf _ clause _ = throwError $ BadSpecialForm "Bad case clause" clause
    datumToPred :: LispVal -> LispVal -> ThrowsError LispVal
    datumToPred kval (List vals) = return $ List (Atom "cond" : (datumToClause kval <$> vals))
    datumToPred _ datum = throwError $ BadSpecialForm "Bad case datum" datum
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
eval env (List [Atom "load", String filename]) = last <$> (load filename >>= traverse (eval env))
eval env (List (func : args)) = join (liftA2 apply (eval env func) (traverse (eval env) args))
eval _ badForm = throwError $ BadSpecialForm "Unrecognized bad special form" badForm

apply :: LispVal -> [LispVal] -> EvalM LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
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

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> EvalM LispVal
makeFunc vararg closure vparams body = let params = show <$> vparams in return Func {..}

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> EvalM LispVal
makeNormalFunc = makeFunc Nothing

makeVararg :: LispVal -> Env -> [LispVal] -> [LispVal] -> EvalM LispVal
makeVararg = makeFunc . Just . show

load :: String -> EvalM [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList
