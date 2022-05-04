-- |
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Lisp.Types where

import Control.Monad.Except
import Text.ParserCombinators.Parsec (ParseError)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import System.IO (Handle)
import Control.Monad.Reader (ReaderT (runReaderT))

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Char Char
  | Float Float
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func {params :: [String], vararg :: Maybe String, body :: [LispVal], closure :: Env}
  | IOFunc ([LispVal] -> EvalM LispVal)
  | Port Handle
  | Any

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

instance Show LispVal where
  show (Atom s) = s
  show (List contents) = "(" ++ unwordsList contents ++ ")"
  show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
  show (Number s) = show s
  show (String s) = "\"" ++ s ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Char c) = "'" ++ [c] ++ "'"
  show (Float f) = show f
  show (PrimitiveFunc _) = "<primitives>"
  show (IOFunc _) = "<io func>"
  show (Port _) = "<io port>"
  show Func {..} = "(lambda (" ++ unwords params ++ maybe "" (" . " ++) vararg ++ ") ...)"
  show Any = "<any>"

instance Eq LispVal where
  Atom a == Atom b = a == b
  List a == List b = a == b
  DottedList a alast == DottedList b blast = a == b && alast == blast
  Number a == Number b = a == b
  String a == String b = a == b
  Bool a == Bool b = a == b
  Char a == Char b = a == b
  Float a == Float b = a == b
  Any == _ = True
  _ == Any = True
  _ == _ = False

data LispError
  = NumArgs Int [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | FileNotFound String
  | Default String

instance Eq LispError where
  _ == _ = True

instance Show LispError where
  show (NumArgs expected found) = "Expected " ++ show expected ++ " args: found values: " ++ unwordsList found
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
  show (Parser parseErr) = "Parse error at " ++ show parseErr
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (UnboundVar message varname) = message ++ ": " ++ show varname
  show (FileNotFound filename) = "File not found: " ++ filename
  show (Default message) = message

type ThrowsError = Either LispError

data Config =
  Config {loadPaths :: [String]}

type EvalM = ReaderT Config (ExceptT LispError IO)

runEvalM :: EvalM a -> IO (ThrowsError a)
runEvalM = runEvalMWith Config {loadPaths = []}

runEvalMPrint :: Show a => EvalM a -> IO String
runEvalMPrint = runEvalMPrintWith Config {loadPaths = []}

runEvalMWith :: Config -> EvalM a -> IO (ThrowsError a)
runEvalMWith config m = runExceptT $ runReaderT m config

runEvalMPrintWith :: Show a => Config -> EvalM a -> IO String
runEvalMPrintWith config = (extractVal . trapError <$>) . (fmap . fmap) show . runEvalMWith config
  where
    trapError :: ThrowsError String -> ThrowsError String
    trapError action = catchError action (return . show)
    extractVal :: ThrowsError a -> a
    extractVal (Right val) = val
    extractVal _ = undefined

liftThrows :: ThrowsError a -> EvalM a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . lookup var <$> readIORef envRef

getVar :: Env -> String -> EvalM LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> EvalM LispVal
setVar envRef var val = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . flip writeIORef val)
    (lookup var env)
  return val

defineVar :: Env -> String -> LispVal -> EvalM LispVal
defineVar envRef var val = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var val
    else liftIO $ do
      env <- readIORef envRef
      valRef <- newIORef val
      writeIORef envRef ((var, valRef) : env)
      return val

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = (++ env) <$> traverse addBinding bindings
    addBinding (var, val) = (var,) <$> newIORef val
