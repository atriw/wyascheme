-- |

module Lisp (
  P.readExpr,
  P.readExprList,
  E.eval,
  Pri.bindings,
  T.LispVal(..),
  T.LispError(..),
  T.Env,
  T.Config,
  T.ThrowsError,
  T.EvalM,
  T.liftThrows,
  T.runEvalM,
  T.runEvalMPrint,
  T.nullEnv,
  T.defineVar,
  T.setVar,
  T.getVar,
  evalParse,
  evalParseSeq,
  evalParsePrint
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Traversable (for)

import qualified Lisp.Types as T
import qualified Lisp.Parse as P
import qualified Lisp.Eval as E
import qualified Lisp.Primitive as Pri

evalParse :: String -> T.EvalM T.LispVal
evalParse expr = liftIO Pri.bindings >>= flip evalParseEnv expr

evalParseEnv :: T.Env -> String -> T.EvalM T.LispVal
evalParseEnv env expr = T.liftThrows (P.readExpr expr) >>= E.eval env

evalParseSeq :: [String] -> T.EvalM [T.LispVal]
evalParseSeq exprs = liftIO Pri.bindings >>= for exprs . evalParseEnv

evalParsePrint :: String -> IO String
evalParsePrint expr = T.runEvalMPrint $ evalParse expr
