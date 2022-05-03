{-# LANGUAGE QuasiQuotes #-}
import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec

import Text.RawString.QQ

import Text.Parsec
import Lisp.Parse
import Lisp.Eval
import Lisp.Types
import Data.Either (isLeft)
import Control.Monad ((>=>))
import Data.Function ((&))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Except
import Data.Traversable

main :: IO()
main = do
  specs <- concat <$> mapM testSpecs
    [spec_parseString,
     spec_parseAtom,
     spec_parseNumber,
     spec_parseChar,
     spec_parseFloat,
     spec_parseList,
     spec_parseQuoted,
     spec_eval]
  defaultMain $ testGroup "All tests" [testGroup "Specs" specs]

shouldBeRight :: (Eq a, Show a, Show e, Eq e) => Either e a -> a -> Expectation
shouldBeRight a b = a `shouldBe` Right b

shouldFail :: (Eq a, Show a, Show e) => Either e a -> Expectation
shouldFail = (`shouldSatisfy` isLeft)

shouldReturnRight :: (Eq a, Show a, Show e, Eq e) => ExceptT e IO a -> a -> Expectation
shouldReturnRight a b = runExceptT a `shouldReturn` Right b

shouldReturnFail :: (Eq a, Show a) => ExceptT LispError IO a -> Expectation
shouldReturnFail a = runExceptT a `shouldReturn` Left (Default "")

spec_parseString :: Spec
spec_parseString =
  describe "parseString" $ do
    it "parses empty string" $
      parse parseString "" [r|""|] `shouldBeRight` String [r||]
    it "parses normal string" $
      parse parseString "" [r|"xxx"|] `shouldBeRight` String [r|xxx|]
    it "parses escaped \\\"" $
      parse parseString "" [r|"x\"y"|] `shouldBeRight` String [r|x"y|]
    it "parses escaped \\\\" $
      parse parseString "" [r|"x\\y"|] `shouldBeRight` String [r|x\y|]
    it "parses escaped \\n" $
      parse parseString "" [r|"x\ny"|] `shouldBeRight` String "x\ny"

spec_parseAtom :: Spec
spec_parseAtom =
  describe "parseAtom" $ do
    it "parses #t" $
      parse parseAtom "" "#t" `shouldBeRight` Bool True
    it "parses #f" $
      parse parseAtom "" "#f" `shouldBeRight` Bool False
    it "parses letters" $
      parse parseAtom "" "abc" `shouldBeRight` Atom "abc"
    it "parses symbols" $
      parse parseAtom "" "+-_" `shouldBeRight` Atom "+-_"
    it "parses letter symbol digit" $
      parse parseAtom "" "+a3" `shouldBeRight` Atom "+a3"

spec_parseNumber :: Spec
spec_parseNumber =
  describe "parseNumber" $ do
    it "parses decimal" $
      parse parseNumber "" "1234" `shouldBeRight` Number 1234
    it "parses oct" $
      parse parseNumber "" "#o0175" `shouldBeRight` Number 125
    it "parses hex" $
      parse parseNumber "" "#xabcd" `shouldBeRight` Number 43981
    it "fails leading spaces" $
      parse parseNumber "" " 1234" & shouldFail
    it "fails not oct digit" $
      parse parseNumber "" "#o8765" & shouldFail
    it "fails not hex digit" $
      parse parseNumber "" "#xgabcd" & shouldFail

spec_parseChar :: Spec
spec_parseChar =
  describe "parseChar" $ do
    it "parses letter char literals" $
      parse parseChar "" [r|#\a|] `shouldBeRight` Char 'a'
    it "parses symbol char literals" $
      parse parseChar "" [r|#\#|] `shouldBeRight` Char '#'
    it "parses char name 'space'" $
      parse parseChar "" [r|#\space|] `shouldBeRight` Char ' '
    it "parses char name 'newline'" $
      parse parseChar "" [r|#\newline|] `shouldBeRight` Char '\n'

spec_parseFloat :: Spec
spec_parseFloat =
  describe "parseFloat" $
    it "parses floats" $ do
  parse parseFloat "" "#i3.14" `shouldBeRight` Float 3.14

spec_parseList :: Spec
spec_parseList =
  describe "parseList" $ do
    it "parses normal list" $
      parse parseExpr "" "(1 #o13 abc #\\newline)" `shouldBeRight`
      List [Number 1, Number 11, Atom "abc", Char '\n']
    it "parses dotted list" $
      parse parseExpr "" [r|(1 #\# #t #i3.14 . "x\ny")|] `shouldBeRight`
      DottedList [Number 1, Char '#', Bool True, Float 3.14] (String "x\ny")
    it "fails bad dotted list" $
      parse parseExpr "" [r|(1 2 3 . 4 5)|] & shouldFail

spec_parseQuoted :: Spec
spec_parseQuoted =
  describe "parseQuoted" $ do
    it "parses quoted Atom" $
      parse parseExpr "" "'abc" `shouldBeRight` List [Atom "quote", Atom "abc"]
    it "parses quoted List" $
      parse parseExpr "" "'(1 #t #f)" `shouldBeRight` List [Atom "quote", List [Number 1, Bool True, Bool False]]

evalParse :: String -> IOThrowsError LispVal
evalParse expr = liftIO nullEnv >>= flip evalParseEnv expr

evalParseEnv :: Env -> String -> IOThrowsError LispVal
evalParseEnv env expr = liftThrows (readExpr expr) >>= eval env

evalParseSeq :: [String] -> IOThrowsError [LispVal]
evalParseSeq exprs = liftIO nullEnv >>= for exprs . evalParseEnv

spec_eval :: Spec
spec_eval =
  describe "eval primitives" $ do
    it "evals String" $ do
      evalParse [r|"xxx"|] `shouldReturnRight` String "xxx"
    it "evals Number" $ do
      evalParse [r|1|] `shouldReturnRight` Number 1
    it "evals Bool" $ do
      evalParse [r|#t|] `shouldReturnRight` Bool True
      evalParse [r|#f|] `shouldReturnRight` Bool False
    it "evals quoted" $ do
      evalParse [r|'1|] `shouldReturnRight` Number 1
    it "evals primitive functions" $ do
      evalParse [r|(+ 1 2)|] `shouldReturnRight` Number 3
      evalParse [r|(- 1 10)|] `shouldReturnRight` Number (-9)
      evalParse [r|(mod 21 2)|] `shouldReturnRight` Number 1
      evalParse [r|(symbol? 'aaa)|] `shouldReturnRight` Bool True
      evalParse [r|(string->symbol "xxx")|] `shouldReturnRight` Atom "xxx"
      evalParse [r|(symbol->string 'xxx)|] `shouldReturnRight` String "xxx"
      evalParse [r|(string>? "xxx" "xxy")|] `shouldReturnRight` Bool False
      evalParse [r|(&& #t #f)|] `shouldReturnRight` Bool False
    it "evals if" $ do
      evalParse [r|(if #t (+ 2 1) "xxx")|] `shouldReturnRight` Number 3
      evalParse [r|(if "" "xxx" "yyy")|] `shouldReturnRight` String "xxx"
      evalParse [r|(if (= 1 2) "xxx" "yyy")|] `shouldReturnRight` String "yyy"
    it "evals car" $ do
      evalParse [r|(car '(1 2))|] `shouldReturnRight` Number 1
      evalParse [r|(car '(1 2 . 3))|] `shouldReturnRight` Number 1
      evalParse [r|(car '())|] & shouldReturnFail
      evalParse [r|(car "xxx")|] & shouldReturnFail
      evalParse [r|(car)|] & shouldReturnFail
      evalParse [r|(car '("xxx") "yyy")|] & shouldReturnFail
    it "evals cdr" $ do
      evalParse [r|(cdr '(1 2 3))|] `shouldReturnRight` List [Number 2, Number 3]
      evalParse [r|(cdr '(1 2 . 3))|] `shouldReturnRight` DottedList [Number 2] (Number 3)
      evalParse [r|(cdr '(1 . 2))|] `shouldReturnRight` Number 2
      evalParse [r|(cdr '())|] & shouldReturnFail
      evalParse [r|(cdr "xxx")|] & shouldReturnFail
      evalParse [r|(cdr)|] & shouldReturnFail
      evalParse [r|(cdr '("xxx") "yyy")|] & shouldReturnFail
    it "evals cons" $ do
      evalParse [r|(cons "xxx" '())|] `shouldReturnRight` List [String "xxx"]
      evalParse [r|(cons "xxx" '(1))|] `shouldReturnRight` List [String "xxx", Number 1]
      evalParse [r|(cons "xxx" '(1 2 . 3))|] `shouldReturnRight` DottedList [String "xxx", Number 1, Number 2] (Number 3)
      evalParse [r|(cons "xxx" "yyy")|] `shouldReturnRight` DottedList [String "xxx"] (String "yyy")
      evalParse [r|(cons 1)|] & shouldReturnFail
      evalParse [r|(cons)|] & shouldReturnFail
      evalParse [r|(cons 1 2 3)|] & shouldReturnFail
    it "evals eq? and eqv?" $ do
      evalParse [r|(eq? "xxx" "xxx")|] `shouldReturnRight` Bool True
      evalParse [r|(eqv? "xxx" "xxx")|] `shouldReturnRight` Bool True
      evalParse [r|(eq? "xxx" "yyy")|] `shouldReturnRight` Bool False
      evalParse [r|(eqv? "xxx" "yyy")|] `shouldReturnRight` Bool False
      evalParse [r|(eq? "1" 1)|] `shouldReturnRight` Bool False
      evalParse [r|(eqv? "1" 1)|] `shouldReturnRight` Bool False
      evalParse [r|(eq? '(1 "xxx") '(1 "xxx"))|] `shouldReturnRight` Bool True
      evalParse [r|(eq? '(1 "xxx") '(1 2))|] `shouldReturnRight` Bool False
      evalParse [r|(eq? '(1 2 3 . 4) '(1 2 3 . 4))|] `shouldReturnRight` Bool True
      evalParse [r|(eq? '(1 3 . 4) '(1 2 3 . 4))|] `shouldReturnRight` Bool False
      evalParse [r|(eq? 1)|] & shouldReturnFail
    it "evals equal?" $ do
      evalParse [r|(equal? 1 "1")|] `shouldReturnRight` Bool True
      evalParse [r|(equal? 1 "2")|] `shouldReturnRight` Bool False
      evalParse [r|(equal? #t "True")|] `shouldReturnRight` Bool True
      evalParse [r|(equal? '(1 2 3) '(1 2 3))|] `shouldReturnRight` Bool True
      evalParse [r|(equal? '(1 "2" 3) '(1 2 3))|] `shouldReturnRight` Bool True
      evalParse [r|(equal? '(1 "2" 3 . 4) '(1 2 3 . "4"))|] `shouldReturnRight` Bool True
    it "evals cond" $ do
      evalParse [r|(cond ((equal? 1 2) 3) (#t 4) (else 5))|] `shouldReturnRight` Number 4
      evalParse [r|(cond 1 2 3)|] & shouldReturnFail
      evalParse [r|(cond (#t 1) 2 (else 3))|] & shouldReturnFail
    it "evals case" $ do
      evalParse [r|(case (* 1 2) ((1 3 5 7 9) 1) ((2 4 6 8) 2))|] `shouldReturnRight` Number 2
      evalParse [r|(case #t ((#f) 1) (else 2))|] `shouldReturnRight` Number 2
      evalParse [r|(case #t (1 2)))|] & shouldReturnFail
    it "evals define, set! and get" $ do
      evalParseSeq [[r|(define x 11)|], [r|x|]] `shouldReturnRight` [Number 11, Number 11]
      evalParseSeq [[r|(define a "x")|], [r|(set! a 1)|], [r|a|]] `shouldReturnRight` [String "x", Number 1, Number 1]
      evalParseSeq [[r|(set! a 1)|], [r|a|]] & shouldReturnFail
