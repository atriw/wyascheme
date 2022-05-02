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

buildFunc :: String -> [LispVal] -> LispVal
buildFunc f args = List $ Atom f : args

buildQuoted :: LispVal -> LispVal
buildQuoted v = List [Atom "quote", v]

evalParse :: String -> ThrowsError LispVal
evalParse = readExpr >=> eval

spec_eval :: Spec
spec_eval =
  describe "eval primitives" $ do
    it "evals String" $ do
      eval (String "xxx") `shouldBeRight` String "xxx"
    it "evals Number" $ do
      eval (Number 1) `shouldBeRight` Number 1
    it "evals Bool" $ do
      eval (Bool True) `shouldBeRight` Bool True
      eval (Bool False) `shouldBeRight` Bool False
    it "evals quoted" $ do
      eval (buildFunc "quote" [Number 1]) `shouldBeRight` Number 1
    it "evals primitive functions" $ do
      eval (buildFunc "+" [Number 1, Number 2]) `shouldBeRight` Number 3
      eval (buildFunc "-" [Number 1, Number 10]) `shouldBeRight` Number (-9)
      eval (buildFunc "mod" [String "21", String "2"]) `shouldBeRight` Number 1
      eval (buildFunc "symbol?" [Atom "aaa"]) `shouldBeRight` Bool True
      eval (buildFunc "string->symbol" [String "xxx"]) `shouldBeRight` Atom "xxx"
      eval (buildFunc "symbol->string" [Atom "xxx"]) `shouldBeRight` String "xxx"
      eval (buildFunc "string>?" [String "xxx", String "xxy"]) `shouldBeRight` Bool False
      eval (buildFunc "&&" [Bool True, Bool False]) `shouldBeRight` Bool False
    it "evals if" $ do
      eval (List [Atom "if", Bool True, List [Atom "+", Number 2, Number 1], String "xxx"]) `shouldBeRight` Number 3
      eval (List [Atom "if", String "", String "xxx", String "yyy"]) `shouldBeRight` String "xxx"
      eval (List [Atom "if", List [Atom "=", Number 1, Number 2], String "xxx", String "yyy"]) `shouldBeRight` String "yyy"
    it "evals car" $ do
      eval (buildFunc "car" [buildQuoted $ List [Number 1, Number 2]]) `shouldBeRight` Number 1
      eval (buildFunc "car" [buildQuoted $ DottedList [Number 1, Number 2] (Number 3)]) `shouldBeRight` Number 1
      eval (buildFunc "car" [buildQuoted $ List []]) & shouldFail
      eval (buildFunc "car" [String "xxx"]) & shouldFail
      eval (buildFunc "car" []) & shouldFail
      eval (buildFunc "car" [buildQuoted $ List [String "xxx"], String "yyy"]) & shouldFail
    it "evals cdr" $ do
      eval (buildFunc "cdr" [buildQuoted $ List [Number 1, Number 2, Number 3]]) `shouldBeRight` List [Number 2, Number 3]
      eval (buildFunc "cdr" [buildQuoted $ DottedList [Number 1, Number 2] (Number 3)]) `shouldBeRight` DottedList [Number 2] (Number 3)
      eval (buildFunc "cdr" [buildQuoted $ DottedList [Number 1] (Number 2)]) `shouldBeRight` Number 2
      eval (buildFunc "cdr" [buildQuoted $ List []]) & shouldFail
      eval (buildFunc "cdr" [String "xxx"]) & shouldFail
      eval (buildFunc "cdr" []) & shouldFail
      eval (buildFunc "cdr" [buildQuoted $ List [String "xxx"], String "yyy"]) & shouldFail
    it "evals cons" $ do
      eval (buildFunc "cons" [String "xxx", buildQuoted $ List []]) `shouldBeRight` List [String "xxx"]
      eval (buildFunc "cons" [String "xxx", buildQuoted $ List [Number 1]]) `shouldBeRight` List [String "xxx", Number 1]
      eval (buildFunc "cons" [String "xxx", buildQuoted $ DottedList [Number 1, Number 2] (Number 3)]) `shouldBeRight` DottedList [String "xxx", Number 1, Number 2] (Number 3)
      eval (buildFunc "cons" [String "xxx", String "yyy"]) `shouldBeRight` DottedList [String "xxx"] (String "yyy")
      eval (buildFunc "cons" [Number 1]) & shouldFail
      eval (buildFunc "cons" []) & shouldFail
      eval (buildFunc "cons" [Number 1, Number 2, Number 3]) & shouldFail
    it "evals eq? and eqv?" $ do
      eval (buildFunc "eq?" [String "xxx", String "xxx"]) `shouldBeRight` Bool True
      eval (buildFunc "eqv?" [String "xxx", String "xxx"]) `shouldBeRight` Bool True
      eval (buildFunc "eq?" [String "xxx", String "yyy"]) `shouldBeRight` Bool False
      eval (buildFunc "eqv?" [String "xxx", String "yyy"]) `shouldBeRight` Bool False
      eval (buildFunc "eq?" [String "xxx", Atom "xxx"]) `shouldBeRight` Bool False
      eval (buildFunc "eqv?" [String "xxx", Atom "xxx"]) `shouldBeRight` Bool False
      eval (buildFunc "eq?" [buildQuoted $ List [Number 1, String "xxx"], buildQuoted $ List [Number 1, String "xxx"]]) `shouldBeRight` Bool True
      eval (buildFunc "eq?" [buildQuoted $ List [Number 1, String "xxx"], buildQuoted $ List [Number 1, Number 2]]) `shouldBeRight` Bool False
      evalParse [r|(eq? '(1 2 3 . 4) '(1 2 3 . 4))|] `shouldBeRight` Bool True
      evalParse [r|(eq? '(1 3 . 4) '(1 2 3 . 4))|] `shouldBeRight` Bool False
      evalParse [r|(eq? 1)|] & shouldFail
    it "evals equal?" $ do
      evalParse [r|(equal? 1 "1")|] `shouldBeRight` Bool True
      evalParse [r|(equal? 1 "2")|] `shouldBeRight` Bool False
      evalParse [r|(equal? #t "True")|] `shouldBeRight` Bool True
      evalParse [r|(equal? '(1 2 3) '(1 2 3))|] `shouldBeRight` Bool True
      evalParse [r|(equal? '(1 "2" 3) '(1 2 3))|] `shouldBeRight` Bool True
      evalParse [r|(equal? '(1 "2" 3 . 4) '(1 2 3 . "4"))|] `shouldBeRight` Bool True
    it "evals cond" $ do
      evalParse [r|(cond ((equal? 1 2) 3) (#t 4) (else 5))|] `shouldBeRight` Number 4
      evalParse [r|(cond 1 2 3)|] & shouldFail
      evalParse [r|(cond (#t 1) 2 (else 3))|] & shouldFail
