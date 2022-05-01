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

spec_parseString :: Spec
spec_parseString =
  describe "parseString" $ do
    it "parses empty string" $
      parse parseString "" [r|""|] `shouldBe` Right (String [r||])
    it "parses normal string" $
      parse parseString "" [r|"xxx"|] `shouldBe` Right (String [r|xxx|])
    it "parses escaped \\\"" $
      parse parseString "" [r|"x\"y"|] `shouldBe` Right (String [r|x"y|])
    it "parses escaped \\\\" $
      parse parseString "" [r|"x\\y"|] `shouldBe` Right (String [r|x\y|])
    it "parses escaped \\n" $
      parse parseString "" [r|"x\ny"|] `shouldBe` Right (String "x\ny")

spec_parseAtom :: Spec
spec_parseAtom =
  describe "parseAtom" $ do
    it "parses #t" $
      parse parseAtom "" "#t" `shouldBe` Right (Bool True)
    it "parses #f" $
      parse parseAtom "" "#f" `shouldBe` Right (Bool False)
    it "parses letters" $
      parse parseAtom "" "abc" `shouldBe` Right (Atom "abc")
    it "parses symbols" $
      parse parseAtom "" "+-_" `shouldBe` Right (Atom "+-_")
    it "parses letter symbol digit" $
      parse parseAtom "" "+a3" `shouldBe` Right (Atom "+a3")

spec_parseNumber :: Spec
spec_parseNumber =
  describe "parseNumber" $ do
    it "parses decimal" $
      parse parseNumber "" "1234" `shouldBe` Right (Number 1234)
    it "parses oct" $
      parse parseNumber "" "#o0175" `shouldBe` Right (Number 125)
    it "parses hex" $
      parse parseNumber "" "#xabcd" `shouldBe` Right (Number 43981)
    it "fails leading spaces" $
      parse parseNumber "" " 1234" `shouldSatisfy` isLeft
    it "fails not oct digit" $
      parse parseNumber "" "#o8765" `shouldSatisfy` isLeft
    it "fails not hex digit" $
      parse parseNumber "" "#xgabcd" `shouldSatisfy` isLeft

spec_parseChar :: Spec
spec_parseChar =
  describe "parseChar" $ do
    it "parses letter char literals" $
      parse parseChar "" [r|#\a|] `shouldBe` Right (Char 'a')
    it "parses symbol char literals" $
      parse parseChar "" [r|#\#|] `shouldBe` Right (Char '#')
    it "parses char name 'space'" $
      parse parseChar "" [r|#\space|] `shouldBe` Right (Char ' ')
    it "parses char name 'newline'" $
      parse parseChar "" [r|#\newline|] `shouldBe` Right (Char '\n')

spec_parseFloat :: Spec
spec_parseFloat =
  describe "parseFloat" $
    it "parses floats" $ do
  parse parseFloat "" "#i3.14" `shouldBe` Right (Float 3.14)

spec_parseList :: Spec
spec_parseList =
  describe "parseList" $ do
    it "parses normal list" $
      parse parseExpr "" "(1 #o13 abc #\\newline)" `shouldBe`
      Right (List [Number 1, Number 11, Atom "abc", Char '\n'])
    it "parses dotted list" $
      parse parseExpr "" [r|(1 #\# #t #i3.14 . "x\ny")|] `shouldBe`
      Right (DottedList [Number 1, Char '#', Bool True, Float 3.14] (String "x\ny"))
    it "fails bad dotted list" $
      parse parseExpr "" [r|(1 2 3 . 4 5)|] `shouldSatisfy` isLeft

spec_parseQuoted :: Spec
spec_parseQuoted =
  describe "parseQuoted" $ do
    it "parses quoted Atom" $
      parse parseExpr "" "'abc" `shouldBe` Right (List [Atom "quote", Atom "abc"])
    it "parses quoted List" $
      parse parseExpr "" "'(1 #t #f)" `shouldBe` Right (List [Atom "quote",
                                                            List [Number 1, Bool True, Bool False]])
spec_eval :: Spec
spec_eval =
  describe "eval primitives" $ do
    it "evals String" $ do
      eval (String "xxx") `shouldBe` Right (String "xxx")
    it "evals Number" $ do
      eval (Number 1) `shouldBe` Right (Number 1)
    it "evals Bool" $ do
      eval (Bool True) `shouldBe` Right (Bool True)
      eval (Bool False) `shouldBe` Right (Bool False)
    it "evals quoted" $ do
      eval (List [Atom "quote", Number 1]) `shouldBe` Right (Number 1)
    it "evals primitive functions" $ do
      eval (List [Atom "+", Number 1, Number 2]) `shouldBe` Right (Number 3)
      eval (List [Atom "-", Number 1, Number 10]) `shouldBe` Right (Number (-9))
      eval (List [Atom "mod", String "21", String "2"]) `shouldBe` Right (Number 1)
      eval (List [Atom "symbol?", Atom "aaa"]) `shouldBe` Right (Bool True)
      eval (List [Atom "string->symbol", String "xxx"]) `shouldBe` Right (Atom "xxx")
      eval (List [Atom "symbol->string", Atom "xxx"]) `shouldBe` Right (String "xxx")
      eval (List [Atom "string>?", String "xxx", String "xxy"]) `shouldBe` Right (Bool False)
      eval (List [Atom "&&", Bool True, Bool False]) `shouldBe` Right (Bool False)
