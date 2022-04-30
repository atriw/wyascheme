{-# LANGUAGE QuasiQuotes #-}
import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec

import Text.RawString.QQ

import Text.Parsec
import Parse
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
     spec_parseQuoted]
  defaultMain $ testGroup "All tests" [testGroup "Specs" specs]

spec_parseString :: Spec
spec_parseString =
  describe "parseString" $ do
    it "parses empty string" $ do
      parse parseString "" [r|""|] `shouldBe` Right (String [r||])
    it "parses normal string" $ do
      parse parseString "" [r|"xxx"|] `shouldBe` Right (String [r|xxx|])
    it "parses escaped \\\"" $ do
      parse parseString "" [r|"x\"y"|] `shouldBe` Right (String [r|x"y|])
    it "parses escaped \\\\" $ do
      parse parseString "" [r|"x\\y"|] `shouldBe` Right (String [r|x\y|])
    it "parses escaped \\n" $ do
      parse parseString "" [r|"x\ny"|] `shouldBe` Right (String "x\ny")

spec_parseAtom :: Spec
spec_parseAtom =
  describe "parseAtom" $ do
    it "parses #t" $ do
      parse parseAtom "" "#t" `shouldBe` Right (Bool True)
    it "parses #f" $ do
      parse parseAtom "" "#f" `shouldBe` Right (Bool False)
    it "parses letters" $ do
      parse parseAtom "" "abc" `shouldBe` Right (Atom "abc")
    it "parses symbols" $ do
      parse parseAtom "" "+-_" `shouldBe` Right (Atom "+-_")
    it "parses letter symbol digit" $ do
      parse parseAtom "" "+a3" `shouldBe` Right (Atom "+a3")

spec_parseNumber :: Spec
spec_parseNumber =
  describe "parseNumber" $ do
    it "parses decimal" $ do
      parse parseNumber "" "1234" `shouldBe` Right (Number 1234)
    it "parses oct" $ do
      parse parseNumber "" "#o0175" `shouldBe` Right (Number 125)
    it "parses hex" $ do
      parse parseNumber "" "#xabcd" `shouldBe` Right (Number 43981)
    it "fails leading spaces" $ do
      parse parseNumber "" " 1234" `shouldSatisfy` isLeft
    it "fails not oct digit" $ do
      parse parseNumber "" "#o8765" `shouldSatisfy` isLeft
    it "fails not hex digit" $ do
      parse parseNumber "" "#xgabcd" `shouldSatisfy` isLeft

spec_parseChar :: Spec
spec_parseChar =
  describe "parseChar" $ do
    it "parses letter char literals" $ do
      parse parseChar "" [r|#\a|] `shouldBe` Right (Char 'a')
    it "parses symbol char literals" $ do
      parse parseChar "" [r|#\#|] `shouldBe` Right (Char '#')
    it "parses char name 'space'" $ do
      parse parseChar "" [r|#\space|] `shouldBe` Right (Char ' ')
    it "parses char name 'newline'" $ do
      parse parseChar "" [r|#\newline|] `shouldBe` Right (Char '\n')

spec_parseFloat :: Spec
spec_parseFloat =
  describe "parseFloat" $ do
    it "parses floats" $ do
      parse parseFloat "" "#i3.14" `shouldBe` Right (Float 3.14)

spec_parseList :: Spec
spec_parseList =
  describe "parseList" $ do
    it "parses normal list" $ do
      parse parseExpr "" "(1 #o13 abc #\\newline)" `shouldBe`
        Right (List [Number 1, Number 11, Atom "abc", Char '\n'])
    it "parses dotted list" $ do
      parse parseExpr "" [r|(1 #\# #t #i3.14 . "x\ny")|] `shouldBe`
        Right (DottedList [Number 1, Char '#', Bool True, Float 3.14] (String "x\ny"))
    it "fails bad dotted list" $ do
      parse parseExpr "" [r|(1 2 3 . 4 5)|] `shouldSatisfy` isLeft

spec_parseQuoted :: Spec
spec_parseQuoted =
  describe "parseQuoted" $ do
    it "parses quoted Atom" $ do
      parse parseExpr "" "'abc" `shouldBe` Right (List [Atom "quote", Atom "abc"])
    it "parses quoted List" $ do
      parse parseExpr "" "'(1 #t #f)" `shouldBe` Right (List [Atom "quote",
                                                              List [Number 1, Bool True, Bool False]])
