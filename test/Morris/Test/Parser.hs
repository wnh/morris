module Morris.Test.Parser (parserSpec) where

import Data.Text hiding (length)
import Morris.Parser
import Test.Hspec
import Test.Hspec.Attoparsec
import Test.QuickCheck

parserSpec :: Spec
parserSpec = do
  describe "parseNumber" $ do

    it "parses arbitrary integers" $ do
      property $ \i -> (pack (show i)) ~> parseNumber `shouldParse` (LispInt i)

    it "parses '123   ' into 123" $ do
      ("123  " :: Text ) ~> parseNumber `shouldParse` (LispInt 123)

    it "fails to parse leading spaces" $ do
      parseNumber `shouldFailOn` ("    123" :: Text )

    it "fails to parse empty strings" $ do
      parseNumber `shouldFailOn` ("" :: Text )

    it "fails to parse words" $ do
      parseNumber `shouldFailOn` ("some words" :: Text )

  describe "parseList" $ do
    it "should parse empty braces" $ do
      ("()" :: Text) ~> parseList `shouldParse` (LispList [])

    it "should parse empty braces whith whitespace" $ do
      ("(  )" :: Text) ~> parseList `shouldParse` (LispList [])

    it "should fail on opening bracesempty braces" $ do
      parseList `shouldFailOn` ("(" :: Text)

    it "parses a sigle number list" $ do
      ("(123)" :: Text) ~> parseList `shouldParse` (LispList [LispInt 123])

    it "parses a two number list" $ do
      ("(123 456)" :: Text) ~> parseList `shouldParse` (LispList [LispInt 123, LispInt 456])

    it "parses a two number list with spaces" $ do
      ("(\t 123 456    )" :: Text) ~> parseList `shouldParse` (LispList [LispInt 123, LispInt 456])

    it "parses a list with a single list" $ do
      ("(())" :: Text) ~> parseList 
        `shouldParse` (LispList [LispList []])

  describe "parseVal" $ do
    it "parses numbers" $ do
      ("123" :: Text) ~> parseVal `shouldParse` (LispInt 123)

    it "parses lists" $ do
      ("()" :: Text) ~> parseVal `shouldParse` (LispList [])

    it "parses complex structures" $ do
      ("(123 ((456 22) 784) 123)" :: Text) ~> parseVal `shouldParse` 
        (LispList [LispInt 123, LispList [LispList [LispInt 456, LispInt 22], LispInt 784], LispInt 123])

    it "parses complex functions" $ do
      ( "(define add (lambda (x y) (+ 1 2)))" :: Text) ~> parseVal `shouldParse`
        (LispList [ LispSymbol "define", LispSymbol "add", LispList [ LispSymbol "lambda",LispList [LispSymbol "x",LispSymbol "y"],LispList [LispSymbol "+",LispInt 1,LispInt 2]]])

  describe "parseSymbol" $ do
    it "parses simple text" $ do
      ("define" :: Text) ~> parseSymbol `shouldParse` (LispSymbol "define")
      ("lambda" :: Text) ~> parseSymbol `shouldParse` (LispSymbol "lambda")

    it "parses dashed content" $ do
      ("define-thing" :: Text) ~> parseSymbol `shouldParse` (LispSymbol "define-thing")

    it "parses star globals" $ do
      ("*global-thing*" :: Text) ~> parseSymbol `shouldParse` (LispSymbol "*global-thing*")

    it "fails when starting with a number" $ do
      parseSymbol `shouldFailOn` ("123bad-symbo" :: Text)

    it "parses single letter symbol" $ do
      ("a" :: Text) ~> parseSymbol `shouldParse` (LispSymbol "a")

    it "parses math symbols" $ do
      ("+" :: Text) ~> parseSymbol `shouldParse` (LispSymbol "+")

  describe "parseFile" $ do
    it "parses single expressions" $ do
      ("(+ 1 a)" :: Text) ~> parseFile `parseSatisfies` ((==1) . length)

    it "parses multiple expressions expressions" $ do
      ("(define a 3)\n(+ 1 a)" :: Text) ~> parseFile `parseSatisfies` ((==2) . length)

    it "works for trailing whitespace" $ do
      ("(define a 3)\n(+ 1 a)  \n\n" :: Text) ~> parseFile `parseSatisfies` ((==2) . length)

    it "works for leading whitespace" $ do
      ("   (define a 3)\n(+ 1 a)" :: Text) ~> parseFile `parseSatisfies` ((==2) . length)

