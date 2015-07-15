
module Morris.Parser where

import Data.Attoparsec.Text
import Control.Applicative

-- | AST types =
data LispVal = LispInt Int
             | LispList [LispVal]
             | LispSymbol String
  deriving (Show, Eq)

parseNumber :: Parser LispVal
parseNumber =  (signed decimal) >>= return . LispInt

parseSymbol :: Parser LispVal
parseSymbol = do
    h <- parseInitial
    t <- parseSubsequent
    return $ LispSymbol (h:t)

parseInitial :: Parser Char
parseInitial = satisfy $ inClass "a-zA-Z*+"

parseSubsequent :: Parser String
parseSubsequent  = many $ satisfy $ inClass "a-zA-Z0-9+.@*-" -- ensure the '-' is the last charachter

parseList :: Parser LispVal
parseList = do
  skip (=='(')
  skipSpace
  a <- parseVal `sepBy` skipSpace
  skipSpace
  skip (==')')
  return $ LispList a

parseVal :: Parser LispVal
parseVal =  parseList 
        <|> parseNumber
        <|> parseSymbol

parseFile :: Parser  [LispVal]
parseFile = skipSpace *> parseVal `sepBy'` skipSpace 



