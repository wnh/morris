{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text
import Data.Text (pack)
import Morris.Parser

main :: IO ()
--main = putStrLn $ show $ runParser parseNumber () "TEST" "123"
main = do
  input <- getContents 
  let ast = parseOnly parseFile (pack input)
  putStrLn $ show ast

