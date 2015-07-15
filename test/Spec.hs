{-# LANGUAGE OverloadedStrings #-}

import Morris.Test.Parser (parserSpec)
import Test.Hspec

main :: IO ()
main =  hspec $ do
  describe "parser" parserSpec

