module FmlaParserTest where

import Test.QuickCheck
import Formula
import FormulaTest
import FmlaParser

parserTests :: IO ()
parserTests = do
    putStrLn "Testing Parser"
    quickCheck prop_ParserTest

prop_ParserTest :: Fmla -> Bool
prop_ParserTest fmla = case parseFmla' (show fmla) of
    Left _  -> False
    Right _ -> True