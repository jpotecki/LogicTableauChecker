module FmlaParserTest where

import Test.QuickCheck
import Formula
import FormulaTest
import FmlaParser

parserTests :: IO ()
parserTests = do
    putStrLn "Testing Parser"
    quickCheck prop_ParserTest
    putStrLn "Test False Strings"
    quickCheck prop_ParserTest2

prop_ParserTest :: Fmla -> Bool
prop_ParserTest fmla = case parseFmla' (show fmla) of
    Left _  -> False
    Right _ -> True

prop_ParserTest2 :: Bool
prop_ParserTest2 = foldl (&&) True $ map isNotFmla noFmlas
  where
    isNotFmla :: String -> Bool
    isNotFmla str = case parseFmla str of
        Just _  -> False
        Nothing -> True
    noFmlas :: [String]
    noFmlas = ["p^p", "(p)", "(-p)", "-(p)", "((p))"]