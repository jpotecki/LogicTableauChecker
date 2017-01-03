module Main (main) where

import System.Environment as Sys
import Control.Monad ((>=>))

import Prover (satisfiable)
import FmlaParser (parseFmla')
import Formula


main :: IO ()
main = getArgs >>= parse >>= putStrLn

parse :: [String] -> IO (String)
parse ["-h"] = usage
parse str'   = case parseFmla' str of
    Left _  -> return $ str ++ " is not a formula"
    Right f -> if satisfiable f then return $ (str ++ " is satisfiable")
                                else return $ (str ++ " is not satisfiable")
  where
    str = concat str'

usage :: IO (String)
usage   = return "Usage: --satisfiable \"fmla\""
