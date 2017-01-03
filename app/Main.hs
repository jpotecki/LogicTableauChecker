module Main (main) where

import System.Environment as Sys
import Prover                       (satisfiable, valid)
import FmlaParser                   (parseFmla')

main :: IO ()
main = getArgs >>= parse >>= putStrLn

parse :: [String] -> IO (String)
parse []     = usage
parse ("-h":xs) = usage
parse ("--satisfiable":[])        = return $ "please provide a fmla"
parse ("--satisfiable":fmla:_)   = case parseFmla' (fmla) of
                    Left _  -> return $ (fmla) ++ " is not a formula"
                    Right f ->  if satisfiable f
                                then return $ (fmla) ++ " is satisfiable"
                                else return $ (fmla) ++ " is not satisfiable"
parse ("--valid":fmla:_)   = case parseFmla' (fmla) of
                    Left _  -> return $ (fmla) ++ " is not a formula"
                    Right f ->  if valid f
                                then return $ (fmla) ++ " is valid"
                                else return $ (fmla) ++ " is not valid"

usage :: IO (String)
usage   = return "Usage: --satisfiable [fmla]  --valid [fmla] "
