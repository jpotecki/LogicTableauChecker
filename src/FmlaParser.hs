module FmlaParser ( parseFmla, parseFmla' ) where

import Formula
import Control.Monad (liftM)
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))

parseFmla :: String -> Maybe Fmla
parseFmla str = case P.parse parseExpr "" str of
    Left err -> Nothing
    Right fm -> Just fm

parseFmla' :: String -> Either P.ParseError Fmla
parseFmla' str = P.parse parseExpr "" str

bins :: P.Parser Char
bins = P.oneOf allBins

props :: P.Parser Char
props = P.oneOf allProps

readExpr :: String -> String
readExpr str = case P.parse parseExpr "" str of
    Left  err  -> "ERROR " ++ show err
    Right fmla -> "Found value " ++ show fmla

parseProp :: P.Parser Fmla
parseProp = do  c <- props
                return $ Prop c

parseBinConn :: P.Parser Fmla
parseBinConn = do
    P.char '('
    expr1 <- parseExpr
    bin   <- bins
    expr2 <- parseExpr
    P.char ')'
    return $ case bin of
                '^' -> Conjunction expr1 expr2
                'v' -> Disjunction expr1 expr2
                '>' -> Implication expr1 expr2

parseNeg :: P.Parser Fmla
parseNeg = do   P.char '-'
                expr <- parseExpr
                return $ Neg expr


parseExpr :: P.Parser Fmla
parseExpr =  parseNeg
         <|> parseBinConn
         <|> parseProp
