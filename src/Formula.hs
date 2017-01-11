module Formula ( Fmla(..), negFmla, allProps, allBins ) where

import Data.List (delete)

type BinaryConnector = Char

allProps :: [Char]
allProps = delete 'v' ['a'..'z']

allBins :: [Char]
allBins = "^>v"

data Fmla = Prop     Char
          | Neg      Fmla
          | Conjunction Fmla Fmla
          | Disjunction Fmla Fmla
          | Implication Fmla Fmla
          deriving (Eq)

instance Show Fmla where
  show (Prop c)  = [c]
  show (Neg fmla) = "-" ++ show fmla
  show (Conjunction a b) =  '(' : show a ++ "^" ++ show b ++ ")"
  show (Disjunction a b) =  '(' : show a ++ "v" ++ show b ++ ")"
  show (Implication a b) =  '(' : show a ++ ">" ++ show b ++ ")"

negFmla :: Fmla -> Fmla
negFmla fmla = Neg fmla