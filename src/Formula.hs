module Formula where

import Test.QuickCheck

type BinaryConnector = Char

allProps :: [Char]
allProps = "prq"

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
  show (Conjunction a b) =  '(' : show a ++ " ^ " ++ show b ++ ")"
  show (Disjunction a b) =  '(' : show a ++ " v " ++ show b ++ ")"
  show (Implication a b) =  '(' : show a ++ " -> " ++ show b ++ ")"

parseFmla :: String -> Maybe Fmla
parseFmla ( x : [] )   = prop x
parseFmla ('-' : xs)   = parseFmla xs >>= \res -> Just (negFmla res)
parseFmla ('(' : xs)   = splitFmla (init xs)
parseFmla _            = Nothing

splitFmla :: String -> Maybe Fmla
splitFmla xs = getBinIndex xs >>= \i -> transToBinFmla i xs

transToBinFmla :: Int -> String -> Maybe Fmla
transToBinFmla index  xs = do b <- parseFmla $ drop (index + 1)  xs
                              a <- parseFmla $ take index xs
                              let bin = xs !! index
                              if validBin bin
                              then (classifyBinFmla a bin b) else Nothing

classifyBinFmla :: Fmla -> BinaryConnector -> Fmla -> Maybe Fmla
classifyBinFmla a bin b = case bin of
  '^' -> Just (Conjunction a b)
  'v' -> Just (Disjunction a b)
  '>' -> Just (Implication a b)
  _ -> Nothing

getBinIndex :: String -> Maybe Int
getBinIndex xs = getBinIndex' xs 0 0
  where
    getBinIndex' :: String -> Int -> Int -> Maybe Int
    getBinIndex' []  _ _ = Nothing
    getBinIndex' ('(' : xs) acc index = getBinIndex' xs (acc + 1) (index + 1)
    getBinIndex' (')' : xs) acc index = getBinIndex' xs (acc - 1) (index + 1)
    getBinIndex' ( x  : xs)  0  index = if validBin x
                                        then Just index
                                        else getBinIndex' xs 0 (index + 1)
    getBinIndex' ( _ :  xs) acc index = getBinIndex' xs acc (index + 1)

prop :: Char -> Maybe Fmla
prop xs = if validProp xs then Just (Prop xs) else Nothing

validProp :: Char -> Bool
validProp x = x `elem` allProps

validBin :: Char -> Bool
validBin x = x `elem` allBins

negFmla :: Fmla -> Fmla
negFmla fmla = Neg fmla

-- QuickCheck Stuff
instance Arbitrary Fmla where
    arbitrary = scale (\x -> if x < 4 then x else 4) $ sized fmlaGen'

fmlaGen :: Gen (Fmla)
fmlaGen = fmlaGen' 4

fmlaGen' :: Int -> Gen (Fmla)
fmlaGen' 0 = propGen
fmlaGen' n = oneof $ fmlaGenerators (n - 1)
  where
    fmlaGenerators :: Int -> [Gen (Fmla)]
    fmlaGenerators n = [ propGen, (negaGen n) , (conjGen n) , (disjGen n) , (implGen n) ]

propGen :: Gen (Fmla)
propGen = elements $ map (\x -> Prop x) allProps

negaGen :: Int -> Gen (Fmla)
negaGen n = Neg <$> fmlaGen' n

conjGen :: Int ->  Gen (Fmla)
conjGen n = Conjunction <$> fmlaGen' n <*> fmlaGen' n

disjGen :: Int ->  Gen (Fmla)
disjGen n = Disjunction <$> fmlaGen' n <*> fmlaGen' n

implGen :: Int ->  Gen (Fmla)
implGen n = Implication <$> fmlaGen' n <*> fmlaGen' n