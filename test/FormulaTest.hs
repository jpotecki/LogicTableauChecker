module FormulaTest where

import Test.QuickCheck
import Formula

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
    fmlaGenerators n = [ (negaGen n) , (conjGen n) , (disjGen n) , (implGen n) ]

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