module Axioms where

import Formula

isAxiom :: Fmla -> Bool
isAxiom fmla =
    foldl (||) False $ map (\f -> f fmla) axioms'
  where
      axioms' = [isAxiom1, isAxiom2, isAxiom3, isAxiom4]

isAxiom1 :: Fmla -> Bool
-- ^ True iff Fmla A -> (B -> A)
isAxiom1 (Implication a (Implication _ a')) = a == a'
isAxiom1 _ = False

isAxiom2 :: Fmla -> Bool
-- ^ True iff Fmla (A -> (B -> C)) -> ((A -> B) -> (A -> C))
isAxiom2 (Implication (Implication a (Implication b c)) (Implication (Implication a' b') (Implication a'' c'))) =
    a == a'&& a == a'' && b == b' && c == c'
isAxiom2 _ = False

isAxiom3 :: Fmla -> Bool
-- ^ True iff (-A -> -B) -> ( B -> A )
isAxiom3 (Implication (Implication (Neg a) (Neg b)) (Implication b' a')) =
    a == a' && b == b'
isAxiom3 _ = False

isAxiom4 :: Fmla -> Bool
-- ^ True iff (A -> --A) and (--A -> A)
isAxiom4 (Implication f (Neg (Neg f'))) = f == f'
isAxiom4 (Implication (Neg (Neg f')) f) = f == f'
isAxiom4 _ = False

createAxiom1 :: Fmla -> Fmla -> Fmla
-- ^ Takes two Fmlae and creates an Axiom Axiom 1
createAxiom1 a b = (Implication a (Implication b a))

createAxiom2 :: Fmla -> Fmla -> Fmla -> Fmla
-- ^ Takes 3 Fmlae and creates an Axiom Axiom 2
createAxiom2 a b c =
    (Implication (Implication a (Implication b c)) (Implication (Implication a b) (Implication a c)))

createAxiom3 :: Fmla -> Fmla -> Fmla
createAxiom3 a b = (Implication (Implication (Neg a) (Neg b)) (Implication b a))

createAxiom4 :: Fmla -> Fmla
createAxiom4 f = (Implication f (Neg (Neg f)))

-- Check, if the Axioms are correct
prop_axiom1Test :: Fmla -> Fmla -> Bool
prop_axiom1Test f g = isAxiom1 $ createAxiom1 f g

prop_axiom2Test :: Fmla -> Fmla -> Fmla -> Bool
prop_axiom2Test f g h = isAxiom2 $ createAxiom2 f g h

prop_axiom3Test :: Fmla -> Fmla -> Bool
prop_axiom3Test f g = isAxiom3 $ createAxiom3 f g

prop_axiom4Test :: Fmla -> Bool
prop_axiom4Test f = isAxiom4 $ createAxiom4 f