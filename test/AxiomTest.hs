module AxiomTest where

import Test.QuickCheck
import Formula
import Axioms
import FormulaTest

axiomTests :: IO ()
axiomTests = do
    putStrLn "Check if Axiom1 builder is correct"
    quickCheck prop_axiom1Test
    putStrLn "Check if Axiom2 builder is correct"
    quickCheck prop_axiom2Test
    putStrLn "Check if Axiom3 builder is correct"
    quickCheck prop_axiom3Test
    putStrLn "Check if Axiom4 builder is correct"
    quickCheck prop_axiom4Test

-- Check, if the Axioms are correct
prop_axiom1Test :: Fmla -> Fmla -> Bool
prop_axiom1Test f g = isAxiom1 $ createAxiom1 f g

prop_axiom2Test :: Fmla -> Fmla -> Fmla -> Bool
prop_axiom2Test f g h = isAxiom2 $ createAxiom2 f g h

prop_axiom3Test :: Fmla -> Fmla -> Bool
prop_axiom3Test f g = isAxiom3 $ createAxiom3 f g

prop_axiom4Test :: Fmla -> Bool
prop_axiom4Test f = isAxiom4 $ createAxiom4 f

