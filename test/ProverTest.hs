module  ProverTest where

import Prover
import Axioms
import Formula
import FormulaTest
import Test.QuickCheck

proverTests :: IO ()
proverTests = do
    putStrLn "Running Robins Tests"
    quickCheck prop_robinTests
    putStrLn "Testing with negated Axiom1"
    quickCheck prop_NotSatisfiable1
    putStrLn "Testing with Axiom1"
    quickCheck prop_Satisfiable1
    putStrLn "Testing with negated Axiom2"
    quickCheck prop_NotSatisfiable2
    putStrLn "Testing with Axiom2"
    quickCheck prop_Satisfiable2
    putStrLn "Testing with negated Axiom3"
    quickCheck prop_NotSatisfiable3
    putStrLn "Testing with Axiom3"
    quickCheck prop_Satisfiable3
    putStrLn "Testing with negated Axiom4"
    quickCheck prop_NotSatisfiable4
    putStrLn "Testing with Axiom4"
    quickCheck prop_Satisfiable4

robinsInput :: [ (String, Bool) ]
robinsInput = [ ("-(p>p)" , False)
              , ("-(p>(q>p))" , False)
              , ("-((p>q)>p)" , True)
              , ("--((pvq)>(-p^-q))", True)
              , ("(p^-p)", False)
              , ("((p>q)^(q>p))", True)
              , ("-((p>(qvr))>((p>q)v(p>r)))", False)
              , ("((p>(qvr))>((p>q)v(p>r)))", True)
              , ("((p>q)^(-q>-p))", True)
              , ("((pvq)^p>-q)", False) -- not a Fmla
              ]
runRobinsTests :: [(Bool, Bool)]
runRobinsTests = map (\(fmla, expected) -> (satisfiable' fmla, expected)) robinsInput

passTests :: Bool
passTests = foldl (&&) True $ [ x == y | (x, y) <- runRobinsTests ]

-- QuickCheck in order to check if the prover is correct
prop_NotSatisfiable1 :: Fmla -> Fmla -> Bool
prop_NotSatisfiable1 f g = not (satisfiable $ negFmla $ createAxiom1 f g)

prop_Satisfiable1 :: Fmla -> Fmla -> Bool
prop_Satisfiable1 f g = satisfiable $ createAxiom1 f g

prop_NotSatisfiable2 :: Fmla -> Fmla -> Fmla -> Bool
prop_NotSatisfiable2 f g h = not (satisfiable $ negFmla $ createAxiom2 f g h)

prop_Satisfiable2 :: Fmla -> Fmla -> Fmla -> Bool
prop_Satisfiable2 f g h = satisfiable $ createAxiom2 f g h

prop_NotSatisfiable3 :: Fmla -> Fmla -> Bool
prop_NotSatisfiable3 f g = not (satisfiable $ negFmla $ createAxiom3 f g)

prop_Satisfiable3 :: Fmla -> Fmla -> Bool
prop_Satisfiable3 f g = satisfiable $ createAxiom3 f g

prop_Satisfiable4 ::Fmla -> Bool
prop_Satisfiable4 f = satisfiable $ createAxiom4 f

prop_NotSatisfiable4 :: Fmla ->  Bool
prop_NotSatisfiable4 f = not (satisfiable $ negFmla $ createAxiom4 f)

prop_robinTests :: Bool
prop_robinTests = passTests