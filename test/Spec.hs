import Test.QuickCheck
import qualified Axioms
import qualified Prover

main :: IO ()
main =  do
    putStrLn ""
    putStrLn "Running Robins Tests"
    quickCheck Prover.prop_robinTests
    putStrLn "Check if Axiom1 builder is correct"
    quickCheck Axioms.prop_axiom1Test
    putStrLn "Check if Axiom2 builder is correct"
    quickCheck Axioms.prop_axiom2Test
    putStrLn "Check if Axiom3 builder is correct"
    quickCheck Axioms.prop_axiom3Test
    putStrLn "Check if Axiom4 builder is correct"
    quickCheck Axioms.prop_axiom4Test
    putStrLn "Testing with negated Axiom1"
    quickCheck Prover.prop_NotSatisfiable1
    putStrLn "Testing with Axiom1"
    quickCheck Prover.prop_Satisfiable1
    putStrLn "Testing with negated Axiom2"
    quickCheck Prover.prop_NotSatisfiable2
    putStrLn "Testing with Axiom2"
    quickCheck Prover.prop_Satisfiable2
    putStrLn "Testing with negated Axiom3"
    quickCheck Prover.prop_NotSatisfiable3
    putStrLn "Testing with Axiom3"
    quickCheck Prover.prop_Satisfiable3
    putStrLn "Testing with negated Axiom4"
    quickCheck Prover.prop_NotSatisfiable4
    putStrLn "Testing with Axiom4"
    quickCheck Prover.prop_Satisfiable4


