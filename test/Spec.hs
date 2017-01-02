import Test.QuickCheck
import qualified AxiomTest as Axioms
import qualified ProverTest as Prover

main :: IO ()
main =  do
    Axioms.axiomTests
    Prover.proverTests



