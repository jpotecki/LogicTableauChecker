import Test.QuickCheck
import qualified AxiomTest as Axioms
import qualified ProverTest as Prover
import qualified FmlaParserTest as Parser
main :: IO ()
main =  do
    putStrLn "\n STARTING TESTS"
    Parser.parserTests
    Axioms.axiomTests
    Prover.proverTests

