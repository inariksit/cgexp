import Arbitrary
import Test.QuickCheck

main :: IO ()
main = verboseCheck checkAutomaton


checkAutomaton :: Automaton Tag -> Bool
checkAutomaton a = True