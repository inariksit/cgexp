import Arbitrary
import Automaton
import Test.QuickCheck

main :: IO ()
main = verboseCheck checkAutomaton

--TODO: add some actual tests
checkAutomaton :: Automaton Tag -> Bool
checkAutomaton a = True