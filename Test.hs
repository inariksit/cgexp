module Test where

import Automaton
import Rule
import Test.QuickCheck

import Control.Monad
--import Data.List
import Debug.Trace


main' :: IO ()
main' = do
         verboseCheck checkTag
         verboseCheck checkAutomaton



--just for fun, to see automatically generated CG rules
checkTag :: Tag -> Bool
checkTag tag = True

checkAutomaton :: Automaton Tag -> Bool
checkAutomaton a = True


--------------------------------------------------------------------------------

instance (Arbitrary a) => Arbitrary (Automaton a) where
	arbitrary = liftM3 A arbitrary -- trans :: Gen (State -> State -> [a]))
						 (suchThat arbitrary (<10)) -- bound :: Gen State)
						 arbitrary -- final :: Gen (State -> Bool))


instance Arbitrary Tag where
	--arbitrary :: Gen a
	arbitrary = elements [minBound..maxBound]

instance CoArbitrary Tag where
    --coarbitrary :: a -> Gen b -> Gen b
    coarbitrary = coarbitrary . fromEnum



--------------------------------------------------------------------------------