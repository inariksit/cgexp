{-# LANGUAGE FlexibleInstances #-}

module Test where

import Automaton
import Test.QuickCheck

import Control.Monad
import Debug.Trace


main' :: IO ()
main' = do
         verboseCheck checkAutomaton



checkAutomaton :: Automaton Tag -> Bool
checkAutomaton a = True


randomAutomaton :: IO (Automaton Tag)
randomAutomaton = generate (arbitrary :: Gen (Automaton Tag)) 

--------------------------------------------------------------------------------

instance Arbitrary (Automaton Tag) where
  arbitrary = liftM3 A arbitrary -- trans :: Gen (State -> State -> [a]))
                       (elements [1..10]) -- bound :: Gen State)
                       arbitrary -- final :: Gen (State -> Bool))


--instance Arbitrary Tag where
--    --arbitrary :: Gen a
--    arbitrary = elements [minBound..maxBound]

instance Arbitrary (Symbols Tag) where
  --arbitrary :: Gen a
  arbitrary = S `fmap` sublistOf [minBound..maxBound]

instance CoArbitrary Tag where
  --coarbitrary :: a -> Gen b -> Gen b
  coarbitrary = coarbitrary . fromEnum



--------------------------------------------------------------------------------