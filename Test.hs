{-# LANGUAGE FlexibleInstances #-}

module Test where

import Automaton
--import Rule
import Test.QuickCheck

import Control.Monad
--import Data.List
import Debug.Trace


main' :: IO ()
main' = do
         verboseCheck checkAutomaton



checkAutomaton :: Automaton Tag -> Bool
checkAutomaton a = True


--------------------------------------------------------------------------------

instance Arbitrary (Automaton Tag) where
  arbitrary = liftM3 A arbitrary -- trans :: Gen (State -> State -> [a]))
                       (suchThat arbitrary (<10)) -- bound :: Gen State)
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