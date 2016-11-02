{-# LANGUAGE FlexibleInstances #-}

module Arbitrary 
    ( randomAutomaton 
    ) where

import Automaton
import Test.QuickCheck

import Control.Monad
import Debug.Trace




randomAutomaton :: IO (Automaton Tag)
randomAutomaton = generate (arbitrary :: Gen (Automaton Tag)) 

--------------------------------------------------------------------------------

instance Arbitrary (Automaton Tag) where
  arbitrary = liftM4 A arbitrary -- trans :: Gen (State -> State -> [a]))
					   (return [minBound..maxBound]) -- alpha :: Gen [State]
                       (elements [1..10]) -- bound :: Gen State)
                       arbitrary -- final :: Gen (State -> Bool))


instance Arbitrary Tag where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary (Symbols Tag) where
  arbitrary = S `fmap` frequency [ (1, sublistOf [minBound..maxBound])
                                 , (3, vectorOf 0 (arbitrary :: Gen Tag)) ]

instance CoArbitrary Tag where
  --coarbitrary :: a -> Gen b -> Gen b
  coarbitrary = coarbitrary . fromEnum
