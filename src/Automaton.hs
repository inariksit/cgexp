module Automaton where

import Data.List ( delete, nub )
--------------------------------------------------------------------------------

type State = Int

--workaround for generating QuickCheck.
--I suspect there's a better way, but I don't know of it.
newtype Symbols a = S { symb :: [a] }

{- Nicer for random generating automata to have the transition function
   as State->State->[a], instead of State->a->State.
-}
data Automaton a = A { trans :: State -> State -> Symbols a 
                     , alpha :: [a]
                     , bound :: State 
                     , final :: State -> Bool }

--to get the list of symbols directly
tr :: Automaton a -> State -> State -> [a]
tr a s s' = symb $ trans a s s'

instance (Show a) => Show (Automaton a) where
  show = showAutomaton

showAutomaton :: (Show a) => Automaton a -> String
showAutomaton a = unlines  
  [        pad ++ show vals  ++ "\n" ++
    ifFin from ++ arrow ++ ifFin to ++ "\n"
    | from <- [0..bound a]
    , to <- [0..bound a]
    , let vals = tr a from to
    , not $ null vals 

    , let padLen = length $ ifFin from
    , let pad = replicate padLen ' '
    , let arrLen = length $ show vals
    , let arrow = replicate arrLen '-' ++ ">"

  ]
 where ifFin s = if final a s
                  then "(" ++ show s ++ ")"
                  else show s

---

fromState :: Automaton a -> State -> [(a,State)]
fromState a sFrom = [ (v,sTo) | sTo <- [0..bound a]
                              , v <- tr a sFrom sTo ]

toState :: Automaton a -> State -> [(State,a)]
toState a sTo = [ (sFrom,v) | sFrom <- [0..bound a]
                            , v <- tr a sFrom sTo ]

---
 
withSymbol :: (Eq a, Enum a, Bounded a) => Automaton a -> a -> [(State,State)]
withSymbol a s = [ (from,to) | from <- [0..bound a]  
                             , to <- [0..bound a]
                             , s `elem` tr a from to ]

--which symbols lead to the state, and which symbols go out from it.
withState :: (Eq a, Enum a, Bounded a) => Automaton a -> State -> ([a],[a])
withState aut s = ( nub $ concat [ tr aut t s | t <- [0..bound aut] ] 
                  , nub $ concat [ tr aut s t | t <- [0..bound aut] ] )


---

sink :: Automaton a -> State -> Bool
sink a s = all null [ tr a s t | t <- [0..bound a] ]

noTransitionsTo :: Automaton a -> State -> Bool
noTransitionsTo a s = all null [ symb $ trans a t s | t <- [0..bound a] ]

--------------------------------------------------------------------------------

data Tag = Det | Adj | Noun | Verb deriving (Show,Eq,Enum,Bounded)

alltags :: [Tag]
alltags = [minBound..maxBound]

detAdjNoun :: Automaton Tag
detAdjNoun = A t alph 2 fin
 where 
  t 0 1 = S [Det]
  t 1 1 = S [Adj]
  t 1 2 = S [Noun]
  t _ _ = S []

  fin 2 = True
  fin _ = False

  alph = delete Verb alltags

detNounVerb :: Automaton Tag
detNounVerb = A t alltags 3 fin
 where
  t 0 1 = S [Det]
  t 0 2 = S [Noun]
  t 0 3 = S [Det,Noun,Verb]
  t 1 1 = S [Adj]
  t 1 2 = S [Noun]
  t 1 3 = S [Noun]
  t 2 3 = S [Verb]
  t _ _ = S []

  fin 3 = True
  fin 2 = True
  fin _ = False

{-
detNounVerb :: Automaton Tag
detNounVerb = A t 3 fin
 where
  t 0 1 = [Det]
 -- t 1 1 = [Adj]
  t 1 2 = [Noun]
  t 2 3 = [Verb]
  t _ _ = []

  fin 3 = True
  fin _ = False
-}