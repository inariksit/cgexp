module Automaton where

--------------------------------------------------------------------------------

type State = Int

data Automaton a = A { transition :: State -> State -> [a] 
                     , bound :: State }


fromState :: Automaton a -> State -> [(a,State)]
fromState (A f b) from = [ (a,to) | to <- [0..b]
                                  , a <- f from to ]
  


toState :: Automaton a -> State -> [(State,a)]
toState (A f b) to = [ (from,a) | from <- [0..b]
                                , a <- f from to ]




--------------------------------------------------------------------------------

data Tag = Det | Adj | Noun  deriving (Show,Eq,Enum,Bounded)


detAdjNoun :: Automaton Tag
detAdjNoun = A trans 2
 where 
  trans 0 1 = [Det]
  trans 1 1 = [Adj]
  trans 1 2 = [Adj,Noun]
  trans _ _ = []

