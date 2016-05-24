module Automaton where

--------------------------------------------------------------------------------

type State = Int

data Automaton a = A { trans :: State -> State -> [a] 
                     , bound :: State 
                     , final :: State -> Bool }


showAutomaton :: (Show a) => Automaton a -> String
showAutomaton (A f b _) = unlines  
  [       pad ++ show vals  ++ "\n" ++
    show from ++ arrow ++ show to ++ "\n"
    | from <- [0..b]
    , to <- [0..b]
    , let vals = f from to
    , not $ null vals 


    , let padLen = length $ show from
    , let pad = take padLen $ repeat ' '
    , let arrLen = length $ show vals
    , let arrow = (take arrLen $ repeat '-') ++ ">"
  ]


fromState :: Automaton a -> State -> [(a,State)]
fromState (A f b _) from = [ (a,to) | to <- [0..b]
                                    , a <- f from to ]
  

toState :: Automaton a -> State -> [(State,a)]
toState (A f b _) to = [ (from,a) | from <- [0..b]
                                  , a <- f from to ]

 
withSymbol :: (Eq a, Enum a, Bounded a) => Automaton a -> a -> [(State,State)]
withSymbol (A f b _) symb = [ (from,to) | from <- [0..b]  
                                        , to <- [0..b]
                                        , symb `elem` f from to ] 

transition :: (Eq a, Enum a, Bounded a) => Automaton a -> State -> a -> [State]
transition (A f b _) from symb = [ to | to <- [0..b]
                                      , symb `elem` f from to ]


--------------------------------------------------------------------------------

data Tag = Det | Adj | Noun | Verb deriving (Show,Eq,Enum,Bounded)

alltags :: [Tag]
alltags = [minBound..maxBound]

detAdjNoun :: Automaton Tag
detAdjNoun = A t 2 fin
 where 
  t 0 1 = [Det]
  t 1 1 = [Adj]
  t 1 2 = [Adj,Noun]
  t _ _ = []

  fin 2 = True
  fin _ = False


detNounVerb :: Automaton Tag
detNounVerb = A t 3 fin
 where
  t 0 1 = [Det]
  t 0 2 = [Noun]
  t 0 3 = [Det,Noun,Verb]
--  t 1 1 = [Adj]
  t 1 2 = [Noun]
  t 1 3 = [Noun]
  t 2 3 = [Verb]
  t _ _ = []

  fin 3 = True
  fin 2 = True
  fin _ = False
