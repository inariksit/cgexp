module Automaton where

--------------------------------------------------------------------------------

type State = Int

data Automaton a = A { transition :: State -> State -> [a] 
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

--------------------------------------------------------------------------------

data Tag = Det | Adj | Noun  deriving (Show,Eq,Enum,Bounded)


detAdjNoun :: Automaton Tag
detAdjNoun = A trans 2 fin
 where 
  trans 0 1 = [Det]
  trans 1 1 = [Adj]
  trans 1 2 = [Adj,Noun]
  trans _ _ = []

  fin 2 = True
  fin _ = False

