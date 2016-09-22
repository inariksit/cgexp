module Earley where 

import Data.List (intercalate)
import qualified Data.IntMap as IM
import Data.IntMap ((!))

type Grammar = [Production]
type Sentence = [String]

data Production = (:->) { lhs :: Nonterminal
                        , rhs :: [RHS] } --different options of consecutive symbols

data Nonterminal = S | N String -- S is special; beside that, any other letter or word
type Terminal = String -- anything goes
data Symbol = T Terminal | NT Nonterminal -- To have terminals and nonterminals in the same list

type RHS = [Symbol] -- Any consecutive symbols

data State = State { prod :: Production 
                   , dot :: Int --where in the rhs-list are we reading currently. 
                   , blocksProd :: Maybe Int } -- Nothing: the state is there in the final analysis of the string
                                               -- Just x: word in index x blocks this state from happening

zeroState :: Grammar -> [State]
zeroState gr = [State startProd 0 Nothing]
  where [startProd] = filter start gr


type Chart = IM.IntMap [State]

--------------------------------------------------------------------------------

instance Show Symbol where
  show (NT nt) = show nt
  show (T ter) = ter

instance Show Nonterminal where
  show S     = "S"
  show (N x) = x

instance Show Production where
  show (lh :-> rh) = show lh ++ "->" ++ intercalate "|" (map show rh)

--------------------------------------------------------------------------------

start :: Production -> Bool
start (S :-> _) = True
start _         = False

finished :: State -> Bool
finished s = dot s >= length (rhs $ prod s)

next :: State -> Symbol
next s = (head $ rhs $ prod s) !! dot s

terminal :: Symbol -> Bool
terminal (T _) = True
terminal _     = False

--------------------------------------------------------------------------------

earley :: Sentence -> Grammar -> Chart
earley sent gr = foldl go IM.empty (zip [0..] ("DUMMY":sent))
 where 
  go :: Chart -> (Int,String) -> Chart
  go states (0,word) = IM.insert 0 (zeroState gr) states

  go states (i,word) = IM.insertWith (++) i (concatMap act latestState) states  
  	

   where
    latestState = states ! (i-1)

    completer :: State -> [State]
    -- For every state in S(k) of the form (X → γ •, j), 
    -- find states in S(j) of the form (Y → α • X β, i) and add (Y → α X • β, i) to S(k).
    completer s = undefined

    scanner :: State -> [State]
    scanner s = undefined

    predictor :: State -> [State]
    predictor s = undefined

    act :: State -> [State]
    act s | finished s        = completer s
          | terminal (next s) = scanner s 
          | otherwise         = predictor s

--------------------------------------------------------------------------------

np = N "NP"
vp = N "VP"
n = N "N"
v = N "V"
dt = N "Det"

grammar = [ S  :-> [ [ NT np, NT vp ] ]
          , np :-> [ [ NT dt, NT n ]  ]
          , vp :-> [ [ NT v,  NT np ] ]
          , dt :-> [ [T "the"], [T "a"] ]
          , n  :-> [ [T "cat"], [T "marks"], [T "essays"] ]
          , v  :-> [ [T "cat"], [T "marks"], [T "sleeps"] ]
          ]