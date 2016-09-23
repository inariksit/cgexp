module Earley where 

import Data.List (intercalate)
import qualified Data.Set as S
import qualified Data.IntMap as IM
import Data.IntMap ((!))


type Grammar = [Production]
type Sentence = [String]

data Production = (:->) { lhs :: Nonterminal
                        , rhs :: RHS } --different options of consecutive symbols

data Nonterminal = S | NP | VP | PP | POS String deriving (Eq) -- S is special; beside that, any other letter or word
type Terminal = String -- anything goes
data Symbol = T Terminal | NT Nonterminal deriving (Eq) -- To have terminals and nonterminals in the same list

type RHS = [Symbol] -- Any consecutive symbols

data State = State { prod :: Production 
                   , orig :: Int -- where the constituent predicted by this state begins
                   , dot :: Int -- where in the RHS of the prod are we reading currently 
                   , blocksProd :: Maybe Int } deriving (Show)
         									   -- Nothing: the state is there in the final analysis of the string
                                               -- Just x: word in index x blocks this state from happening

zeroState :: Grammar -> [State]
zeroState gr = [ State sp 0 0 Nothing | sp <- filter start gr ]



type Chart = IM.IntMap [State] --(S.Set State)

--------------------------------------------------------------------------------

instance Show Symbol where
  show (NT nt) = show nt
  show (T ter) = ter

instance Show Nonterminal where
  show S     = "S"
  show NP    = "NP"
  show VP    = "VP"
  show PP    = "PP"
  show (POS x) = x

instance Show Production where
  show (lh :-> rh) = show lh ++ "->" ++ intercalate "|" (map show rh)

--------------------------------------------------------------------------------

start :: Production -> Bool
start (S :-> _) = True
start _         = False

finished :: State -> Bool
finished s = dot s >= length (rhs $ prod s)

next :: State -> Symbol
next s = (rhs $ prod s) !! dot s

terminal :: Symbol -> Bool
terminal (T _) = True
terminal _     = False

isPos :: Symbol -> Bool
isPos (NT (POS _)) = True
isPos _       = False

--------------------------------------------------------------------------------

earley :: Sentence -> Grammar -> Chart
earley sent gr = foldl go states (zip [0..] sent)
 where 
  states = IM.insert 0 (zeroState gr) IM.empty

  go :: Chart -> (Int,String) -> Chart
  go states (j,word) = IM.insertWith (++) j (concatMap act latestStates) states  
  	

   where
    latestStates = states ! (j-1)

    completer :: State -> [State]
    -- For every state in S(k) of the form (X → γ •, j), 
    -- find states in S(j) of the form (Y → α • X β, i) and add (Y → α X • β, i) to S(k).
    completer s = undefined

    scanner :: State -> [State] -- For every state in S(k) of the form (X → γ • Pos, [i,j]),  
    scanner s = undefined     -- examine the input `word' to see if it matches the Pos,
    						   -- then create rule (Pos → word, [j,j+1])

    predictor :: State -> [State]
    predictor s = [ State pd d d Nothing | pd <- filter (isLhs (next s)) gr ]
     where (o,d) = (orig s, dot s)

    act :: State -> [State]
    act s | finished s     = completer s
          | isPos (next s) = scanner s 
          | otherwise      = predictor s

--------------------------------------------------------------------------------


n = POS "N"
v = POS "V"
dt = POS "Det"

grammar = [ S  :-> [NT NP, NT VP]
          , NP :-> [NT dt, NT n]
          , NP :-> [NT NP, NT n]
          , VP :-> [NT v,  NT NP]
          , VP :-> [NT v]
          , dt :-> [T "the"]
          , n  :-> [T "cat"]
          , n  :-> [T "marks"]
          , n  :-> [T "essays"]
          , v  :-> [T "cat"]
          , v  :-> [T "marks"]
          , v  :-> [T "sleeps"]
          ]

--pos :: Grammar -> Terminal -> [Symbol]
--pos gr term = map rhs $ filter isTerm gr

isLhs symb (_ :-> symbs) | symb `elem` symbs = True
						 | otherwise         = False

