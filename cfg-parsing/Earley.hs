module Earley where 

import Data.List (splitAt)
import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM
import Debug.Trace (trace)

type Grammar = [Production]
type Sentence = [String]

data Production = (:->) { lhs :: Nonterminal
                        , rhs :: [Symbol] } deriving (Eq,Ord) --any consecutive symbols

data Nonterminal = S | NP | VP | PP | POS String deriving (Eq,Ord) -- S is special; beside that, any other letter or word
type Terminal = String -- anything goes
data Symbol = T Terminal | NT Nonterminal deriving (Eq,Ord) -- To have terminals and nonterminals in the same list


data State = State { prod :: Production 
                   , spans :: (Int,Int) -- where in the input string this state spans
                   , dot :: Int -- where in the RHS of the prod are we reading currently 
                   , createdByRule :: String
                   , createdByState :: Maybe Id } deriving (Eq,Ord)
          -- later: , blocksProd :: Maybe Int } -- Nothing: the state is there in the final analysis of the string
                                               -- Just x: word in index x blocks this state from happening

state :: Production -> (Int,Int) -> Int -> String -> State
state pd sp d c = State pd sp d c Nothing

zeroState :: Grammar -> States
zeroState gr = S.singleton (State (POS "γ" :-> [NT S]) (0,0) 0 "default" (Just (0,0)))

type Id = (Int,Int)

type States = S.Set State --TODO: unambiguous ID

type Chart = IM.IntMap States

(!) :: Chart -> Int -> [State]
(!) chart key = maybe [] S.toAscList (IM.lookup key chart)

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
  show (lh :-> rh) = show lh ++ "->" ++ unwords (map show rh) -- intercalate "|" (map show rh)

instance Show State where
  show s = "\n" ++ createdByRule s ++ ":\n"
               ++ show (lhs $ prod s) ++ " ->" 
             ++ bdStr ++ " * " ++ adStr ++ "\n"
             ++ show (spans s) ++ "\n"
             ++ "created by: " ++ (show $ createdByState s) ++ "\n"
             ++ "-----------------"
   where
       (bd,ad) = splitAt (dot s) (rhs $ prod s)  -- :: ([Symbol],[Symbol])
       (bdStr,adStr) = ( unwords $ map show bd
                       , unwords $ map show ad )

showLite :: State -> String
showLite s = createdByRule s ++ ": " ++ show (prod s)

--------------------------------------------------------------------------------

finished :: State -> Bool
finished s = length (rhs $ prod s) <= (dot s)

next :: State -> Symbol
next s = --trace ("next: " ++ show ((rhs $ prod s) !! (dot s))) $
         if finished s
             then trace ("Cannot access next symbol, state is finished\n***" ++ showLite s ++ "***") 
                    $ NT (POS "dummy")
             else (rhs $ prod s) !! dot s

isPos :: Symbol -> Bool
isPos (NT (POS _)) = True
isPos _       = False

--------------------------------------------------------------------------------

earley :: Sentence -> Grammar -> Chart
earley sent grammar = foldl go chart ((zip [0..] sent) ++ [(length sent,"dummy")])
 where 
  chart = IM.insert 0 (zeroState grammar) IM.empty

  go :: Chart -> (Int,String) -> Chart
  go chart (j,word) = trace ("\nRound " ++ show j ++ ": " ++ word ++
                               "\n***********\n"  ++ show chart ++ "\n") $
                      iterate insertRepeatedly chart !! 5 --totally arbitrary
   where

    insertRepeatedly chart = foldl insertStates chart (act `concatMap` recentStates chart)

    recentStates chart = [ (j,state) | state <- chart ! j ]

    insertStates chart (k,state) = IM.insertWith S.union k (S.singleton state) chart 
    												 -- :: Chart -> (Int,State) -> Chart


    act :: (Int,State) -> [(Int,State)]
    act (k,s) | finished s     = trace ("finished") $ completer k s
              | isPos (next s) = scanner k s 
              | otherwise      = predictor k s


    scanner :: Int -> State -> [(Int,State)]   
    scanner k s = [ (k+1, state pd (j,j+1) (dot s+1) "Scanner")
                     | pd <- grammar         -- For every state in S(k) of the form (X → γ • Pos, [i,j]),
                     , T word `isRhs` pd     -- examine the input `word' to see if it matches the Pos,
                     , let (i,j) = spans s ]  -- then create rule (Pos → word, [j,j+1]) and store it in S(k+1)

    completer :: Int -> State -> [(Int,State)]
    completer k s = [ (k, state (prod oldSt) (o,j) (dot oldSt+1) "Completer")
                      | oldSt <- concatMap (chart !) [0..k-1]
                      , next oldSt `isLhs` prod s   -- For every state in S(k) of the form (X → γ •, [j,k]), 
                      , let (o,_) = spans oldSt ]     -- find states in S(j) of the form (Y → α • X β, [i,j])
                      							    -- and add (Y → α X • β, [i,k]) to S(k).


    predictor :: Int -> State -> [(Int,State)] 
    predictor k s = [ (k, state pd (j,j) 0 "Predictor")
                        | pd <- grammar
                        , next s `isLhs` pd
                        , let (i,j) = spans s ]



--------------------------------------------------------------------------------

main = do 
	    --sentence <- words `fmap` getLine
	    let chart = earley sentence grammar
	    print chart


--------------------------------------------------------------------------------

n = POS "N"
v = POS "V"
dt = POS "Det"

grammar = [ S  :-> [NT NP, NT VP]
          , NP :-> [NT dt, NT n]
        --  , NP :-> [NT NP, NT n]
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

sentence = words "the cat marks essays"

isRhs :: Symbol -> Production -> Bool
isRhs symb (_ :-> rh) = symb `elem` rh

isLhs :: Symbol -> Production -> Bool
isLhs (NT symb) (lh :-> _) = symb == lh
isLhs _         _          = False
