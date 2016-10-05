module Earley where 

import Data.List (splitAt)
import Data.Maybe (fromMaybe)
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
                   , spans :: Span -- where in the input string this state spans
                   , dot :: Int    -- where in the RHS of the prod are we reading currently 
                   , createdByRule :: String
                   , createdByState :: [Id] } deriving (Ord)
          -- later: , blocksProd :: Maybe Int } -- Nothing: the state is there in the final analysis of the string
                                               -- Just x: word in index x blocks this state from happening

st :: Production -> Span -> Int -> String -> [Id] -> State
st pd sp d c id_ = State pd sp d c id_

zeroState :: Grammar -> State
zeroState gr = st (POS "γ" :-> [NT S]) (0,0) 0 "default" []

printChart :: Chart -> String
printChart c = unlines $ map printState as
  where 
  	as = concatMap ( \(k,v) -> [ (k,i,st) | (i,st) <- IM.assocs v ] ) (IM.assocs c)

  	printState :: (Int,Int,State) -> String
  	printState (k,i,s) = "\nState " ++ show [k,i] ++ ":\n" ++
                         "------------" ++ 
                         show s


type Span = (Int,Int)
type Id = (Int,Int)

type States = IM.IntMap State --TODO: unambiguous ID

type Chart = IM.IntMap States

(!) :: Chart -> Int -> [(Int,State)]
(!) chart key = maybe [] IM.assocs (IM.lookup key chart)

--getTree :: Chart -> Sentence -> 

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

instance Eq State where
	s1 == s2 = prod s1 == prod s2 && 
			   spans s1 == spans s2 &&
			   dot s1 == dot s2

instance Show State where
  show s = "\n" ++ createdByRule s ++ ":\n"
               ++ show (lhs $ prod s) ++ " ->" 
             ++ bdStr ++ " * " ++ adStr ++ "\n"
             ++ "spans " ++ show (spans s) ++ "\n"
             ++ "created by: " ++ (showCreatedBy $ createdByState s) ++ "\n"
             ++ "------------"
   where
       (bd,ad) = splitAt (dot s) (rhs $ prod s)  -- :: ([Symbol],[Symbol])
       (bdStr,adStr) = ( unwords $ map show bd
                       , unwords $ map show ad )

showLite :: State -> String
showLite s = createdByRule s ++ ": " ++ show (prod s)

showCreatedBy :: [Id] -> String
showCreatedBy [] = "virgin birth"
showCreatedBy xs = show xs


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
  chart = IM.insert 0 (IM.insert 0 (zeroState grammar) IM.empty) IM.empty

  go :: Chart -> (Int,String) -> Chart
  go chart (j,word) = trace ("\nRound " ++ show j ++ ": " ++ word ++
                               "\n***********\n"  ++ show chart ++ "\n") $
                      iterate insertRepeatedly chart !! 5 --totally arbitrary
   where

    insertRepeatedly chart = foldl insertStates chart (act `concatMap` recentStates chart)

    recentStates chart = [ (j,i,state) | (i,state) <- chart ! j ]

    insertStates chart (k,state) = IM.insertWith IM.union k newInnerChart chart
      where 
      	innerChart = fromMaybe IM.empty (IM.lookup k chart)
    	maxKey = if IM.null innerChart then (-1) else fst $ IM.findMax innerChart
    	newInnerChart = if state `elem` IM.elems innerChart
    				      then innerChart
    				      else IM.insert (maxKey+1) state innerChart 
    												 -- :: Chart -> (Int,State) -> Chart


    act :: (Int,Int,State) -> [(Int,State)]
    act (k,i,s) | finished s     = trace ("finished") $ completer k i s
                | isPos (next s) = scanner k i s 
                | otherwise      = predictor k i s


    scanner :: Int -> Int -> State -> [(Int,State)]   
    scanner k i s = [ (k+1, st pd (sp,sp+1) (dot s+1) "Scanner" [(k,i)])
                     | pd <- grammar         -- For every state in S(k) of the form (X → γ • Pos, [i,j]),
                     , T word `isRhs` pd     -- examine the input `word' to see if it matches the Pos,
                     , let (_,sp) = spans s ]  -- then create rule (Pos → word, [j,j+1]) and store it in S(k+1)

    completer :: Int -> Int -> State -> [(Int,State)]
    completer k i s@(State pd sp dt _ createdBy) = 
    	[ (k, st (prod oldSt) (o,j) (dot oldSt+1) "Completer" ((k,i):createdBy))
                      | (_,oldSt) <- (chart !) =<< [0..k-1]
                      , next oldSt `isLhs` prod s   -- For every state in S(k) of the form (X → γ •, [j,k]), 
                      , let (o,spOld) = spans oldSt   -- find states in S(j) of the form (Y → α • X β, [i,j])
                      , spOld == fst (spans s)]  
                      							    -- and add (Y → α X • β, [i,k]) to S(k).


    predictor :: Int -> Int -> State -> [(Int,State)] 
    predictor k i s = [ (k, st pd (j,j) 0 "Predictor" [(k,i)])
                        | pd <- grammar
                        , next s `isLhs` pd
                        , let (i,j) = spans s ]



--------------------------------------------------------------------------------

main = do 
	    --sentence <- words `fmap` getLine
	    let chart = earley sentence grammar
	    putStrLn (printChart chart)


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

sentence = words "the cat marks" -- essays"

isRhs :: Symbol -> Production -> Bool
isRhs symb (_ :-> rh) = symb `elem` rh

isLhs :: Symbol -> Production -> Bool
isLhs (NT symb) (lh :-> _) = symb == lh
isLhs _         _          = False
