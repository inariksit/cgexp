module Earley where 

import Data.List (splitAt,intercalate,nub)
import Data.Maybe (mapMaybe,fromMaybe)
import qualified Data.IntMap.Strict as IM
--import Debug.Trace (trace)

trace x y = y

type Grammar = [Production]
type Sentence = [String]
data Rule = Default | Predictor | Scanner | Completer deriving (Eq,Show,Ord)

data Production = (:->) { lhs :: Nonterminal
                        , rhs :: [Symbol] } deriving (Eq,Ord) --any consecutive symbols

data Nonterminal = S | NP | VP | PP | POS String deriving (Eq,Ord) -- S is special; beside that, any other letter or word
type Terminal = String -- anything goes
data Symbol = T Terminal | NT Nonterminal deriving (Eq,Ord) -- To have terminals and nonterminals in the same list


data State = State { prod :: Production 
                   , spans :: Span -- where in the input string this state spans
                   , dot :: Int    -- where in the RHS of the prod are we reading currently 
                   , byRule :: Rule
                   , prevStates :: [Id] } deriving (Ord)
          -- later: , blocksProd :: Maybe Int } -- Nothing: the state is there in the final analysis of the string
                                               -- Just x: word in index x blocks this state from happening
zeroState :: State
zeroState = State (POS "γ" :-> [NT S]) (0,0) 0 Default []

type Span = (Int,Int)
type Id = (Int,Int)

type States = IM.IntMap State

type Chart = IM.IntMap States

(!) :: Chart -> Int -> [(Int,State)]
(!) chart key = maybe [] IM.assocs (IM.lookup key chart)

statesIDs :: Chart -> [(Int,Int,State)]
statesIDs c = concatMap ( \(k,v) -> [ (k,i,st) | (i,st) <- IM.assocs v ] ) (IM.assocs c)

takeSpan :: Span -> Sentence -> Sentence
takeSpan (orig,sp) | orig==sp  = const []
                   | otherwise = drop orig . take sp

predState :: Chart -> State -> [State]
predState c s = mapMaybe doubleLookup (prevStates s)
  where doubleLookup (outerK,innerK) = lookup innerK (c ! outerK)

--unluckyAlternative :: Chart -> State -> [(State,State)]
unluckyAlternative c s = [ (s, (k,i,deadEndState))
                                  | fruitfulState <- predState c s
                                  , (k,i,deadEndState) <- statesIDs c
                                  --, byRule deadEndState == Scanner
                                  , lhs (prod deadEndState) /= lhs (prod fruitfulState)
                                  , rhs (prod deadEndState) == rhs (prod fruitfulState) ]


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
  s1 == s2 | byRule s1==Completer && byRule s2==Completer 
              =  prod s1 == prod s2 
                 && dot s1 == dot s2 
                 && prevStates s1 == prevStates s2
           | otherwise = prod s1 == prod s2 
                         && dot s1 == dot s2 

instance Show State where
  show = uncurry (++) . showStateTuple

showLite :: State -> String
showLite s = show (byRule s) ++ ": " ++ show (prod s)

showPrevStates :: [Id] -> String
showPrevStates [] = "-"
showPrevStates xs = intercalate ", " $ map showId xs

showId :: Id -> String
showId (i,j) = show i ++ "." ++ show j

showStateTuple :: State -> (String,String)
showStateTuple s =
  ( " " ++ show (byRule s) ++ " "
        ++ show (lhs $ prod s) ++ " -> " 
        ++ bdot ++ "● " ++ adot ++ "\n"
        ++ show (spans s) ++ " "

  , "\n" ++ (if True --byRule s == Completer 
             then "created by: " ++ showPrevStates (prevStates s) ++ "\n"
             else "")
   ++ "------------")
  where
   (bd,ad)     = splitAt (dot s) (rhs $ prod s)  -- :: ([Symbol],[Symbol])
   (bdot,adot) = ( if null bd then ""
                    else unwords (map show bd) ++ " "
                 , unwords (map show ad) ++ " ")

printChart :: Chart -> Sentence -> String
printChart c sent = unlines $ map (printState sent) as
  where 
    as = statesIDs c

    printState :: Sentence -> (Int,Int,State) -> String
    printState sent (k,i,s) = "\n" ++ showId (k,i) ++ s1 
                                   ++ unwords (takeSpan (spans s) sent) 
                                   ++ s2
      where (s1,s2) = showStateTuple s

--------------------------------------------------------------------------------

finished :: State -> Bool
finished s = length (rhs $ prod s) <= dot s

next :: State -> Symbol
next s = --trace ("next: " ++ show ((rhs $ prod s) !! (dot s))) $
         if finished s
             then trace ("Cannot access next symbol, state is finished\n***" ++ showLite s ++ "***") 
                    $ NT (POS "dummy")
             else rhs (prod s) !! dot s

isPos :: Symbol -> Bool
isPos (NT (POS _)) = True
isPos _       = False

--------------------------------------------------------------------------------

earley :: Sentence -> Grammar -> Chart
earley sent grammar = foldl go chart (zip [0..] sent ++ [(length sent,"dummy")])
 where 
  chart = IM.insert 0 (IM.insert 0 zeroState IM.empty) IM.empty

  go :: Chart -> (Int,String) -> Chart
  go chart (j,word) = --trace ("\nRound " ++ show j ++ ": " ++ word ++
                      --         "\n***********\n"  ++ show chart ++ "\n") $
                      iterate insertRepeatedly chart !! 4 --totally arbitrary
   where

    insertRepeatedly chart = foldl insertStates chart (act `concatMap` recentStates chart)

    recentStates chart = [ (j,i,state) | (i,state) <- chart ! j ]

    insertStates chart (k,state) = IM.insertWith IM.union k newInnerChart chart
      where 
        innerChart = fromMaybe IM.empty (IM.lookup k chart)
        maxKey = if IM.null innerChart then (-1) else fst $ IM.findMax innerChart
        newInnerChart = if state `elem` IM.elems innerChart
                         then trace ("state " ++ show state ++ " already in chart") innerChart
                         else IM.insert (maxKey+1) state innerChart 
                             -- :: Chart -> (Int,State) -> Chart


    act :: (Int,Int,State) -> [(Int,State)]
    act (k,i,s) | finished s     = trace "finished" $ completer k i s
                | isPos (next s) = scanner k i s 
                | otherwise      = predictor k i s


    scanner :: Int -> Int -> State -> [(Int,State)]   
    scanner k i s = [ (k+1, State pd (j,j+1) (dot s+1) Scanner [])
                     | pd@(lh :-> rh) <- grammar 
                     , T word `elem` rh                     
                     --, next s == lh --Uncomment for standard Earley, with top-down filtering
                     , wordIsLegit (T word) (next s) grammar
                     , let (_i,j) = spans s ]

    predictor :: Int -> Int -> State -> [(Int,State)] 
    predictor k i s = [ (k, State pd (j,j) 0 Predictor [] ) --[(k,i)])
                        | pd@(lh :-> rh) <- grammar
                        , next s == NT lh
                        , let (_i,j) = spans s ]

    completer :: Int -> Int -> State -> [(Int,State)]
    completer k i (State (lhCur :-> rhCur) (origCur,spCur) _ _ prevs) = 
      [ (k, State pdOld (origOld,j) (dotOld+1) Completer ((k,i):prevs++prevsOld))
           | (_,old@(State pdOld (origOld,spOld) dotOld _ prevsOld)) 
               <- (chart !) =<< [0..k-1]     -- For every state in S(k) of the form (X → γ •, [j,k]), 
           , next old == NT lhCur            -- find states in S(j) of the form (Y → α • X β, [i,j])
           , spOld == origCur]               -- and add (Y → α X • β, [i,k]) to S(k).
                                        





--------------------------------------------------------------------------------

main = do 
      --sentence <- words `fmap` getLine
      let chart = earley sentence grammar
      --putStrLn (printChart chart sentence)

                -- deleteMax to remove the dummy start state
      let (_,finalState) = IM.findMax (snd $ IM.findMax chart)
      let preds = concatMap (predState chart) `iterate` [finalState]
      let preds' = nub $ concat $ take 10 preds

      putStrLn "---------"
      putStrLn ""
      mapM_ print preds'

      putStrLn "---------"
      putStrLn ""
      putStrLn "now some states that didn't make it to the final parse tree"

      let deadends = concatMap (unluckyAlternative chart) preds'
      mapM_ (\(x,y) -> putStrLn ("\n" ++ show x ++ "\nblocks\n" ++ showNice y)) deadends

showNice :: (Show a) => (Int,Int,a) -> String
showNice (n,k,a) = show n ++ "." ++ show k ++ " " ++ show a
 
--------------------------------------------------------------------------------

n = POS "N"
v = POS "V"
dt = POS "Det"

grammar = [ S  :-> [NT NP, NT VP]
          , NP :-> [NT dt, NT n]
          , NP :-> [NT NP, NT n]
          , NP :-> [NT n]
          , VP :-> [NT v,  NT NP]
          , VP :-> [NT v]
          , dt :-> [T "the"]
          , n  :-> [T "cat"]
          , n  :-> [T "marks"]
          , n  :-> [T "leaves"]
          , n  :-> [T "essays"]
          , v  :-> [T "cat"]
          , v  :-> [T "marks"]
          , v  :-> [T "leaves"]
          ]

sentence = words "the cat marks leaves"


wordIsLegit :: Symbol -> Symbol -> Grammar -> Bool
wordIsLegit word (NT nt) gr = 
  (not.null) [ lh :-> rh | (lh :-> rh) <- grammar
                         , word `elem` rh 
                         , nt == lh ]
