module Rule where

import Automaton
import Data.Char
import Data.List
import Control.Monad ( forM_ )
import System.Environment

import Debug.Trace


main :: IO ()
main = do
  args <- getArgs
  case args of 
    ["detNounV"] -> printRules detNounVerb
    ["detAdjN"]  -> printRules detAdjNoun
    _            -> mapM_ (putStrLn . showAutomaton) [detAdjNoun, detNounVerb]

printRules :: Automaton Tag -> IO ()
printRules a = do mapM_ putStrLn definitions
                  putStrLn ""

                  putStrLn "BEFORE-SECTIONS"
                  mapM_ print (baseRules a)
                  putStrLn ""


                  putStrLn "SECTION"
                  putStrLn ""
                  putStrLn ""
                  putStrLn "# Remove tags NOT between certain states"
                  alltags `forM_` \t -> mapM_ print (removeTag a t)

                  --putStrLn "# Remove tags coming from a certain state (-1C foo)"
                  --[0..bound a] `forM_` \i -> mapM_ print (removeTagFrom a i)
                  --putStrLn ""

                  --putStrLn "# Remove tags coming to a certain state (1C foo)"
                  --[1..bound a] `forM_` \i -> mapM_ print (removeTagTo a i)

                  putStrLn ""
                  putStrLn "# Remove states between certain tags"
                  [0..bound a] `forM_` \i -> mapM_ print (removeState a i)



--------------------------------------------------------------------------------
-- States need to be tags too

data TagPlus = T Tag | TS State | BOS | EOS deriving (Eq)

stateToTag :: State -> TagPlus
stateToTag = TS  --Makes states also into special tags; 
                 --these will be inserted between each word.


alltps :: [TagPlus] --All normal tags as TagPlus, ie. no states nor BOS/EOS
alltps = map T alltags


instance Show TagPlus where
  show (T tag) = show tag
  show (TS s)  = "(s" ++ show s ++ ")"
  show BOS     = ">>>"
  show EOS     = "<<<"


showTS :: [TagPlus] -> String
showTS = intercalate " OR " . map show 

--------------------------------------------------------------------------------
-- Rules, contexts & positions

data Rule = R { target  :: [TagPlus]
              , context :: [Context] } deriving (Eq)


data Context = Yes Position [TagPlus] | No Position [TagPlus] deriving (Eq)

data Position = C Int | NC Int deriving (Eq)

-- Shorthands, we'll write these a lot
c1   = C 1
c_1  = C (-1)
nc1  = NC 1
nc_1 = NC (-1)

instance Show Context where
  show (Yes pos ts) = "(" ++ show pos ++ " " ++ showTS ts ++ ")"
  show (No pos ts)  = "(NOT " ++ show pos ++ " " ++ showTS ts ++ ")"

instance Show Position where
  show (C i)  = show i ++ "C"
  show (NC i) = show i
 
instance Show Rule where
  show (R trg ctxs) = "REMOVE " ++ showTS trg ++ 
                      " IF " ++ (unwords $ map show ctxs) ++ " ;"


definitions :: [String]
definitions = "SET >>> = (>>>) ;":
              "SET <<< = (<<<) ;":
              [ "SET " ++ (x:xs) ++ " = (" ++ (toLower x:xs) ++ ") ;" 
                | (x:xs) <- map show alltags ] 
              ++ [ anySet ]
 where anySet = "SET Any = " ++ showTS alltps ++ " ;"



--------------------------------------------------------------------------------
-- Automaton to Rules

baseRules :: Automaton Tag -> [Rule]
baseRules a = [ R allButStart [bos] --[noPrec] 
              , R allButEnd   [eos] --[noFoll] 
              , R onlyStart   [hasPrec] 
              , R onlyEnd     [hasFoll] ] 

 where
  allButStart = [ TS s | s <- [1..bound a] ] --all states excluding 0 
  allButEnd   = [ TS s | s <- [0..bound a]
                       , not $ final a s ]

  --remove start state from all but first (state) cohort;
  onlyStart = [ TS s | s <- [0] \\ if final a 0
                                    then [0] --EXCEPT if start also accepts
                                     else [] ] 

  --remove states that are accepting and sink from all but last (state) cohort
  onlyEnd = [ TS s | s <- [0..bound a]
                   , final a s 
                   , sink a s ]

  noPrec = No nc_1 alltps
  noFoll = No nc1  alltps
  hasPrec = Yes nc_1 alltps
  hasFoll = Yes nc1  alltps

  --alternative to noPrec and noFoll
  bos = Yes (NC (-1)) [BOS]
  eos = Yes (NC   0)  [EOS]

---

removeTag :: Automaton Tag -> Tag -> [Rule]
removeTag a t = [ R [T t] [No nc_1 (map TS froms)]
                    ,  R [T t] [No nc1  (map TS tos)] ]
  where (froms,tos) = unzip $ withSymbol a t       

---

--TODO: make it not remove a *final* state when it's not followed by something
removeState :: Automaton Tag -> State -> [Rule]
removeState a s = [ R [TS s] [c] 
                    | c@(No _ (x:xs)) <- contexts] --only non-empty ctxs
 where
  (tagsFrom,_) = unzip $ fromState a s
  (_,tagsTo)   = unzip $ toState a s 
  contexts     = [ No (NC 1) (map T $ nub tagsFrom) 
                 , No (NC (-1)) (map T $ nub tagsTo) ] 


--------------------------------------------------------------------------------
-- General utilities

compl :: (Bounded a, Eq a, Enum a) => [a] -> [a]
compl as = [minBound..maxBound] \\ as


--mergeFst [(a,1), (b,2), (c,1), (d,2)] = [([a,c],1),([b,d],2)] 
mergeFst :: (Ord b, Eq b) => [(a,[b])] -> [([a],[b])]
mergeFst a_bs = [ (as, bs) 
                  | as_bs <- groupBy (\(_,b) (_,b') -> b==b') $ sortOn snd a_bs -- :: [[(a,[b])]]
                  , let as = map fst as_bs
                  , let bs = snd $ head as_bs 
                ]

--------------------------------------------------------------------------------
-- Old, unused, TODO make sure it's not actually needed and delete

{-   Desired behaviour:
    REMOVE Noun IF (-1C (s1)) (NOT 1 (s2) OR (s3)) ;
    REMOVE Det OR Noun OR Verb IF (-1C (s0)) (NOT 1 (s3)) ;

   Bad behaviour:
    REMOVE Noun IF (-1C (s1)) (NOT 1 (s2)) ;
    REMOVE Noun IF (-1C (s1)) (NOT 1 (s3)) ;
  
    REMOVE Det IF (-1C (s0)) (NOT 1 (s3)) ;
    REMOVE Noun IF (-1C (s0)) (NOT 1 (s3)) ;
    REMOVE Verb IF (-1C (s0)) (NOT 1 (s3)) ;


removeTagFrom :: Automaton Tag -> State -> [Rule]
removeTagFrom = removeTag transitionFrom c_1 nc1

removeTagTo :: Automaton Tag -> State -> [Rule]
removeTagTo = removeTag transitionTo c1 nc_1


removeTag :: (Automaton Tag -> State -> Tag -> [State])
          -> Position -> Position
          -> Automaton Tag 
          -> State
          -> [Rule]
removeTag tr posFrom posTo a s = never:sometimes
 where
                     -- if s==0, then [ (Det,  [1,2])
                     --               , (Noun, [2,3])
                     --               , (Verb, [3])  ]
  byTag = [ (tag,states) -- if s==1, then [ (Noun, [2,3]) ]
           | tag <- alltags :: [Tag] 
           , let states = tr a s tag 
           , not $ null states ] :: [(Tag,[State])]

  ctxFrom   = Yes posFrom [stateToTag s] -- e.g. `-1C (s0)'


  --If there is no transition with Noun from/to 0, remove all Nouns from/to 0.
  --May not be strictly needed if we go for actual sentences, not symbolic?
  (tags,_)  = unzip byTag  --transitions that happen *to* some state
  never = R [ T t | t <- compl tags ] [ctxFrom]

  -- If there is a transition with Det from 0, 
  -- allow that but remove Det from everywhere else.
  sometimes = [ R trg [ctxFrom, ctxTo]
                | (tags,ss) <- mergeFst byTag
                , let trg = map T tags
                , let ctxTo = No posTo (map stateToTag ss) ]
-}