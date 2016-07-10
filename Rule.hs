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

                  putStrLn "BEFORE-SECTIONS"
                  mapM_ print (baseRules a)
                  putStrLn ""


                  putStrLn "SECTION"
                  putStrLn ""
                  putStrLn "# Remove tags coming from a certain state (-1C foo)"
                  [0..bound a] `forM_` \i -> mapM_ print (removeTagFrom a i)
                  putStrLn ""

                  putStrLn "# Remove tags coming to a certain state (1C foo)"
                  [1..bound a] `forM_` \i -> mapM_ print (removeTagTo a i)

                  putStrLn ""
                  putStrLn "# Remove states between certain tags"
                  [0..bound a] `forM_` \i -> mapM_ print (removeState a i)



--------------------------------------------------------------------------------
-- Ugly hacks

data TagPlus = T Tag | TS State | BOS | EOS deriving (Eq)

--all normal tags, ie. no states nor BOS/EOS
alltps :: [TagPlus]
alltps = map T alltags

instance Show TagPlus where
  show (T tag) = show tag
  show (TS s)  = "(s" ++ show s ++ ")"
  show BOS     = ">>>"
  show EOS     = "<<<"

compl :: (Bounded a, Eq a, Enum a) => [a] -> [a]
compl as = [minBound..maxBound] \\ as

stateToTag :: State -> TagPlus
stateToTag = TS  --Makes states also into special tags; 
                 --these will be inserted between each word.


--------------------------------------------------------------------------------


data Rule = R { target  :: [TagPlus]
              , context :: [Context] } deriving (Eq)


data Context = Yes Position [TagPlus] | No Position [TagPlus] deriving (Eq)

data Position = C Int | NC Int deriving (Eq)


instance Show Context where
  show (Yes pos ts) = "(" ++ show pos ++ " " ++ showTS ts ++ ")"
  show (No pos ts)  = "(NOT " ++ show pos ++ " " ++ showTS ts ++ ")"

instance Show Position where
  show (C i)  = show i ++ "C"
  show (NC i) = show i
 
instance Show Rule where
  show (R trg ctxs) = "REMOVE " ++ showTS trg ++ 
                      " IF " ++ (unwords $ map show ctxs) ++ " ;"


showTS :: [TagPlus] -> String
showTS = intercalate " OR " . map show 

--------------------------------------------------------------------------------


definitions :: [String]
definitions = "SET >>> = (>>>) ;":
              "SET <<< = (<<<) ;":
              [ "SET " ++ (x:xs) ++ " = (" ++ (toLower x:xs) ++ ") ;" 
                | (x:xs) <- map show alltags ] 
              ++ [ anySet ]
 where anySet = "SET Any = " ++ (intercalate " OR " $ map show alltps) ++ " ;"


baseRules :: Automaton Tag -> [Rule]
baseRules a = [ R allButStart [bos] --[noPrec] 
              , R allButEnd   [eos] --[noFoll] 
              , R onlyStart   [hasPrec] 
              , R onlyEnd     [hasFoll] ] 

 where


  allButStart = [ TS s | s <- [1..bound a] ] --all states excluding 0 
  allButEnd   = [ TS s | s <- [0..bound a]
                       , not $ final a s ]

  onlyStart --remove start state if it has a preceding cohort;
            --except if start state is also an accepting state, then keep it
    = [ TS s | s <- [0] \\ --all states excluding 0 
                    if final a 0 then [0] else [] ] 

  onlyEnd = [ TS s | s <- [0..bound a]
                   , final a s ]

  noPrec = No (NC (-1)) alltps
  noFoll = No (NC  1  ) alltps
  hasPrec = Yes (NC (-1)) alltps
  hasFoll = Yes (NC   1 ) alltps

  --more readable alternative to noPrec and noFoll
  bos = Yes (NC (-1)) [BOS]
  eos = Yes (NC   1)  [EOS]

--------------------------------------------------------------------------------

{-   Desired behaviour:
    REMOVE Noun IF (-1C (s1)) (NOT 1 (s2) OR (s3)) ;
    REMOVE Det OR Noun OR Verb IF (-1C (s0)) (NOT 1 (s3)) ;

   Bad behaviour:
    REMOVE Noun IF (-1C (s1)) (NOT 1 (s2)) ;
    REMOVE Noun IF (-1C (s1)) (NOT 1 (s3)) ;
  
    REMOVE Det IF (-1C (s0)) (NOT 1 (s3)) ;
    REMOVE Noun IF (-1C (s0)) (NOT 1 (s3)) ;
    REMOVE Verb IF (-1C (s0)) (NOT 1 (s3)) ;
-}

removeTagFrom :: Automaton Tag -> State -> [Rule]
removeTagFrom = removeTag transition (C (-1)) (NC 1)

removeTagTo :: Automaton Tag -> State -> [Rule]
removeTagTo = removeTag transitionTo (C 1) (NC (-1))


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


--------------------------------------------------------------------------------

removeState :: Automaton Tag -> State -> [Rule]
removeState a s = [ R [TS s] [c] 
                    | c@(No _ (x:xs)) <- contexts] --only non-empty ctxs
 where
  (tagsFrom,_) = unzip $ fromState a s
  (_,tagsTo)   = unzip $ toState a s 
  contexts     = [ No (NC 1) (map T $ nub tagsFrom) 
                 , No (NC (-1)) (map T $ nub tagsTo) ] 
         
--------------------------------------------------------------------------------

{-
mergeFst [('a',[1,2,3]), ('b',[1,2]), ('c',[1,2,3]), ('d',[1,2])]
[(['b','d'],[1,2]),(['a','c'],[1,2,3])]

-}

mergeFst :: (Ord b, Eq b) => [(a,[b])] -> [([a],[b])]
mergeFst a_bs = [ (as, bs) 
                  | as_bs <- groupBy (\(_,b) (_,b') -> b==b') $ sortOn snd a_bs -- :: [[(a,[b])]]
                  , let as = map fst as_bs
                  , let bs = snd $ head as_bs 
                ]

