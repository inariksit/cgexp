module Rule where

import Automaton
import Test
import Data.Char ( toLower )
import Data.List
import Control.Monad ( forM_ )
import System.Environment ( getArgs )

import Debug.Trace


main :: IO ()
main = do
  args <- getArgs
  case args of 
    ["detNounV"] -> putStrLn $ printRules detNounVerb
    ["detAdjN"]  -> putStrLn $ printRules detAdjNoun
    ["random"]   -> do a <- randomAutomaton
                       print a
                       --putStrLn $ printRules a

                       let n = show (bound a)
                       writeFile "examples/random/random.rlx" (printRules a)
                       writeFile ("examples/random/ex-random-" ++ n ++ ".txt")
                                 (printCohorts $ bound a)
    _            -> mapM_ print [detAdjNoun, detNounVerb]

printRules :: Automaton Tag -> String
printRules a = 
  unlines [ unlines definitions
          , "BEFORE-SECTIONS\n"
          , pr (baseRules a)
          , "SECTION\n"
          , "# Remove tags NOT between certain states"
          , concatMap (pr . removeTag a) alltags
          , "# Remove states between certain tags"
          , concatMap (pr . removeState a) [0..bound a]
          ]

printCohorts :: Int -> String
printCohorts bnd = unlines $ cohortS bnd : replicate bnd (cohortW tags ++ cohortS bnd)
 where tags = ["det", "adj", "noun", "verb"]

cohortW :: [String] -> String
cohortW as = "\n\"<w>\"\n" ++ cohort as

           
cohortS :: Int -> String
cohortS i = "\n\"<s>\"\n" ++ cohort [ "s" ++ show n | n <- [0..i] ]


cohort :: [String] -> String
cohort ss = unlines ( [ "   \""     ++ s ++ "\" "     ++ s | s <- ss ] ++
                      [ "   \"not_" ++ s ++ "\" not_" ++ s | s <- ss ] )

--------------------------------------------------------------------------------
-- Include states in tags

--data TagPlus = T Tag | TS State | BOS | EOS deriving (Eq)
data TagPlus = T Tag | NoT Tag | TS State | NoTS State | BOS | EOS deriving (Eq)


stateToTag :: State -> TagPlus
stateToTag = TS  --Makes states also into special tags; 
                 --these will be inserted between each word.


alltps :: [TagPlus] --All normal tags as TagPlus, ie. no states nor BOS/EOS
alltps = map T alltags


instance Show TagPlus where
  show (T tag) = show tag
  show (TS s)  = "(s" ++ show s ++ ")"
  show (NoT tag) = "No" ++ show tag
  show (NoTS s)  = "(not_s" ++ show s ++ ")"
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
nc0  = NC 0
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
                      " IF " ++ unwords (map show ctxs) ++ " ;"


definitions :: [String]
definitions = "SET >>> = (>>>) ;":
              "SET <<< = (<<<) ;":
              [ "SET " ++ tag ++ " = (" ++ tagName ++ ") ;\n" ++
                "SET " ++ noTag ++ " = (" ++ noTagName ++ ") ;"
                | tag@(x:xs) <- map show alltags 
                , let tagName = toLower x:xs
                , let noTag = "No"++tag 
                , let noTagName = "not_"++tagName ] ++ 
              [ anySet ]
 where anySet = "SET Any = " ++ showTS alltps ++ " ;"



--------------------------------------------------------------------------------
-- Automaton to Rules

baseRules :: Automaton Tag -> [Rule]
baseRules a = [ R allButStart [bos]   --[noPrec] 
              , R allButEnd   [eos] ] --[noFoll] 
              ++ [ R onlyStart [hasPrec] | not (null onlyStart) ] 
              ++ [ R onlyEnd   [hasFoll] | not (null onlyEnd) ] 

 where
  allButStart = [ TS s | s <- [1..bound a] ] --all states excluding 0 
  allButEnd   = [ TS s | s <- [0..bound a]
                       , not $ final a s ]

  --if the start state has no transitions leading to it,
  --we can remove it from all but first (state) cohort
  onlyStart = [ TS 0 | noTransitionsTo a 0 ]

  --remove accepting & sink states from all but last (state) cohort
  onlyEnd = [ TS s | s <- [0..bound a]
                   , final a s 
                   , sink a s ]

  noPrec = No nc_1 alltps
  noFoll = No nc1  alltps
  hasPrec = Yes nc_1 alltps
  hasFoll = Yes nc1  alltps

  --alternative to noPrec and noFoll
  bos = Yes nc_1 [BOS]
  eos = Yes nc0  [EOS]

-----

-- Doesn't work properly if a sentence has more or less cohorts 
-- than the automaton needs transitions to reach accepting state.
-- "Doesn't work" means it still disambiguates, but nonsensically.

removeTag :: Automaton Tag -> Tag -> [Rule]
removeTag a t = ( R [NoT t] . context) `map` withSymbol a t 
 where 

  context :: (State,State) -> [Context]
  context (from,to) = [No nc_1 [NoTS from], No nc1 [NoTS to]]

--old version
--removeTag a t = [ R [T t] [c] | c <- contexts ]
-- where 
--  (statesFrom,statesTo) = unzip $ withSymbol a t       
--  contexts = [ No nc_1 (TS `map` nub statesFrom) | not (null statesFrom) ] ++
--             [ No nc1  (TS `map` nub statesTo) | not (null statesTo) ]

------

removeState :: Automaton Tag -> State -> [Rule]
removeState a s = [ R [TS s] (c:notEos)
                    | c@(No pos cs) <- contexts
                    , not $ null cs

                    --don't remove *final* state when not followed by something
                    , let notEos = [ No (NC 0) [EOS] 
                                     | final a s && pos == NC 1 ]

                    -- We don't need a similar check for initial state.
                    -- The rule `R allButStart [bos]' in BEFORE-SECTIONS 
                    -- will remove everything but s0 from the first state cohort.
                    -- A rule created here may target s0 in the first state cohort,
                    -- but it is protected by "don't remove the last reading".
                                 
                  ] 
 where
  (tagsFrom,_) = unzip $ fromState a s
  (_,tagsTo)   = unzip $ toState a s 
  contexts     = [ No nc1  (T `map` nub tagsFrom)
                 , No nc_1 (T `map` nub tagsTo) ]
                 


--------------------------------------------------------------------------------
-- General utilities

pr :: (Show a) => [a] -> String
pr = unlines . map show

compl :: (Bounded a, Eq a, Enum a) => [a] -> [a]
compl as = [minBound..maxBound] \\ as

--mergeFst [(a,1), (b,2), (c,1), (d,2)] = [([a,c],1),([b,d],2)] 
mergeFst :: (Ord b, Eq b) => [(a,[b])] -> [([a],[b])]
mergeFst a_bs = [ (as, bs) 
                  | as_bs <- groupBy (\(_,b) (_,b') -> b==b') $ sortOn snd a_bs -- :: [[(a,[b])]]
                  , let as = map fst as_bs
                  , let bs = snd $ head as_bs 
                ]
