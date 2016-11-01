module Rule where

import Automaton
import Cohort (printCohorts)
import Test
import Data.Char ( toLower )
import Text.Printf (printf)
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
  unlines [ "DELIMITERS = \"<$.>\" \"<$?>\" \"<$!>\" \"<$:>\" \"<$\\;>\" ;"
          , "SET >>> = (>>>) ;"
          , "SET <<< = (<<<) ;"
          , ""
          , unlines [ printf "SET %s = (%s) ;" (t:ag) (toLower t:ag)
                        | (t:ag) <- map show alltags ]
          , unlines [ printf "SET %s = (%s) ;" ('S':tate) ('s':tate)
                        | tate <- map show [0..bound a] ]
          , unlines templates
          , ""
          , "BEFORE-SECTIONS\n"
          , pr (baseRules a)
          , "SECTION\n"
          , "# Remove tags NOT between certain states"
--          , concatMap (pr . removeTag a) alltags
          , pr rmTagRules         
          , "# Remove states between certain tags"
          , concatMap (pr . removeState a) [0..bound a]
          ]
  where (rmTagRules,templates) = unzip (removeTag a `map` alltags)


--------------------------------------------------------------------------------
-- Include states in tags

--data TagPlus = T Tag | NoT Tag | TS State | NoTS State | BOS | EOS deriving (Eq)
data TagPlus = T Tag | TS State | BOS | EOS deriving (Eq)


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

type Name = String

data Context = Yes Position [TagPlus] | No Position [TagPlus] 
                | Templ Name [[Context]] | NegTempl Name [[Context]] deriving (Eq)

data Position = C Int | NC Int deriving (Eq)

-- Shorthands, we'll write these a lot
nc0  = NC 0
c1   = C 1
c_1  = C (-1)
nc1  = NC 1
nc_1 = NC (-1)


instance Show Context where
  show (Yes pos ts) = printf     "(%s %s)" (show pos) (showOr ts)
  show (No pos ts)  = printf "(NOT %s %s)" (show pos) (showOr ts)
  show (Templ name _cs) = "(T:" ++ name ++ ")"
  show (NegTempl name _cs) = "(NEGATE T:" ++ name ++ ")"

instance Show Position where
  show (C i)  = show i ++ "C"
  show (NC i) = show i
 
instance Show Rule where
  show (R trg ctxs) = "REMOVE " ++ showOr trg ++ 
                           " IF " ++ showAnd ctxs ++ " ;"

showAnd :: (Show a) => [a] -> String
showAnd = filter (/='"')  . unwords . map show

showOr :: (Show a) => [a] -> String
showOr = filter (/='"') . intercalate " or " . map show 

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

removeTag :: Automaton Tag -> Tag -> (Rule,String)
removeTag a t = (R [T t] [templ], templString)
 where   
  context :: (State,State) -> [[Context]]
  context (from,to) = [[ Yes nc_1 [TS from], Yes nc1 [TS to] ]]

  templ = NegTempl templLhs (context `concatMap` withSymbol a t)
  templLhs = show t ++ "Ctx"
  templRhs = showOr [ "(-1 " ++ show (TS from) ++ " LINK 2 " ++ show (TS to) ++ ")" 
                        | (from,to) <- withSymbol a t]

  templString = printf "TEMPLATE %s = ( %s ) ;" templLhs templRhs


-----

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
