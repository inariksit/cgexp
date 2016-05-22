module Rule where

import Automaton
import Data.Char
import Data.List
import Control.Monad ( forM_ )

main = test detAdjNoun

test :: Automaton Tag -> IO ()
test a = do mapM_ putStrLn definitions

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
definitions = "SET >>> = (>>>) ;":"SET <<< = (<<<) ;":
              [ "SET " ++ (x:xs) ++ " = (" ++ (toLower x:xs) ++ ") ;" 
                | (x:xs) <- map show tags ]
 where tags = [minBound..maxBound] :: [Tag]


baseRules :: Automaton Tag -> [Rule]
baseRules a = [ R allButStart [noPrec] 
              , R allButEnd   [noFoll] 
              , R onlyStart   [hasPrec] 
              , R end         [hasFoll] ] 

 where 

  alltags = [ T t | t <- [minBound..maxBound] :: [Tag] ]
  allButStart = [ TS s | s <- [1..bound a] ] --all states excluding 0 
  allButEnd   = [ TS s | s <- [0..bound a]
                       , not $ final a s ]

  onlyStart --remove start state if it has a preceding cohort;
            --except if start state is also an accepting state, then keep it
    = [ TS s | s <- [0] \\ --all states excluding 0 
                    if final a 0 then [0] else [] ] 
  end = [ TS s | s <- [0..bound a]
               , final a s ]

  noPrec = No (NC (-1)) alltags
  noFoll = No (NC  1  ) alltags
  hasPrec = Yes (NC (-1)) alltags
  hasFoll = Yes (NC   1 ) alltags

  bos = Yes (NC (-1)) [BOS]
  eos = Yes (NC   1)  [EOS]

--------------------------------------------------------------------------------

removeTagFrom :: Automaton Tag -> State -> [Rule]
removeTagFrom a s = neverFrom:sometimesFrom
 where
  fromS = fromState a s :: [(Tag,State)]


  -- If there is a transition with Det from 0, 
  -- allow that but remove Det from everywhere else.
  sometimesFrom = [ R [T tag] [beforeCtx, afterCtx]
                    | tags_ss <- groupBy (\(a,_) (a',_) -> a==a') fromS
                    , let (tags,ss') = unzip tags_ss
                    , let tag = head tags
                    , let beforeCtx = Yes (C (-1)) [stateToTag s] 
                    , let afterCtx = No (NC 1) (map stateToTag ss') ]


  --If there is no transition with Noun from/to 0, remove all Nouns from/to 0.
  --May not be strictly needed if we go for actual sentences, not symbolic?
  neverFrom = R trgFrom [ctxFrom]
    where (tagsFrom,_) = unzip fromS  --transitions that happen *to* some state
          trgFrom = map T $ compl tagsFrom  --complement: transitions that never happen
          ctxFrom = Yes (C (-1)) [stateToTag s]



removeTagTo :: Automaton Tag -> State -> [Rule]
removeTagTo a s = neverTo:sometimesTo
 where
  toS = toState a s :: [(State,Tag)]

  sometimesTo = [ R [T tag] [afterCtx, beforeCtx]
                  | (s',tag) <- toS
                  , let afterCtx = Yes (C 1) [stateToTag s]
                  , let beforeCtx = No (NC (-1)) [stateToTag s'] ]

  neverTo = R trgTo [ctxTo]
   where (_,tagsTo) = unzip toS  --transitions that happen *from* some state
         trgTo      = map T $ compl tagsTo
         ctxTo      = Yes (C 1) [stateToTag s]

--------------------------------------------------------------------------------

removeState :: Automaton Tag -> State -> [Rule]
removeState a s = [ R [TS s] [c] 
                    | c@(No _ (x:xs)) <- contexts] --only non-empty ctxs
 where
  (tagsFrom,_) = unzip $ fromState a s
  (_,tagsTo)   = unzip $ toState a s 
  contexts     = [ No (NC 1) (map T $ nub tagsFrom) 
                 , No (NC (-1)) (map T $ nub tagsTo) ] 
         


