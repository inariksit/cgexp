module Rule where

import Automaton
import Data.List
import Control.Monad ( forM_ )


test :: Automaton Tag -> IO ()
test a = do mapM_ print (defRules a)
            putStrLn ""
            [0..bound a] `forM_` \i -> mapM_ print (removeTagFrom a i)
            [1..bound a] `forM_` \i -> mapM_ print (removeTagTo a i)



--------------------------------------------------------------------------------
-- Ugly hacks

data TagPlus = T Tag | TS State deriving (Eq)

instance Show TagPlus where
  show (T tag) = show tag
  show (TS s)  = "s" ++ show s

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
  show (R trg ctxs) = "REMOVE " ++ showTS trg ++ " IF " ++ (unwords $ map show ctxs) ++ " ;"


showTS :: [TagPlus] -> String
showTS  = intercalate " OR " . map show

--------------------------------------------------------------------------------

defRules :: Automaton Tag -> [Rule]
defRules a = [ R allButStart [nothingInPrevious]
             , R allButEnd   [nothingInFollowing] ] 
 where 
  allButStart = [ TS s | s <- [1..bound a] ]  --all states excluding 0
  allButEnd   = [ TS s | s <- [0..bound a]
                       , not $ final a s ]
  nothingInPrevious = No (NC (-1)) [ T t | t <- [minBound..maxBound] :: [Tag] ]
  nothingInFollowing = No (NC  1) [ T t | t <- [minBound..maxBound] :: [Tag] ]

--------------------------------------------------------------------------------

removeTagFrom :: Automaton Tag -> State -> [Rule]
removeTagFrom a s = neverFrom:sometimesFrom
 where
  fromS = fromState a s :: [(Tag,State)]


  -- If there is a transition with Det from 0, 
  -- allow that but remove Det from everywhere else.
  sometimesFrom = [ R [T tag] [beforeCtx, afterCtx]
                    | (tag,s') <- fromS
                    , let beforeCtx = Yes (C (-1)) [stateToTag s] 
                    , let afterCtx = No (NC 1) [stateToTag s'] ]



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

removeState :: Automaton a -> State -> Rule
removeState = undefined



