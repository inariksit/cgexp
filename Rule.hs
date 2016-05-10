module Rule where

import Automaton
import Data.List
import Control.Monad ( forM_ )


test :: IO ()
test = [0..2] `forM_` \i -> mapM_ print (removeTagFrom detAdjNoun i)


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


data Context = Yes Int [TagPlus] | No Int [TagPlus] deriving (Eq)

instance Show Context where
  show (Yes i ts) = "(" ++ show i ++ " " ++ show ts ++ ")"
  show (No i ts)  = "(NOT " ++ show i ++ " " ++ show ts ++ ")"

instance Show Rule where
  show (R trg ctxs) = "REMOVE " ++ show trg ++ " IF " ++ (unwords $ map show ctxs)



--------------------------------------------------------------------------------

removeTagFrom :: Automaton Tag -> State -> [Rule]
removeTagFrom a s = never:sometimes
 where
  fromS = fromState a s :: [(Tag,State)]

  --If there is a transition with Det from 0, allow that but remove Det from everywhere else
  sometimes = [ R [T tag] [beforeCtx,afterCtx]
                   | (tag,s') <- fromS
                   , let beforeCtx = Yes (-1) [stateToTag s] 
                   , let afterCtx = No 1 [stateToTag s'] ]


  --If there is no transition with Noun from 0, remove all Nouns from 0.
  --May not be strictly needed if we go for actual sentences, not symbolic?
  never = R trg [ctx]
    where (tags,_) = unzip fromS  --transitions that happen to *some* state
          trg = map T $ compl tags  --complement: transitions that never happen
          ctx = Yes (-1) [stateToTag s]




--removeState :: Automaton a -> State -> Rule