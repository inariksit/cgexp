module Rule where

import Automaton
import Data.Char ( toLower )
import Text.Printf ( printf )
import Data.List ( intercalate, nub )

import Debug.Trace


toRules :: Automaton Tag -> String
toRules a = 
  unlines [ "DELIMITERS = \"<$.>\" \"<$?>\" \"<$!>\" \"<$:>\" \"<$\\;>\" ;"
          , "SET >>> = (>>>) ;"
          , "SET <<< = (<<<) ;"
          , ""
          , unlines [ printf "SET %s = (%s) ;" (t:ag) (toLower t:ag)
                        | (t:ag) <- map show (alpha a) ]
          , unlines [ printf "SET %s = (%s) ;" ('S':tate) ('s':tate)
                        | tate <- map show [0..bound a] ]
          , unlines tagTemplates
          , unlines stateTemplates
          , ""
          , "BEFORE-SECTIONS\n"
          , pr (baseRules a)
          , "SECTION\n"
          , "# Remove tags NOT between certain states"
          , pr tagRules         
          , "# Remove states between certain tags"
          , pr stateRules 
          ]
  where (tagRules,tagTemplates) = unzip (removeTag a `map` alpha a)
        (stateRules,stateTemplates) = unzip (removeState a `map` [0..bound a])

        pr :: (Show a) => [a] -> String
        pr = unlines . map show

--------------------------------------------------------------------------------
-- Include states in tags

--data TagPlus = T Tag | NoT Tag | TS State | NoTS State | BOS | EOS deriving (Eq)
data TagPlus = T Tag | TS State | BOS | EOS deriving (Eq)


instance Show TagPlus where
  show (T tag) = show tag
  show (TS s)  = "S" ++ show s
  --show (NoT tag) = "No" ++ show tag
  --show (NoTS s)  = "(not_s" ++ show s ++ ")"
  show BOS     = ">>>"
  show EOS     = "<<<"

--------------------------------------------------------------------------------
-- Rules, contexts & positions

newtype OrList a = OrList { getOrList :: [a] } deriving (Eq)
newtype AndList a = AndList { getAndList :: [a] } deriving (Eq)

data Rule = R { target  :: OrList TagPlus
              , context :: AndList Context } deriving (Eq) 

type Name = String

data Context = Yes Position (OrList TagPlus)
                | No Position (OrList TagPlus)
                | Templ Name --this is kinda crap, I'm generating the content in removeTag function and throwing it away
                | NegTempl Name deriving (Eq)

data Position = C Int | NC Int deriving (Eq)

-- Shorthands, we'll write these a lot
nc0  = NC 0
c1   = C 1
c_1  = C (-1)
nc1  = NC 1
nc_1 = NC (-1)


instance Show Context where
  show (Yes pos ts) = printf     "(%s %s)" (show pos) (show ts)
  show (No pos ts)  = printf "(NOT %s %s)" (show pos) (show ts)
  show (Templ name) = "(T:" ++ name ++ ")"
  show (NegTempl name) = "(NEGATE T:" ++ name ++ ")"

instance Show Position where
  show (C i)  = show i ++ "C"
  show (NC i) = show i
 
instance Show Rule where
  show (R trg ctxs) = printf "REMOVE %s IF %s ;" (show trg) (show ctxs)

instance (Show a) => Show (AndList a) where
  show = filter (/='"') . unwords . map show . getAndList

instance (Show a) => Show (OrList a) where
  show = filter (/='"') . intercalate " or " . map show . getOrList


--------------------------------------------------------------------------------
-- Automaton to Rules

baseRules :: Automaton Tag -> [Rule]
baseRules a = [ R (OrList allButStart) bos  
              , R (OrList allButEnd)   eos ]
              ++ [ R (OrList onlyStart) hasPrec | not (null onlyStart) ] 
              ++ [ R (OrList onlyEnd)   hasFoll | not (null onlyEnd) ] 

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

  hasPrec = AndList [Yes nc_1 (alpha' a)]
  hasFoll = AndList [Yes nc1  (alpha' a)]
  bos = AndList [Yes nc_1 (OrList [BOS])]
  eos = AndList [Yes nc0  (OrList [EOS])]

  alpha' = OrList . map T . alpha

-----

removeTag :: Automaton Tag -> Tag -> (Rule,String)
removeTag a t = (R target templ, templString)
 where 
  target = OrList [T t]
  templ = AndList [NegTempl templLhs]

  froms_tos = withSymbol a t

  rhs :: (State,State) -> String
  rhs (f,t) = printf "(-1 %s LINK 2 %s)" (showTS f) (showTS t)

  templLhs = show t ++ "Ctx"
  templRhs = showOr (map rhs froms_tos)
  templString = printf "TEMPLATE %s = ( %s ) ;" templLhs templRhs

  showOr = show . OrList
  showTS = show . TS

-----

removeState :: Automaton Tag -> State -> (Rule,String)
removeState a s = (R target templ, templString)
 where
  target = OrList [TS s]
  templ = AndList [NegTempl templLhs]

  froms_tos = withState a s

  rhs :: ([Tag], [Tag]) -> String
  rhs ([],[]) = "(0 (*))" --something trivial to make it not crash
  rhs (fs,[]) = printf "(-1 %s)" (showOr fs)
  rhs ([],ts) = printf "(1 %s)"  (showOr ts)
  rhs (fs,ts) = printf "(-1 %s LINK 2 %s)" (showOr fs) (showOr ts)

  templLhs = show (TS s) ++ "Ctx"
  templRhs = rhs froms_tos
  templString = printf "TEMPLATE %s = ( %s ) ;" templLhs templRhs

  showOr = show . OrList