module Main where

import Automaton 
import Rule ( toRules )
import Arbitrary ( randomAutomaton )
import Cohort ( printCohorts )

import System.Environment ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  case args of 
    ["detNounV"] -> do 
                     let rules = toRules detNounVerb
                     writeFile "examples/detNounV/detNounV.rlx" rules
                     putStrLn rules

    ["detAdjN"]  -> do
                      let rules = toRules detAdjNoun
                      writeFile "examples/detAdjN/detAdjN.rlx" rules
                      putStrLn rules

    ["random"]   -> do a <- randomAutomaton
                       print a
                       --putStrLn $ printRules a

                       let n = show (bound a)
                       writeFile "examples/random/random.rlx" (toRules a)
                       writeFile ("examples/random/ex-random-" ++ n ++ ".txt")
                                 (printCohorts $ bound a)
    _            -> mapM_ print [detAdjNoun, detNounVerb]
