module Cohort where

import Automaton


printCohorts :: Int -> String
printCohorts bnd = unlines $ cohortS bnd : replicate bnd (cohortW tags ++ cohortS bnd)
 where tags = ["det", "adj", "noun", "verb"]

cohortW :: [String] -> String
cohortW as = "\n\"<w>\"\n" ++ cohort as

           
cohortS :: Int -> String
cohortS i = "\n\"<s>\"\n" ++ cohort [ "s" ++ show n | n <- [0..i] ]


cohort :: [String] -> String
cohort ss = unlines [ "   \""     ++ s ++ "\" "     ++ s | s <- ss ] 
--                    ++ [ "   \"not_" ++ s ++ "\" not_" ++ s | s <- ss ] )