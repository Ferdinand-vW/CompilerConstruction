
module Dev where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Char
import Data.Maybe

import Administration
import Lexer
import Main
import Parser
import Analysis
import ConstantPropagation
import LiveVariableAnalysis
import PpAnalyse

-- To make it all compile for the moment:


{-- How To Run (examples)

-- Strongly Live Variables
ghci> run slv "fib"

--}
--data Lattice a = Top | Bottom | Value a deriving (E/q)
run :: (Eq a, Show a, View a) => (ProgramInfo -> IO a) -> String -> IO ()
run = runAnalysis'

-- run some analysis by passing an analysis function and a 'show' function to display the result
runAnalysis' :: (Eq a, Show a, View a) => (ProgramInfo -> IO a) -> String -> IO ()
runAnalysis' analyze programName = do
  p <- parse programName
  putStrLn "OUTPUT:"
  an <- analyze p
  putStrLn $ show $ p
  putStrLn "Analysis:"
  putStrLn $ view $ an
  putStrLn "G'bye"

-- parse program

parse :: String -> IO ProgramInfo
parse programName = do
  let fileName = "../examples/"++programName++".c"
  content <- readFile fileName
  return . toProgramInfo . happy . alex $ content



--data ProgramInfo = ProgramInfo {blocks :: M.Map Label Block, init :: [Label], finals :: [Label], flow :: Flow, vars :: [Var]}

--Pretty printing
--ppProcs :: Procs' -> String
--ppProcs xs = foldr (\a b -> ppProc a ++ b) "" xs
--prettyprint
--ppProc :: Proc' -> String
--ppProc (Proc' le lr n i o s) = newLine ++ "Proc:" ++ show le ++ "," ++ show lr  ++ newLine ++ ppStat s
--Pretty print the stat type
--ppStat :: Stat -> String
--ppStat Skip = "Skip"
--ppStat (IfThenElse s1 s2) = "If condition" ++ newLine ++ "then" ++ show l ++ ppStat s1 ++ "else" ++ ppStat s2
--ppStat (While c s)          = newLine ++ "While" ++ show l ++ ppStat s
----ppStat (Call lc le n p o)     = newLine ++ "Call' " ++ "("++ show lc ++ "," ++ show le ++  ")"
--ppStat (IAssign n v)        = newLine ++ "IAssign" ++ show l
--ppStat (BAssign n v)        = newLine ++ "BAssign" ++ show l
--ppStat (Seq s1 s2)            = ppStat s1 ++ ppStat s1
--ppStat (Malloc l n s)         = newLine ++ "Malloc" ++ show l
--ppStat (Free l p)             = newLine ++ "Free" ++ show l
--ppStat (RefAssign l p v)      = newLine ++ "RefAssign" ++ show l
--ppStat (Continue l)           = newLine ++ "Continue" ++ show l
--ppStat (Break l)              = newLine ++ "Break" ++ show l

