
module Dev where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Char
import Data.Maybe

import Administration
import Lexer
import Main
import Parser

import Monotone.Analysis
import Monotone.ConstantPropagation (cp)
import Monotone.LiveVariableAnalysis (slv)
import Monotone.EmbellishedConstantPropagation (ecp)

import View.View
import View.PpAdministration
import View.PpLiveVariableAnalysis
import View.PpConstantPropagation
import View.PpEmbellishedConstantPropagation
import View.PpLiveVariableAnalysis

-- To make it all compile for the moment:


{-- How To Run (examples)

-- Strongly Live Variables
ghci> run slv "cp/fib"

--Constant Propagation
ghci> run cp "cp/fib"

--Embellished Constant Propagation
ghci> run (ecp 2) "cp/fib"

--}

run :: (Eq a, Show a, View a) => (ProgramInfo -> a) -> String -> IO ()
run = runAnalysis'

-- run some analysis by passing an analysis function and a 'show' function to display the result
runAnalysis' :: (Eq a, Show a, View a) => (ProgramInfo -> a) -> String -> IO ()
runAnalysis' analyze programName = do
  p <- parse programName
  putStrLn "OUTPUT:"
  let an = analyze p
  putStrLn $ view p
  putStrLn "Analysis:"
  putStrLn $ view an
  putStrLn "G'bye"

-- parse program

parse :: String -> IO ProgramInfo
parse programName = do
  let fileName = "../examples/" ++ programName ++ ".c"
  content <- readFile fileName
  return . toProgramInfo . happy . alex $ content
