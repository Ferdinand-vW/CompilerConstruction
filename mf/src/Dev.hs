
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
import EmbellishedConstantPropagation

-- To make it all compile for the moment:


{-- How To Run (examples)

-- Strongly Live Variables
ghci> run slv "fib"

--}

run :: (Eq a, Show a) => (ProgramInfo -> IO (Analysis a)) -> String -> IO ()
run = runAnalysis'

-- run some analysis by passing an analysis function and a 'show' function to display the result
runAnalysis' :: (Eq a, Show a) => (ProgramInfo -> IO (Analysis a)) -> String -> IO ()
runAnalysis' analyze programName = do
  p <- parse programName
  putStrLn $ show $ p
  putStrLn "OUTPUT:"
  an <- analyze p
  putStrLn "Analysis:"
  mapM_ (\(k,(a,b)) -> putStrLn (show k) >> putStrLn ("Entry: " ++ show a) >> putStrLn ("Exit: " ++ show b) >> putStrLn "") (M.toList an)
  putStrLn "G'bye"

-- parse program

parse :: String -> IO ProgramInfo
parse programName = do
  let fileName = "../examples/"++programName++".c"
  content <- readFile fileName
  return . toProgramInfo . happy . alex $ content
