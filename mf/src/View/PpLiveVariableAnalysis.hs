{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module View.PpLiveVariableAnalysis where


import Data.Set as S
import Data.Map as M


import View.View
import View.PpHelper
import Monotone.LiveVariableAnalysis
import Monotone.Analysis
import Administration

instance View (S.Set Var) where
    view xs = brackets $ S.foldr (\a b -> show a ++ "," ++ b) "" xs

instance View (Analysis (S.Set Var)) where
    view xs = let s = maxSizeVar xs
              in header s "Entry" "Exit" ++
                 M.foldrWithKey (\k v b -> (row k v s ++ b)) "" xs

header :: (Int,Int) -> String -> String -> String
header (ls,rs) lv rv = "|" ++ replicate 5 ' ' ++ "|"
                    ++ column (1 + rs - length rv) lv
                    ++ column (3 + ls - length lv) rv
                    ++ newLine
 
row :: Int -> (S.Set Var,  S.Set Var) -> (Int,Int) -> String
row l (lv,rv) (lm,rm) = "|" 
                 ++ column 4 (show l)
                 ++ column (rm - sizeVar rv) (view rv)
                 ++ column (lm - sizeVar lv) (view lv)
                 ++ newLine

column :: Int -> String -> String
column s w = w ++ replicate s ' ' ++ "|"


maxSizeVar :: Analysis (S.Set Var) -> (Int, Int)
maxSizeVar xs = (maximum lm,maximum rm)
            where (lm,rm) = unzip $ M.foldr (\(lv,rv) b -> (sizeVar lv,sizeVar rv) : b) [] $ xs

sizeVar :: S.Set Var -> Int
sizeVar xs = (S.size xs * 3) + (S.foldr (\a b -> length a + b ) 0 xs)