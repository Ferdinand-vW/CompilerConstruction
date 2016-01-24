{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module View.PpLiveVariableAnalysis where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List

import Administration
import Monotone.Analysis
import View.View
import View.PpHelper

instance View (S.Set Var) where
    view xs = brackets $ intercalate "," (S.toList xs)



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