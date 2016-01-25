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
    view xs = intercalate "," (S.toList xs)

instance View (Analysis (S.Set Var)) where
    view xs = header ++ M.foldrWithKey (\k (l,r) b -> show k ++ " " ++ 
                                                brackets (view r ++ spaces r) 
                                                ++ " <= " ++ 
                                                brackets (view l ++ spaces l) 
                                            ++ newLine ++ b ) "" xs
        where maxSize      = maximum $ map varLength $ map (S.toList . fst) (M.elems xs)
              varLength :: [String] -> Int
              varLength vars = (max 0 $ length vars - 1) + foldr (\x y -> length x + y) 0 vars
              numSpaces  n = maxSize - (varLength n)
              spaces set   = replicate (numSpaces (S.toList set)) ' '
              header       = "  Entry" ++ replicate (numSpaces []) ' ' ++ " " ++ "Exit\n"