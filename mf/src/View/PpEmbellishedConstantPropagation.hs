{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module View.PpEmbellishedConstantPropagation where

import qualified Data.Map as M
import Data.List

import Monotone.EmbellishedConstantPropagation
import Monotone.Analysis

import View.View
import View.PpHelper
import View.PpConstantPropagation

instance View (ContextLattice Int) where
    view ctxs = "\n" ++ brackets (intercalate "," (map (\(ctx,l) -> "\\" ++ show ctx ++ extraSpaces maxSize ctx
                ++ " -> " ++ view l ++ "\n") (M.toList ctxs))) ++ "\n"
        where maxSize = maximum $ map contextSize (M.keys ctxs)

instance View (Analysis (ContextLattice Int)) where
    view xs =  M.foldrWithKey (\k (l,r) b -> show k ++ view l ++ " => " ++ view r ++ newLine ++ b ) "" xs

extraSpaces :: Int -> Context -> String
extraSpaces max ctx = replicate (max - (contextSize ctx)) ' '

contextSize :: Context -> Int
contextSize [] = 2 --Count the brackets
contextSize (x:xs) = x `div` 10 + 2 + contextSize xs