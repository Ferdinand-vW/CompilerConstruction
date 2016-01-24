{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module View.PpEmbellishedConstantPropagation where

import qualified Data.Map as M
import Data.List

import EmbellishedConstantPropagation
import View.View
import View.PpHelper
import View.PpConstantPropagation

instance View (ContextLattice Int) where
    view ctxs = "\n" ++ brackets (intercalate "," (map (\(ctx,l) -> "\\" ++ show ctx ++ extraSpaces maxSize ctx
                ++ " -> " ++ view l ++ "\n") (M.toList ctxs))) ++ "\n"
        where maxSize = maximum $ map contextSize (M.keys ctxs)

extraSpaces :: Int -> Context -> String
extraSpaces max ctx = replicate (max - (contextSize ctx)) ' '

contextSize :: Context -> Int
contextSize [] = 2 --Count the brackets
contextSize (x:xs) = x `div` 10 + 2 + contextSize xs