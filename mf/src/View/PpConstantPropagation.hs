{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module View.PpConstantPropagation where

import qualified Data.Map as M
import Data.List

import View.View
import View.PpHelper
import Monotone.ConstantPropagation
import Monotone.Analysis (Analysis)

instance View (LatticeVal Int) where 
    view Top = "T"
    view (Value a) = show a

instance View (Lattice Int) where
    view xs = brackets $ intercalate "," $ map (\(k,l) -> k ++ " => " ++ view l) (M.toList xs)


instance (View a) => View (Analysis a) where
    view xs =  M.foldrWithKey (\k (l,r) b -> show k ++ view l ++ " => " ++ view r ++ newLine ++ b ) "" xs