{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module View.PpConstantPropagation where

import qualified Data.Map as M
import Data.List

import View.View
import View.PpHelper
import ConstantPropagation

instance View (LatticeVal Int) where 
    view Top = "T"
    view Bottom = "_"
    view (Value a) = show a

instance View (Lattice Int) where
    view xs = brackets $ intercalate "," $ map (\(k,l) -> k ++ " => " ++ view l) (M.toList xs)