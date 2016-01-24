{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module View.PpAnalysis where

import qualified Data.Map as M

import View.View
import View.PpHelper
import Analysis (Analysis)

instance (View a) => View (Analysis a) where
    view xs =  M.foldrWithKey (\k (l,r) b -> show k ++ view l ++ " => " ++ view r ++ newLine ++ b ) "" xs