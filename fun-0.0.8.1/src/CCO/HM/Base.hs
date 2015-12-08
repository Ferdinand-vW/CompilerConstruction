-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.HM.Base
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A simple, implicitly typed functional language.
--
-------------------------------------------------------------------------------
{-
module CCO.HM.Base (
    -- * Syntax
    Var                                 -- = String
  , Tm (Tm)                             -- instances: Tree
  , Tm_ (HVar, HNat, HLam, HApp, HLet)       -- instances: Tree
) where

import CCO.HM.AG
import CCO.Tree                   (Tree (fromTree, toTree))
import qualified CCO.Tree as T    (ATerm (App))
import CCO.Tree.Parser            (parseTree, app, arg)
import Control.Applicative        (Applicative ((<*>)), (<$>))

-------------------------------------------------------------------------------
-- Tree instances
-------------------------------------------------------------------------------

-}