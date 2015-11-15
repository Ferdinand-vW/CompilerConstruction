-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Diag.Base
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- T-diagrams.
--
-------------------------------------------------------------------------------

module CCO.Diag.Base (
    -- * Syntax
    Ident
  , Diag (Diag)    -- instances: Tree
  , Diag_ (..)     -- instances: Tree
  , checkTy
) where

import CCO.Diag.AG