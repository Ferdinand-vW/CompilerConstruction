-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Diag
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

module CCO.Diag (
    -- * Syntax
    Ident
  , Diag (Diag)    -- instances: Tree
  , Diag_ (..)     -- instances: Tree

    -- * Parser
  , parser         -- :: Component String Diag
) where

import CCO.Diag.Base      (Ident, Diag (Diag), Diag_ (..))
import CCO.Diag.Parser    (parser)