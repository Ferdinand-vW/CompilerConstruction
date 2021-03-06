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
import CCO.Feedback
import CCO.Printing

-- | Type checks a given Diag
checkTy :: Diag -> Feedback Diag
checkTy t = do let syn = wrap_Diag (sem_Diag t) (Inh_Diag Not_recursive)
               messages [Error (pp tyErr) | tyErr <- tyErrs_Syn_Diag syn]
               return t