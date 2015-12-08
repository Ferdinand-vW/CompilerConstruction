-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.HM
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

module CCO.HM (
    -- * Syntax

    -- * Parser
  parser,                      -- :: Component String Tm
  toANormal
) where

import CCO.HM.Parser    (parser)
import CCO.HM.Base      (toANormal)