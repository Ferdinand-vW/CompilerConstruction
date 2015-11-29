-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Picture
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- LaTeX pictures.
--
-------------------------------------------------------------------------------

module CCO.Picture (
    -- * Syntax
    Object (Line, Makebox, Framebox)    -- instances: Tree, Printable
  , Command (Put)                       -- instances: Tree, Printable
  , Commands
  , Picture (Picture)                   -- instances: Tree, Printable

    -- * Pretty printing  
) where

import CCO.Picture.AG
import CCO.Printing    (Printable (pp))

-------------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------------

instance Printable Object where
  pp obj = pp_Syn_Object (wrap_Object (sem_Object obj) Inh_Object)

instance Printable Command where
  pp cmd = pp_Syn_Command (wrap_Command (sem_Command cmd) Inh_Command)

instance Printable Picture where
  pp pict = pp_Syn_Picture (wrap_Picture (sem_Picture pict) Inh_Picture)