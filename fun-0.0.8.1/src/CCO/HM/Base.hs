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

module CCO.HM.Base (
    toANormal
) where
import CCO.HM.AG
import CCO.Feedback

-------------------------------------------------------------------------------
-- Tree instances
-------------------------------------------------------------------------------


--toANormal :: Tm -> Feedback Tm
--toANormal tm = return $ Core_Syn_Tm $ wrap_Tm (sem_Tm tm) (Inh_Core)

toANormal :: Tm -> Feedback Tm
toANormal tm = return $ ntm_Syn_Tm $ wrap_Tm (sem_Tm tm) Inh_Tm

--hm2cr :: Tm -> Feedback Mod
--hm2cr tm = return $ core_Syn_Core $ wrap_Core (sem_Core (Core tm)) (Inh_Core)
