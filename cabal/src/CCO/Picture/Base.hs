module CCO.Picture.Base where

import CCO.Picture.AG
import CCO.Feedback
import CCO.Printing

eval :: Diag -> Feedback Picture
eval t = do
        return $ pic_Syn_Pic $ wrap_Pic (sem_Pic (Pic t)) Inh_Pic

--inh_Diag :: Inh_Diag
--inh_Diag = Inh_Diag { prec_Inh_Diag = 0 }