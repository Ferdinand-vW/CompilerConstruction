module CCO.Picture.Base where

import CCO.Picture.AG
import CCO.Feedback
import CCO.Printing

eval :: Diag -> Feedback Picture
eval t = do
        return $ Picture (200,200) $ pic_Syn_Diag $ wrap_Diag (sem_Diag t) (Inh_Diag (0,200))

--inh_Diag :: Inh_Diag
--inh_Diag = Inh_Diag { prec_Inh_Diag = 0 }