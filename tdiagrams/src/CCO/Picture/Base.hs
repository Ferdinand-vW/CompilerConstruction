module CCO.Picture.Base where

import CCO.Picture.AG
import CCO.Feedback
import CCO.Printing

translate :: Diag -> Feedback Picture
translate d = do
        return $ pic_Syn_Pic $ wrap_Pic (sem_Pic (Pic d)) Inh_Pic