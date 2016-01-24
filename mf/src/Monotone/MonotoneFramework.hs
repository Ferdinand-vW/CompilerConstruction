module Monotone.MonotoneFramework where

import qualified Data.Map as M
import Administration

--We had to add the Map to the transfer function to allow for the return
--of embellished constantpropagation to be binary
type TransferFunction a = M.Map Label a -> Block -> Label -> a -> a

data Framework a =
    MonotoneFramework 
    {
        join :: a -> a -> a,
        bottom :: a,
        latticeMeet :: (a -> a -> Bool),
        transferFuncs :: TransferFunction a,
        flow :: [(Label,Label)],
        interflow :: [(Label,Label,Label,Label)],
        extremeLabel :: [Label],
        extremeValue :: a
    }