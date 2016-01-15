module MonotoneFramework where

import qualified Data.Map as M
import Administration

type Exp = String
type TransferFunction a = Block -> a -> a

data Framework a =
    MonotoneFramework 
    {
        join :: a -> a -> a,
        bottom :: a,
        latticeMeet :: (a -> a -> Bool),
        transferFuncs :: TransferFunction a,
        flow :: [(Label,Label)],
        extremeLabel :: [Label],
        extremeValue :: a
    }