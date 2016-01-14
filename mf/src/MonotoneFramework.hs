module MonotoneFramework where

import qualified Data.Map as M
import Administration

data Lattice a = Top | Bottom | Value a deriving (Eq,Show)
type Exp = String
type TransferFunction a = Block -> a -> a

data Framework a =
    MonotoneFramework 
    {
        latticeMeet :: (a -> a -> Bool),
        transferFuncs :: TransferFunction a,
        flow :: [(Label,Label)],
        extremeLabel :: Label,
        extremeValue :: a
    }