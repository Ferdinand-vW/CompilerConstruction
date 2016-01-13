module MonotoneFramework where

import qualified Data.Map as M
import AttributeGrammar

type Label = Int
data Lattice a = Top | Bottom | Value a
type Var = String
data Block = Block Label Var Exp
type Exp = String
type TransferFunction a = Stat' -> a -> a

data Framework a =
    MonotoneFramework 
    {
        latticeMeet :: (a -> a -> Bool),
        transferFuncs :: TransferFunction a,
        flow :: [(Label,Label)],
        extremeLabel :: Label,
        extremeValue :: a
    }