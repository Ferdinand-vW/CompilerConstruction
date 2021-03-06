module ConstantPropagation
(
cp,
Lattice(..),
LatticeVal(..),
joinOp,
setMeet,
parseExp
) where

import qualified Data.Map as M

import Analysis
import MonotoneFramework
import Administration


data LatticeVal a = Top | Bottom | Value a deriving (Show,Eq)
type Lattice a = M.Map Var (LatticeVal a)

cp :: ProgramInfo -> IO (Analysis (Lattice Int))
cp p = let join = joinOp
           lm = setMeet
           tfunc = transferFunction
           fl = Administration.flow p
           ifl = [] --this is the non-embellished version
           extrL = Administration.init p
           extrV = foldr (\x y -> M.insert x Top y) M.empty (vars p)
           mframe = MonotoneFramework join M.empty lm tfunc fl ifl extrL extrV --Create a MonotoneFramework instance
        in analyse mframe (blocks p) --Analyse the monotoneframework given the blocks

joinOp :: Lattice Int -> Lattice Int -> Lattice Int
joinOp lmap rmap = M.unionWith joinLattice lmap rmap

--Joins two lattices
joinLattice :: LatticeVal Int -> LatticeVal Int -> LatticeVal Int
joinLattice (Value x) (Value y) = if x /= y
                                  then Top
                                  else Value x
joinLattice _ _ = Top

--If the left Map is empty then it is the bottom, thus it is at least as precise as the right map
--Otherwise if foreach variable elements in the left map are at least a precise as the elements in the right map,
--only then is the left map at least as precise as the right map
setMeet :: Lattice Int -> Lattice Int-> Bool
setMeet lmap rmap
  | M.null lmap = True
  | M.null rmap = False
  | otherwise   = foldr (\x y -> lMeet (fst x) (snd x) && y) --Check if the elements in each tuple 'meet'
                  True 
                  (zip (M.elems lmap) (M.elems rmap)) --Create Tuples of all the elements in the maps

--In constant propagation a Value is exactly as precise as another Value
--A Value is also more precise than a Top
lMeet :: LatticeVal Int -> LatticeVal Int -> Bool
lMeet (Value x) (Value y) = x == y
lMeet _ Top = True
lMeet _ _ = False


--We do not care what the previous value was here so we forget it
--Just parse the expression, obviously only works for IAssign right now
transferFunction :: M.Map Label (Lattice Int) -> Block -> Label -> Lattice Int -> Lattice Int
transferFunction _ (B_IAssign var expr) _ st = M.adjust (\_ -> parseExp expr st) var st
transferFunction _ _ _ st = st

parseExp :: IExpr -> M.Map Var (LatticeVal Int) -> LatticeVal Int
parseExp (IConst x) _ = Value x
parseExp (Var x) st = llookup x st
parseExp (Plus l r) st = latticeOp (parseExp l st) (parseExp r st) (+)
parseExp (Minus l r) st = latticeOp (parseExp l st) (parseExp r st) (-)
parseExp (Times l r) st = latticeOp (parseExp l st) (parseExp r st) (*)
parseExp (Divide l r) st = latticeOp (parseExp l st) (parseExp r st) (quot)

--Used for computations on Lattices
latticeOp :: LatticeVal Int -> LatticeVal Int -> (Int -> Int -> Int) -> LatticeVal Int
latticeOp (Value a) (Value b) op = Value $ a `op` b
latticeOp _ _ _ = Top

--Do a lookup for a lattice value, if it doesn't exist then assume Top
llookup :: Var -> M.Map Var (LatticeVal Int) -> LatticeVal Int
llookup x st = case M.lookup x st of
                Nothing -> Top
                Just a -> a