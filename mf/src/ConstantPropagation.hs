module ConstantPropagation
(
cp
)
where

import qualified Data.Map as M

import Analysis
import MonotoneFramework
import Administration

--data ProgramI = ProgramInfo {initl :: Label, finals :: [Label], flow' :: [(Label,Label)], blcks :: M.Map Label Stat', vrs :: [Var]}

cp :: ProgramInfo -> Analysis (M.Map Label (M.Map Var (Lattice Int)))
cp p = let lm = setMeet
           tfunc = transferFunction
           fl = Administration.flow p
           extrL = Administration.init p
           extrV = foldr (\x y -> M.insert x Top y) M.empty (vars p)
           mframe = MonotoneFramework lm tfunc fl extrL extrV --Create a MonotoneFramework instance
        in analyse mframe (blocks p) --Analyse the monotoneframework given the blocks

--If the left Map is empty then it is the bottom, thus it is at least as precise as the right map
--Otherwise if foreach variable elements in the left map are at least a precise as the elements in the right map,
--only then is the left map at least as precise as the right map
setMeet :: M.Map Var (Lattice Int) -> M.Map Var (Lattice Int) -> Bool
setMeet lmap rmap
  | M.null lmap = True
  | M.null rmap = False
  | otherwise   = foldr (\x y -> lMeet (fst x) (snd x) && y) --Check if the elements in each tuple 'meet'
                  True 
                  (zip (M.elems lmap) (M.elems rmap)) --Create Tuples of all the elements in the maps

--In constant propagation a Value is exactly as precise as another Value
--A Value is also more precise than a Top
lMeet :: Lattice Int -> Lattice Int -> Bool
lMeet (Value x) (Value y) = True
lMeet _ Top = True
lMeet _ _ = False


--We do not care what the previous value was here so we forget it
--Just parse the expression, obviously only works for IAssign right now
transferFunction :: Block -> M.Map Var (Lattice Int) -> M.Map Var (Lattice Int)
transferFunction (B_IAssign var expr) st = M.adjust (\_ -> parseExp expr st) var st

parseExp :: IExpr -> M.Map Var (Lattice Int) -> Lattice Int
parseExp (IConst x) _ = Value x
parseExp (Var x) st = llookup x st
parseExp (Plus l r) st = latticeOp (parseExp l st) (parseExp r st) (+)
parseExp (Minus l r) st = latticeOp (parseExp l st) (parseExp r st) (-)
parseExp (Times l r) st = latticeOp (parseExp l st) (parseExp r st) (*)
parseExp (Divide l r) st = latticeOp (parseExp l st) (parseExp r st) (quot)

--Used for computations on Lattices
latticeOp :: Lattice Int -> Lattice Int -> (Int -> Int -> Int) -> Lattice Int
latticeOp (Value a) (Value b) op = Value $ a `op` b
latticeOp _ _ _ = Top

--Do a lookup for a lattice value, if it doesn't exist then assume Top
llookup :: Var -> M.Map Var (Lattice Int) -> Lattice Int
llookup x st = case M.lookup x st of
                Nothing -> Top
                Just a -> a

