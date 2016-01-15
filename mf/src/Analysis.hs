module Analysis where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Char
import Data.Maybe

import MonotoneFramework
import Administration

type Analysis a = a

--Analyse a Program (M.Map Label Stat') using a MonotoneFramework
--Return a Analysis that currently only works for ConstantPropagation
analyse :: Framework (M.Map Var (Lattice Int)) -> M.Map Label Block -> IO (Analysis (M.Map Label (M.Map Var (Lattice Int))))
analyse (MonotoneFramework lmeet tf fl el ev) bl = loop fl array'
  where array = foldr (\x y -> M.insert x M.empty y) M.empty (M.keys bl) --Create an empty Map for each Label
        array' = M.adjust (\x -> ev) el array --Insert the extreme value into the extreme label
        loop :: [(Label,Label)] -> M.Map Label (M.Map Var (Lattice Int)) -> IO (M.Map Label (M.Map Var (Lattice Int)))
        loop [] arr = return arr
        loop ((l,l'):xs) arr = do
          --We lookup the block for label l and it's current state of the variables
          --Then this is passed to the transfer function, which result is used to check if it is not more precise
          --than the state of label l'
              print $ "Current transfer:" ++ show (l,l')
              print $ "others:" ++ show xs
              print $ "state of l:" ++ show (slookup l arr)
              print $ "state of l':" ++ show (slookup l' arr)
              print $ "transferFunction on l:" ++ show (tf (slookup l bl) (slookup l arr))
              print $ "the lmeet: " ++ show (lmeet (tf (slookup l bl) $ slookup l arr) (slookup l' arr))
              if not $ lmeet (tf (slookup l bl) $ slookup l arr) (slookup l' arr)
                --If it is not more precise
                --Add the above union to the array
                then let arr' = M.adjust (\x -> M.unionWith joinOp x (tf (slookup l bl) (slookup l arr))) l' arr
                         w = updateWorkSet l' xs fl --Then add all Tuples in the Flow that start with label l' to the current workset
                      in
                      do
                        print $ "New state l':" ++ show (slookup l' arr')
                        print "-----------------------------------------------------------------------------"
                        loop w arr' --recurse
                else do
                  print "-----------------------------------------------------------------------------"
                  loop xs arr --recurse
        updateWorkSet :: Eq a => a -> [(a,b)] -> [(a,b)] -> [(a,b)]
        updateWorkSet l w [] = w
        updateWorkSet l w (x:xs)
          | l == fst x = updateWorkSet l (x : w) xs
          | otherwise  = updateWorkSet l w xs

--We can assume that a Label always exists in a Map, otherwise
--its a coding error
slookup :: Ord a => a -> M.Map a b -> b
slookup l stats = fromJust $ M.lookup l stats

--Joins two lattices
joinOp :: Lattice Int -> Lattice Int -> Lattice Int
joinOp (Value x) (Value y) = if x /= y
                                  then Top
                                  else Value x
joinOp _ _ = Top