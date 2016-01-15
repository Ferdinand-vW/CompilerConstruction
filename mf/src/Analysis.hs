module Analysis where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Data.Char
import Data.Maybe

import MonotoneFramework
import Administration

--First element is the context
--second element is the effect on the context (transferfunction)
type Analysis a = M.Map Label (a,a)

--Analyse a Program (M.Map Label Stat') using a MonotoneFramework
--Return a Analysis that currently only works for ConstantPropagation
analyse :: Show a => Framework a -> M.Map Label Block -> IO (Analysis a)
analyse (MonotoneFramework join btm lmeet tf fl el ev) bl = do
                                                              anl <- loop fl array'
                                                              return $ finalize anl
  where array = foldr (\x y -> M.insert x btm y) M.empty (M.keys bl) --Create an empty Map for each Label
        array' = foldr (\x y -> M.adjust (\_ -> ev) x y) array el --Insert the extreme value into the extreme label
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
              print $ "kills: " ++ show (kill (slookup l bl))
              print $ "gens: " ++ show (gen (slookup l bl))
              if not $ lmeet (tf (slookup l bl) $ slookup l arr) (slookup l' arr)
                --If it is not more precise
                --Add the above union to the array
                then let arr' = M.adjust (\x -> join x (tf (slookup l bl) (slookup l arr))) l' arr
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
        finalize arr = M.mapWithKey (\k a -> (a, tf (slookup k bl) a)) arr

--We can assume that a Label always exists in a Map, otherwise
--its a coding error
slookup :: Ord a => a -> M.Map a b -> b
slookup l stats = fromJust $ M.lookup l stats

kill :: Block -> S.Set Var
kill (B_IAssign n _) = S.singleton n
kill (B_BAssign n _) = S.singleton n
kill _ = S.empty

gen :: Block -> S.Set Var
gen (B_IAssign _ iexpr) = genIExpr iexpr
gen (B_BAssign _ bexpr) = genBExpr bexpr
gen (B_Cond bexpr) = genBExpr bexpr
gen _ = S.empty

genIExpr :: IExpr -> S.Set Var
genIExpr (IConst _) = S.empty
genIExpr (Var n) = S.singleton n
genIExpr (Plus e1 e2) = S.union (genIExpr e1) $ genIExpr e2
genIExpr (Minus e1 e2) = S.union (genIExpr e1) $ genIExpr e2
genIExpr (Divide e1 e2) = S.union (genIExpr e1) $ genIExpr e2
genIExpr (Times e1 e2) = S.union (genIExpr e1) $ genIExpr e2

genBExpr :: BExpr -> S.Set Var
genBExpr (BConst _) = S.empty
genBExpr (BVar n) = S.singleton n
genBExpr (LessThan e1 e2) = S.union (genIExpr e1) $ genIExpr e2
genBExpr (GreaterThan e1 e2) = S.union (genIExpr e1) $ genIExpr e2
genBExpr (LessEqual e1 e2) = S.union (genIExpr e1) $ genIExpr e2
genBExpr (GreaterEqual e1 e2) = S.union (genIExpr e1) $ genIExpr e2
genBExpr (IEqual e1 e2) = S.union (genIExpr e1) $ genIExpr e2
genBExpr (BEqual b1 b2) = S.union (genBExpr b1) $ genBExpr b2
genBExpr (And b1 b2) = S.union (genBExpr b1) $ genBExpr b2
genBExpr (Or b1 b2) = S.union (genBExpr b1) $ genBExpr b2
genBExpr (Not b) = genBExpr b