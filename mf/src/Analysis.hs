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
analyse (MonotoneFramework join btm lmeet tf fl ifl el ev) bl = 
        do
          print $ "Initiat state: " ++ show array'
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
              print $ "transferFunction on l:" ++ show transfer
              print $ "the lmeet: " ++ show (lmeet transfer (slookup l' arr))
              if not $ lmeet transfer (slookup l' arr)
                --If it is not more precise
                --Add the above union to the array
                then let arr' = M.adjust (\x -> join x transfer) l' arr
                         w = updateWorkSet l' xs fl --Then add all Tuples in the Flow that start with label l' to the current workset
                      in
                      do
                        print $ "New state l':" ++ show (slookup l' arr')
                        print "-----------------------------------------------------------------------------"
                        loop w arr' --recurse
                else do
                  print "-----------------------------------------------------------------------------"
                  loop xs arr --recurse
            where
              transfer = tf arr (slookup l bl) l (slookup l arr)
        updateWorkSet :: Eq a => a -> [(a,b)] -> [(a,b)] -> [(a,b)]
        updateWorkSet l w [] = w
        updateWorkSet l w (x:xs)
          | l == fst x = updateWorkSet l (x : w) xs
          | otherwise  = updateWorkSet l w xs
        finalize arr = M.mapWithKey (\k a -> (a, tf arr (slookup k bl) k a)) arr

--We can assume that a Label always exists in a Map, otherwise
--its a coding error
slookup :: Ord a => a -> M.Map a b -> b
slookup l stats = fromJust $ M.lookup l stats