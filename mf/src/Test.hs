module Test where
import All
import Data.Tuple
lab :: [(Int,Int)]
lab = [(2,10),(1,2),(10,5)]

init' :: [(Int,Int)] -> Int
init' = fst . head

--get labels from flow
labels :: [(Int,Int)] -> [Int]
labels xs = [(minimum l)..(maximum l)]
    where l = concatTuple $ unzip xs

concatTuple :: ([a],[a]) -> [a]
concatTuple (a,b) = a ++ b

flowR :: [(a,b)] -> [(b,a)]
flowR = map swap