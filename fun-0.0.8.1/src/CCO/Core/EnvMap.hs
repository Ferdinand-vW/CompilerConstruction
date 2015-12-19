module EnvMap where

import qualified Prelude as P

type EnvMap a b = (Int,[(a,b)])

getOffSet :: Map String Ref -> Int
getOffSet xs = P.fst xs

lookup'  :: String -> Map String Ref -> Maybe Ref
lookup' s (_,env) = P.lookup s env

insertg :: Map String Ref -> String -> Map String Ref
insertg (n,xs) s = (n + 1,(s, Glob n) : xs)

insertWithValue :: Map String Ref -> String -> Ref -> Map String Ref
insertWithValue (n,xs) s r = (n,(s,r):xs)

insertl :: Map String Ref -> Int -> String -> Map String Ref
insertl (n,xs) lvl s = (n + 1, (s, Loc lvl n) : xs)