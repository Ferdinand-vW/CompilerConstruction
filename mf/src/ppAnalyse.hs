{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module PpAnalyse where

import Administration
import Analysis (analyse, Analysis)

import qualified Data.Map as M
import qualified Data.Set as S

instance Show ProgramInfo where
    show (ProgramInfo b l i fi fl v) = "Block:  " ++ newLine ++ toBlock b ++ newLine
                                    ++ "Flow:   " ++ listOf ',' fl        ++ newLine
                                    ++ "Labels: " ++ listOf ',' l         ++ newLine
                                    ++ "Init:   " ++ listOf ',' i         ++ newLine
                                    ++ "Finals: " ++ listOf ',' fi        ++ newLine

instance Show Block where
    show (B_IAssign n v) = n ++ " := " ++ show v
    show (B_BAssign n v) = n ++ " := " ++ show v
    show (B_Cond    c  ) = show c
    show B_Skip          = "Skip"


toBlock :: M.Map Label Block -> String
toBlock xs = M.foldrWithKey (\k a b -> "Label " ++ show k  ++ ": " ++ show a ++ newLine ++ b) "" xs


instance Show BExpr where
    show (BConst         v) = show v
    show (BVar           n) = show n
    show (LessThan     l r) = show l ++  " < " ++ show r
    show (GreaterThan  l r) = show l ++  " > " ++ show r
    show (LessEqual    l r) = show l ++ " <= " ++ show r
    show (GreaterEqual l r) = show l ++ " >= " ++ show r
    show (IEqual       l r) = show l ++ " == " ++ show r
    show (BEqual       l r) = show l ++ " == " ++ show r
    show (And          l r) = show l ++ " && " ++ show r 
    show (Or           l r) = show l ++ " && " ++ show r 
    show (Not            v) = show v


instance Show IExpr where
    show (IConst   v) = show v
    show (Var n)      = n
    show (Plus   l r) = show l ++ " + " ++ show r
    show (Minus  l r) = show l ++ " - " ++ show r
    show (Times  l r) = show l ++ " * " ++ show r
    show (Divide l r) = show l ++ " / " ++ show r
    show (Deref    p) = show p

--Added the view class to show all the analysis in a correct form. 
class View a where
  view :: a -> String

------View of Live Variables analysis
instance View (S.Set Var) where
    view xs = brackets $ S.foldr (\a b -> show a ++ "," ++ b) "" xs



instance View (Analysis (S.Set Var)) where
    view xs = let s = maxSizeVar xs
              in header s "Entry" "Exit" ++
                 M.foldrWithKey (\k v b -> (row k v s ++ b)) "" xs


maxSizeVar :: Analysis (S.Set Var) -> (Int, Int)
maxSizeVar xs = (maximum lm,maximum rm)
            where (lm,rm) = unzip $ M.foldr (\(lv,rv) b -> (sizeVar lv,sizeVar rv) : b) [] $ xs

header :: (Int,Int) -> String -> String -> String
header (ls,rs) lv rv = "|" ++ replicate 5 ' ' ++ "|"
                    ++ column (2 + ls - length lv) lv
                    ++ column (2 + rs - length rv) rv
                    ++ newLine
 
row :: Int -> (S.Set Var,  S.Set Var) -> (Int,Int) -> String
row l (lv,rv) (lm,rm) = "|" 
                 ++ column 4 (show l) 
                 ++ column (lm - sizeVar lv) (view lv)
                 ++ column (rm - sizeVar rv) (view rv) 
                 ++ newLine

column :: Int -> String -> String
column s w = w ++ replicate s ' ' ++ "|"

sizeVar :: S.Set Var -> Int
sizeVar xs = (S.size xs * 3) + (S.foldr (\a b -> length a + b ) 0 xs)




------View of Constant Propoagation  analysis
instance View (M.Map Var (Lattice Int)) where
    view xs = brackets $ M.foldrWithKey (\v l b -> v ++ " => " ++ show l ++ ',' : b) "" xs


instance View (Analysis (M.Map Var (Lattice Int))) where
    view xs =  M.foldrWithKey (\k (l,r) b -> show k ++ view l ++ " => " ++ view r ++ newLine ++ b ) "" xs



instance Show (Lattice Int) where 
    show Top = "T"
    show Bottom = "_"
    show (Value a) = show a


--This data is used in the ConstantPropagation.
data Lattice a = Top | Bottom | Value a deriving Eq


--helpers
newLine :: String
newLine = "\n"


listOf :: (Show a) => Char -> [a] -> String
listOf c []     = brackets ""
listOf c [x]    = brackets $ show x
listOf c xs     = brackets $ foldr (\a b -> show a ++ c : b) "" xs

brackets :: String -> String
brackets s = "{" ++ s ++ "}" 