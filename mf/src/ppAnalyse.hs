module PpAnalyse where

import Administration
import qualified Data.Map as M

instance Show ProgramInfo where
    show (ProgramInfo b l i fi fl ifl v) = "Block: "  ++ newLine ++ toBlock b ++ newLine
                                    ++ "Flow: "   ++ listOf ',' fl        ++ newLine
                                    ++ "InterFlow: " ++ listOf ',' ifl    ++ newLine
                                    ++ "Labels: " ++ listOf ',' l         ++ newLine
                                    ++ "Init: "   ++ listOf ',' i         ++ newLine
                                    ++ "Finals: " ++ listOf ',' fi        ++ newLine

instance Show Block where
    show (B_IAssign n v) = n ++ " := " ++ show v
    show (B_BAssign n v) = n ++ " := " ++ show v
    show (B_Cond    c  ) = show c
    show B_Skip          = "Skip"


toBlock :: M.Map Label Block -> String
toBlock xs = M.foldrWithKey (\k a b -> show a ++ " Label:" ++ show k ++ newLine ++ b) "" xs


listOf :: (Show a) => Char -> [a] -> String
listOf c []     = brackets ""
listOf c [x]    = brackets $ show x
listOf c xs     = brackets $ foldr (\a b -> show a ++ c : b) "" xs

brackets :: String -> String
brackets s = "{" ++ s ++ "}" 

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
    show (Var n)      = show n
    show (Plus   l r) = show l ++ " + " ++ show r
    show (Minus  l r) = show l ++ " - " ++ show r
    show (Times  l r) = show l ++ " * " ++ show r
    show (Divide l r) = show l ++ " / " ++ show r
    show (Deref    p) = show p


ppAnalyse :: Show a => M.Map Label a -> String
ppAnalyse = undefined

newLine :: String
newLine = "\n"