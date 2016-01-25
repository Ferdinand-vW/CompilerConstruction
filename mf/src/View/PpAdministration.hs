module View.PpAdministration where

import qualified Data.Map as M
import Data.List

import View.View
import View.PpHelper
import Administration

instance View ProgramInfo where
    view (ProgramInfo b l i fi fl ifl vars _) = 
                                       "Block:     " ++ newLine ++ toBlock b ++ newLine
                                    ++ "Flow:      " ++ listOf "," fl        ++ newLine
                                    ++ "InterFlow: " ++ listOf "," ifl       ++ newLine
                                    ++ "Labels:    " ++ listOf "," l         ++ newLine
                                    ++ "Init:      " ++ listOf "," i         ++ newLine
                                    ++ "Finals:    " ++ listOf "," fi        ++ newLine
                                    ++ "Variables: " ++ listOf "," vars      ++ newLine



instance View Block where
    view (B_IAssign n v) = n ++ " := " ++ view v
    view (B_BAssign n v) = n ++ " := " ++ view v
    view (B_Cond    c  ) = show c
    view (B_CallEntry name params out _ _) = "Call " ++ name ++ "(" ++ intercalate "," (map view params) ++ ", " ++ out ++ ") Entry"
    view (B_CallExit name params out _ _) = "Call " ++ name ++ "(" ++ intercalate "," (map view params) ++ ", " ++ out ++ ") Exit"
    view (B_ProcEntry name args out) = "proc " ++ name ++ "(val " ++ head args ++ "," ++ intercalate "," (tail args) ++ ", res " ++ out ++ ") is"
    view  B_ProcExit = "end"
    view  B_Skip          = "Skip"


toBlock :: M.Map Label Block -> String
toBlock xs = M.foldrWithKey (\k a b -> "Label " ++ show k  ++ ": " ++ view a ++ newLine ++ b) "" xs

instance View Expr where
    view (I expr) = view expr
    view (B expr) = view expr

instance View BExpr where
    view (BConst         v) = show v
    view (BVar           n) = show n
    view (LessThan     l r) = view l ++  " < " ++ view r
    view (GreaterThan  l r) = view l ++  " > " ++ view r
    view (LessEqual    l r) = view l ++ " <= " ++ view r
    view (GreaterEqual l r) = view l ++ " >= " ++ view r
    view (IEqual       l r) = view l ++ " == " ++ view r
    view (BEqual       l r) = view l ++ " == " ++ view r
    view (And          l r) = view l ++ " && " ++ view r 
    view (Or           l r) = view l ++ " && " ++ view r 
    view (Not            v) = show v


instance View IExpr where
    view (IConst   v) = show v
    view (Var n)      = n
    view (Plus   l r) = view l ++ " + " ++ view r
    view (Minus  l r) = view l ++ " - " ++ view r
    view (Times  l r) = view l ++ " * " ++ view r
    view (Divide l r) = view l ++ " / " ++ view r
    view (Deref    p) = show p