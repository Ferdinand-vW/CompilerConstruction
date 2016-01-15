module LiveVariableAnalysis 
(slv)
where


import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple

import Analysis (analyse, Analysis)
import MonotoneFramework
import Administration

slv :: ProgramInfo -> IO (Analysis (S.Set Var))
slv p = let join = S.union
            btm  = S.empty
            lmeet = S.isSubsetOf
            tfunc = transferFunction
            flw = map swap (Administration.flow p)
            eLabels = finals p
            eValue = S.fromList (vars p)
            mframe = MonotoneFramework join btm lmeet tfunc flw eLabels eValue
        in analyse mframe (blocks p)

transferFunction :: Block -> S.Set Var -> S.Set Var
transferFunction bl set = S.union (S.difference set (kill bl)) (gen bl)

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
