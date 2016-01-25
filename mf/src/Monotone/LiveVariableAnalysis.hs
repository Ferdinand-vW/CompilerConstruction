{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Monotone.LiveVariableAnalysis 
(slv)
where


import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple

import Administration
import Monotone.Analysis (analyse, Analysis)
import Monotone.MonotoneFramework

slv :: ProgramInfo -> Analysis (S.Set Var)
slv p = let join = S.union
            btm  = S.empty
            lmeet = S.isSubsetOf
            tfunc = transferFunction
            flw = map swap (Administration.flow p)
            ifl = [] --we don't have a slv instance for embellished monotoneframework
            eLabels = finals p
            eValue = S.fromList (statVars p)
            mframe = MonotoneFramework join btm lmeet tfunc flw ifl eLabels eValue
        in analyse mframe (blocks p)

--For live variable analysis we take the current set, remote any killed variables and then add
--any generated  variables
--For strong live variable analysis, we first have to check that whatever we kill is a subset
--of the current set. We do this to ensure that we can actually kill the variables. If the variables
--are not live in the current set, then that means that what ever we are killing is actually deadcode.
--Thus then there is no need to generate and kill
transferFunction :: M.Map Label (S.Set Var) -> Block -> Label -> S.Set Var -> S.Set Var
transferFunction _ bl _ set = if kill bl `S.isSubsetOf` set
                                then S.union (S.difference set (kill bl)) (gen bl)
                                else set

kill :: Block -> S.Set Var
kill (B_IAssign n _) = S.singleton n
kill (B_BAssign n _) = S.singleton n
kill _               = S.empty

gen :: Block -> S.Set Var
gen (B_IAssign _ iexpr) = genIExpr iexpr
gen (B_BAssign _ bexpr) = genBExpr bexpr
gen (B_Cond bexpr)      = genBExpr bexpr
gen _                   = S.empty

genIExpr :: IExpr -> S.Set Var
genIExpr (IConst _)     = S.empty
genIExpr (Var n)        = S.singleton n
genIExpr (Plus e1 e2)   = S.union (genIExpr e1) $ genIExpr e2
genIExpr (Minus e1 e2)  = S.union (genIExpr e1) $ genIExpr e2
genIExpr (Divide e1 e2) = S.union (genIExpr e1) $ genIExpr e2
genIExpr (Times e1 e2)  = S.union (genIExpr e1) $ genIExpr e2

genBExpr :: BExpr -> S.Set Var
genBExpr (BConst _)           = S.empty
genBExpr (BVar n)             = S.singleton n
genBExpr (LessThan e1 e2)     = S.union (genIExpr e1) $ genIExpr e2
genBExpr (GreaterThan e1 e2)  = S.union (genIExpr e1) $ genIExpr e2
genBExpr (LessEqual e1 e2)    = S.union (genIExpr e1) $ genIExpr e2
genBExpr (GreaterEqual e1 e2) = S.union (genIExpr e1) $ genIExpr e2
genBExpr (IEqual e1 e2)       = S.union (genIExpr e1) $ genIExpr e2
genBExpr (BEqual b1 b2)       = S.union (genBExpr b1) $ genBExpr b2
genBExpr (And b1 b2)          = S.union (genBExpr b1) $ genBExpr b2
genBExpr (Or b1 b2)           = S.union (genBExpr b1) $ genBExpr b2
genBExpr (Not b)              = genBExpr b
