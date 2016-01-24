

-- UUAGC 0.9.52.1 (Administration)
module Administration where
{-# LINE 4 "Administration.ag" #-}

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
{-# LINE 11 "Administration.hs" #-}
{-# LINE 1 "AttributeGrammar.ag" #-}

--import qualified Data.Map as M
--import qualified Data.Maybe as Maybe
--import qualified Data.List as L
{-# LINE 17 "Administration.hs" #-}

{-# LINE 93 "AttributeGrammar.ag" #-}


type Procs' = [Proc']

{-# LINE 24 "Administration.hs" #-}

{-# LINE 10 "Administration.ag" #-}



toProgramInfo :: Program -> ProgramInfo
toProgramInfo program = pinfo_Syn_Program $ wrap_Program (sem_Program program) (Inh_Program)

data ProgramInfo = ProgramInfo {blocks :: M.Map Label Block, labels :: [Label] , init :: [Label], finals :: [Label], 
                                flow :: Flow, interflow :: InterFlow, vars :: [Var]}

data Block = 
    B_IAssign {name :: String, valI :: IExpr} |
    B_BAssign {name :: String, valB :: BExpr} |
    B_Cond {cond :: BExpr} |
    B_Skip |
    B_CallEntry {name :: String, params :: Exprs, cOut :: String, pArgs :: [Var], pOut :: Var} |
    B_CallExit  {name :: String, params :: Exprs, cOut :: String, pArgs :: [Var], pOut :: Var} |
    B_ProcEntry {name :: String, args :: [Var], out :: Var} |
    B_ProcExit
    deriving Show

type Label = Int
type Flow = [(Int, Int)]
type InterFlow = [(Int,Int,Int,Int)]
type Var = String
{-# LINE 51 "Administration.hs" #-}

{-# LINE 223 "Administration.ag" #-}


--foldProcs :: Procs -> (Int, M.Map String Proc')
--foldProcs prcs = foldr (\x y -> 
                --let (l,proc') = wrapproc x (fst y)
                --in (l + 1,M.insert (getName proc') proc' $ snd y)) (1,M.empty) prcs

getName :: Proc' -> String
getName (Proc' _ _ name _ _ _) = name

--wrapproc :: Proc -> Int -> (Int, Proc')
--wrapproc proc label = main_Syn_Proc $ wrap_Proc (sem_Proc proc) (Inh_Proc label)
{-# LINE 66 "Administration.hs" #-}
-- BExpr -------------------------------------------------------
data BExpr = BConst (Bool)
           | BVar (String)
           | LessThan (IExpr) (IExpr)
           | GreaterThan (IExpr) (IExpr)
           | LessEqual (IExpr) (IExpr)
           | GreaterEqual (IExpr) (IExpr)
           | IEqual (IExpr) (IExpr)
           | BEqual (BExpr) (BExpr)
           | And (BExpr) (BExpr)
           | Or (BExpr) (BExpr)
           | Not (BExpr)
           deriving ( Eq,Show)
-- cata
sem_BExpr :: BExpr ->
             T_BExpr
sem_BExpr (BConst _val) =
    (sem_BExpr_BConst _val)
sem_BExpr (BVar _name) =
    (sem_BExpr_BVar _name)
sem_BExpr (LessThan _left _right) =
    (sem_BExpr_LessThan (sem_IExpr _left) (sem_IExpr _right))
sem_BExpr (GreaterThan _left _right) =
    (sem_BExpr_GreaterThan (sem_IExpr _left) (sem_IExpr _right))
sem_BExpr (LessEqual _left _right) =
    (sem_BExpr_LessEqual (sem_IExpr _left) (sem_IExpr _right))
sem_BExpr (GreaterEqual _left _right) =
    (sem_BExpr_GreaterEqual (sem_IExpr _left) (sem_IExpr _right))
sem_BExpr (IEqual _left _right) =
    (sem_BExpr_IEqual (sem_IExpr _left) (sem_IExpr _right))
sem_BExpr (BEqual _left _right) =
    (sem_BExpr_BEqual (sem_BExpr _left) (sem_BExpr _right))
sem_BExpr (And _left _right) =
    (sem_BExpr_And (sem_BExpr _left) (sem_BExpr _right))
sem_BExpr (Or _left _right) =
    (sem_BExpr_Or (sem_BExpr _left) (sem_BExpr _right))
sem_BExpr (Not _val) =
    (sem_BExpr_Not (sem_BExpr _val))
-- semantic domain
type T_BExpr = ( BExpr,(S.Set Var))
data Inh_BExpr = Inh_BExpr {}
data Syn_BExpr = Syn_BExpr {slf_Syn_BExpr :: BExpr,svars_Syn_BExpr :: (S.Set Var)}
wrap_BExpr :: T_BExpr ->
              Inh_BExpr ->
              Syn_BExpr
wrap_BExpr sem (Inh_BExpr) =
    (let ( _lhsOslf,_lhsOsvars) = sem
     in  (Syn_BExpr _lhsOslf _lhsOsvars))
sem_BExpr_BConst :: Bool ->
                    T_BExpr
sem_BExpr_BConst val_ =
    (let _lhsOsvars :: (S.Set Var)
         _lhsOslf :: BExpr
         _lhsOsvars =
             ({-# LINE 212 "Administration.ag" #-}
              S.empty
              {-# LINE 123 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 110 "Administration.ag" #-}
              BConst val_
              {-# LINE 128 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 110 "Administration.ag" #-}
              _slf
              {-# LINE 133 "Administration.hs" #-}
              )
     in  ( _lhsOslf,_lhsOsvars))
sem_BExpr_BVar :: String ->
                  T_BExpr
sem_BExpr_BVar name_ =
    (let _lhsOsvars :: (S.Set Var)
         _lhsOslf :: BExpr
         _lhsOsvars =
             ({-# LINE 213 "Administration.ag" #-}
              S.singleton name_
              {-# LINE 144 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 110 "Administration.ag" #-}
              BVar name_
              {-# LINE 149 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 110 "Administration.ag" #-}
              _slf
              {-# LINE 154 "Administration.hs" #-}
              )
     in  ( _lhsOslf,_lhsOsvars))
sem_BExpr_LessThan :: T_IExpr ->
                      T_IExpr ->
                      T_BExpr
sem_BExpr_LessThan left_ right_ =
    (let _lhsOsvars :: (S.Set Var)
         _lhsOslf :: BExpr
         _leftIslf :: IExpr
         _leftIsvars :: (S.Set Var)
         _rightIslf :: IExpr
         _rightIsvars :: (S.Set Var)
         _lhsOsvars =
             ({-# LINE 214 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 170 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 110 "Administration.ag" #-}
              LessThan _leftIslf _rightIslf
              {-# LINE 175 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 110 "Administration.ag" #-}
              _slf
              {-# LINE 180 "Administration.hs" #-}
              )
         ( _leftIslf,_leftIsvars) =
             left_
         ( _rightIslf,_rightIsvars) =
             right_
     in  ( _lhsOslf,_lhsOsvars))
sem_BExpr_GreaterThan :: T_IExpr ->
                         T_IExpr ->
                         T_BExpr
sem_BExpr_GreaterThan left_ right_ =
    (let _lhsOsvars :: (S.Set Var)
         _lhsOslf :: BExpr
         _leftIslf :: IExpr
         _leftIsvars :: (S.Set Var)
         _rightIslf :: IExpr
         _rightIsvars :: (S.Set Var)
         _lhsOsvars =
             ({-# LINE 215 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 200 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 110 "Administration.ag" #-}
              GreaterThan _leftIslf _rightIslf
              {-# LINE 205 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 110 "Administration.ag" #-}
              _slf
              {-# LINE 210 "Administration.hs" #-}
              )
         ( _leftIslf,_leftIsvars) =
             left_
         ( _rightIslf,_rightIsvars) =
             right_
     in  ( _lhsOslf,_lhsOsvars))
sem_BExpr_LessEqual :: T_IExpr ->
                       T_IExpr ->
                       T_BExpr
sem_BExpr_LessEqual left_ right_ =
    (let _lhsOsvars :: (S.Set Var)
         _lhsOslf :: BExpr
         _leftIslf :: IExpr
         _leftIsvars :: (S.Set Var)
         _rightIslf :: IExpr
         _rightIsvars :: (S.Set Var)
         _lhsOsvars =
             ({-# LINE 216 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 230 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 110 "Administration.ag" #-}
              LessEqual _leftIslf _rightIslf
              {-# LINE 235 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 110 "Administration.ag" #-}
              _slf
              {-# LINE 240 "Administration.hs" #-}
              )
         ( _leftIslf,_leftIsvars) =
             left_
         ( _rightIslf,_rightIsvars) =
             right_
     in  ( _lhsOslf,_lhsOsvars))
sem_BExpr_GreaterEqual :: T_IExpr ->
                          T_IExpr ->
                          T_BExpr
sem_BExpr_GreaterEqual left_ right_ =
    (let _lhsOsvars :: (S.Set Var)
         _lhsOslf :: BExpr
         _leftIslf :: IExpr
         _leftIsvars :: (S.Set Var)
         _rightIslf :: IExpr
         _rightIsvars :: (S.Set Var)
         _lhsOsvars =
             ({-# LINE 217 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 260 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 110 "Administration.ag" #-}
              GreaterEqual _leftIslf _rightIslf
              {-# LINE 265 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 110 "Administration.ag" #-}
              _slf
              {-# LINE 270 "Administration.hs" #-}
              )
         ( _leftIslf,_leftIsvars) =
             left_
         ( _rightIslf,_rightIsvars) =
             right_
     in  ( _lhsOslf,_lhsOsvars))
sem_BExpr_IEqual :: T_IExpr ->
                    T_IExpr ->
                    T_BExpr
sem_BExpr_IEqual left_ right_ =
    (let _lhsOsvars :: (S.Set Var)
         _lhsOslf :: BExpr
         _leftIslf :: IExpr
         _leftIsvars :: (S.Set Var)
         _rightIslf :: IExpr
         _rightIsvars :: (S.Set Var)
         _lhsOsvars =
             ({-# LINE 218 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 290 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 110 "Administration.ag" #-}
              IEqual _leftIslf _rightIslf
              {-# LINE 295 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 110 "Administration.ag" #-}
              _slf
              {-# LINE 300 "Administration.hs" #-}
              )
         ( _leftIslf,_leftIsvars) =
             left_
         ( _rightIslf,_rightIsvars) =
             right_
     in  ( _lhsOslf,_lhsOsvars))
sem_BExpr_BEqual :: T_BExpr ->
                    T_BExpr ->
                    T_BExpr
sem_BExpr_BEqual left_ right_ =
    (let _lhsOsvars :: (S.Set Var)
         _lhsOslf :: BExpr
         _leftIslf :: BExpr
         _leftIsvars :: (S.Set Var)
         _rightIslf :: BExpr
         _rightIsvars :: (S.Set Var)
         _lhsOsvars =
             ({-# LINE 219 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 320 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 110 "Administration.ag" #-}
              BEqual _leftIslf _rightIslf
              {-# LINE 325 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 110 "Administration.ag" #-}
              _slf
              {-# LINE 330 "Administration.hs" #-}
              )
         ( _leftIslf,_leftIsvars) =
             left_
         ( _rightIslf,_rightIsvars) =
             right_
     in  ( _lhsOslf,_lhsOsvars))
sem_BExpr_And :: T_BExpr ->
                 T_BExpr ->
                 T_BExpr
sem_BExpr_And left_ right_ =
    (let _lhsOsvars :: (S.Set Var)
         _lhsOslf :: BExpr
         _leftIslf :: BExpr
         _leftIsvars :: (S.Set Var)
         _rightIslf :: BExpr
         _rightIsvars :: (S.Set Var)
         _lhsOsvars =
             ({-# LINE 220 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 350 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 110 "Administration.ag" #-}
              And _leftIslf _rightIslf
              {-# LINE 355 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 110 "Administration.ag" #-}
              _slf
              {-# LINE 360 "Administration.hs" #-}
              )
         ( _leftIslf,_leftIsvars) =
             left_
         ( _rightIslf,_rightIsvars) =
             right_
     in  ( _lhsOslf,_lhsOsvars))
sem_BExpr_Or :: T_BExpr ->
                T_BExpr ->
                T_BExpr
sem_BExpr_Or left_ right_ =
    (let _lhsOslf :: BExpr
         _lhsOsvars :: (S.Set Var)
         _leftIslf :: BExpr
         _leftIsvars :: (S.Set Var)
         _rightIslf :: BExpr
         _rightIsvars :: (S.Set Var)
         _slf =
             ({-# LINE 110 "Administration.ag" #-}
              Or _leftIslf _rightIslf
              {-# LINE 380 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 110 "Administration.ag" #-}
              _slf
              {-# LINE 385 "Administration.hs" #-}
              )
         _lhsOsvars =
             ({-# LINE 109 "Administration.ag" #-}
              _rightIsvars
              {-# LINE 390 "Administration.hs" #-}
              )
         ( _leftIslf,_leftIsvars) =
             left_
         ( _rightIslf,_rightIsvars) =
             right_
     in  ( _lhsOslf,_lhsOsvars))
sem_BExpr_Not :: T_BExpr ->
                 T_BExpr
sem_BExpr_Not val_ =
    (let _lhsOsvars :: (S.Set Var)
         _lhsOslf :: BExpr
         _valIslf :: BExpr
         _valIsvars :: (S.Set Var)
         _lhsOsvars =
             ({-# LINE 221 "Administration.ag" #-}
              _valIsvars
              {-# LINE 407 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 110 "Administration.ag" #-}
              Not _valIslf
              {-# LINE 412 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 110 "Administration.ag" #-}
              _slf
              {-# LINE 417 "Administration.hs" #-}
              )
         ( _valIslf,_valIsvars) =
             val_
     in  ( _lhsOslf,_lhsOsvars))
-- Code --------------------------------------------------------
data Code = CBExpr (BExpr)
          | CIExpr (IExpr)
          | CStat (Stat')
          | CProc (Proc')
          | CProgram (Program')
-- cata
sem_Code :: Code ->
            T_Code
sem_Code (CBExpr _bExpr) =
    (sem_Code_CBExpr (sem_BExpr _bExpr))
sem_Code (CIExpr _iExpr) =
    (sem_Code_CIExpr (sem_IExpr _iExpr))
sem_Code (CStat _stat') =
    (sem_Code_CStat (sem_Stat' _stat'))
sem_Code (CProc _proc') =
    (sem_Code_CProc (sem_Proc' _proc'))
sem_Code (CProgram _program') =
    (sem_Code_CProgram (sem_Program' _program'))
-- semantic domain
type T_Code = ( )
data Inh_Code = Inh_Code {}
data Syn_Code = Syn_Code {}
wrap_Code :: T_Code ->
             Inh_Code ->
             Syn_Code
wrap_Code sem (Inh_Code) =
    (let ( ) = sem
     in  (Syn_Code))
sem_Code_CBExpr :: T_BExpr ->
                   T_Code
sem_Code_CBExpr bExpr_ =
    (let _bExprIslf :: BExpr
         _bExprIsvars :: (S.Set Var)
         ( _bExprIslf,_bExprIsvars) =
             bExpr_
     in  ( ))
sem_Code_CIExpr :: T_IExpr ->
                   T_Code
sem_Code_CIExpr iExpr_ =
    (let _iExprIslf :: IExpr
         _iExprIsvars :: (S.Set Var)
         ( _iExprIslf,_iExprIsvars) =
             iExpr_
     in  ( ))
sem_Code_CStat :: (T_Stat') ->
                  T_Code
sem_Code_CStat stat'_ =
    (let
     in  ( ))
sem_Code_CProc :: (T_Proc') ->
                  T_Code
sem_Code_CProc proc'_ =
    (let
     in  ( ))
sem_Code_CProgram :: (T_Program') ->
                     T_Code
sem_Code_CProgram program'_ =
    (let
     in  ( ))
-- Expr --------------------------------------------------------
data Expr = B (BExpr)
          | I (IExpr)
          deriving ( Eq,Show)
-- cata
sem_Expr :: Expr ->
            T_Expr
sem_Expr (B _expr) =
    (sem_Expr_B (sem_BExpr _expr))
sem_Expr (I _expr) =
    (sem_Expr_I (sem_IExpr _expr))
-- semantic domain
type T_Expr = ( Expr,(S.Set Var))
data Inh_Expr = Inh_Expr {}
data Syn_Expr = Syn_Expr {slf_Syn_Expr :: Expr,svars_Syn_Expr :: (S.Set Var)}
wrap_Expr :: T_Expr ->
             Inh_Expr ->
             Syn_Expr
wrap_Expr sem (Inh_Expr) =
    (let ( _lhsOslf,_lhsOsvars) = sem
     in  (Syn_Expr _lhsOslf _lhsOsvars))
sem_Expr_B :: T_BExpr ->
              T_Expr
sem_Expr_B expr_ =
    (let _lhsOsvars :: (S.Set Var)
         _lhsOslf :: Expr
         _exprIslf :: BExpr
         _exprIsvars :: (S.Set Var)
         _lhsOsvars =
             ({-# LINE 200 "Administration.ag" #-}
              _exprIsvars
              {-# LINE 513 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 110 "Administration.ag" #-}
              B _exprIslf
              {-# LINE 518 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 110 "Administration.ag" #-}
              _slf
              {-# LINE 523 "Administration.hs" #-}
              )
         ( _exprIslf,_exprIsvars) =
             expr_
     in  ( _lhsOslf,_lhsOsvars))
sem_Expr_I :: T_IExpr ->
              T_Expr
sem_Expr_I expr_ =
    (let _lhsOsvars :: (S.Set Var)
         _lhsOslf :: Expr
         _exprIslf :: IExpr
         _exprIsvars :: (S.Set Var)
         _lhsOsvars =
             ({-# LINE 201 "Administration.ag" #-}
              _exprIsvars
              {-# LINE 538 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 110 "Administration.ag" #-}
              I _exprIslf
              {-# LINE 543 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 110 "Administration.ag" #-}
              _slf
              {-# LINE 548 "Administration.hs" #-}
              )
         ( _exprIslf,_exprIsvars) =
             expr_
     in  ( _lhsOslf,_lhsOsvars))
-- Exprs -------------------------------------------------------
type Exprs = [Expr]
-- cata
sem_Exprs :: Exprs ->
             T_Exprs
sem_Exprs list =
    (Prelude.foldr sem_Exprs_Cons sem_Exprs_Nil (Prelude.map sem_Expr list))
-- semantic domain
type T_Exprs = ( Exprs,(S.Set Var))
data Inh_Exprs = Inh_Exprs {}
data Syn_Exprs = Syn_Exprs {slf_Syn_Exprs :: Exprs,svars_Syn_Exprs :: (S.Set Var)}
wrap_Exprs :: T_Exprs ->
              Inh_Exprs ->
              Syn_Exprs
wrap_Exprs sem (Inh_Exprs) =
    (let ( _lhsOslf,_lhsOsvars) = sem
     in  (Syn_Exprs _lhsOslf _lhsOsvars))
sem_Exprs_Cons :: T_Expr ->
                  T_Exprs ->
                  T_Exprs
sem_Exprs_Cons hd_ tl_ =
    (let _lhsOsvars :: (S.Set Var)
         _lhsOslf :: Exprs
         _hdIslf :: Expr
         _hdIsvars :: (S.Set Var)
         _tlIslf :: Exprs
         _tlIsvars :: (S.Set Var)
         _lhsOsvars =
             ({-# LINE 196 "Administration.ag" #-}
              S.union _hdIsvars _tlIsvars
              {-# LINE 583 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 110 "Administration.ag" #-}
              (:) _hdIslf _tlIslf
              {-# LINE 588 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 110 "Administration.ag" #-}
              _slf
              {-# LINE 593 "Administration.hs" #-}
              )
         ( _hdIslf,_hdIsvars) =
             hd_
         ( _tlIslf,_tlIsvars) =
             tl_
     in  ( _lhsOslf,_lhsOsvars))
sem_Exprs_Nil :: T_Exprs
sem_Exprs_Nil =
    (let _lhsOsvars :: (S.Set Var)
         _lhsOslf :: Exprs
         _lhsOsvars =
             ({-# LINE 197 "Administration.ag" #-}
              S.empty
              {-# LINE 607 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 110 "Administration.ag" #-}
              []
              {-# LINE 612 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 110 "Administration.ag" #-}
              _slf
              {-# LINE 617 "Administration.hs" #-}
              )
     in  ( _lhsOslf,_lhsOsvars))
-- IExpr -------------------------------------------------------
data IExpr = IConst (Int)
           | Var (String)
           | Plus (IExpr) (IExpr)
           | Minus (IExpr) (IExpr)
           | Times (IExpr) (IExpr)
           | Divide (IExpr) (IExpr)
           | Deref (IExpr)
           deriving ( Eq,Show)
-- cata
sem_IExpr :: IExpr ->
             T_IExpr
sem_IExpr (IConst _val) =
    (sem_IExpr_IConst _val)
sem_IExpr (Var _name) =
    (sem_IExpr_Var _name)
sem_IExpr (Plus _left _right) =
    (sem_IExpr_Plus (sem_IExpr _left) (sem_IExpr _right))
sem_IExpr (Minus _left _right) =
    (sem_IExpr_Minus (sem_IExpr _left) (sem_IExpr _right))
sem_IExpr (Times _left _right) =
    (sem_IExpr_Times (sem_IExpr _left) (sem_IExpr _right))
sem_IExpr (Divide _left _right) =
    (sem_IExpr_Divide (sem_IExpr _left) (sem_IExpr _right))
sem_IExpr (Deref _ptr) =
    (sem_IExpr_Deref (sem_IExpr _ptr))
-- semantic domain
type T_IExpr = ( IExpr,(S.Set Var))
data Inh_IExpr = Inh_IExpr {}
data Syn_IExpr = Syn_IExpr {slf_Syn_IExpr :: IExpr,svars_Syn_IExpr :: (S.Set Var)}
wrap_IExpr :: T_IExpr ->
              Inh_IExpr ->
              Syn_IExpr
wrap_IExpr sem (Inh_IExpr) =
    (let ( _lhsOslf,_lhsOsvars) = sem
     in  (Syn_IExpr _lhsOslf _lhsOsvars))
sem_IExpr_IConst :: Int ->
                    T_IExpr
sem_IExpr_IConst val_ =
    (let _lhsOsvars :: (S.Set Var)
         _lhsOslf :: IExpr
         _lhsOsvars =
             ({-# LINE 204 "Administration.ag" #-}
              S.empty
              {-# LINE 664 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 110 "Administration.ag" #-}
              IConst val_
              {-# LINE 669 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 110 "Administration.ag" #-}
              _slf
              {-# LINE 674 "Administration.hs" #-}
              )
     in  ( _lhsOslf,_lhsOsvars))
sem_IExpr_Var :: String ->
                 T_IExpr
sem_IExpr_Var name_ =
    (let _lhsOsvars :: (S.Set Var)
         _lhsOslf :: IExpr
         _lhsOsvars =
             ({-# LINE 205 "Administration.ag" #-}
              S.singleton name_
              {-# LINE 685 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 110 "Administration.ag" #-}
              Var name_
              {-# LINE 690 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 110 "Administration.ag" #-}
              _slf
              {-# LINE 695 "Administration.hs" #-}
              )
     in  ( _lhsOslf,_lhsOsvars))
sem_IExpr_Plus :: T_IExpr ->
                  T_IExpr ->
                  T_IExpr
sem_IExpr_Plus left_ right_ =
    (let _lhsOsvars :: (S.Set Var)
         _lhsOslf :: IExpr
         _leftIslf :: IExpr
         _leftIsvars :: (S.Set Var)
         _rightIslf :: IExpr
         _rightIsvars :: (S.Set Var)
         _lhsOsvars =
             ({-# LINE 206 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 711 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 110 "Administration.ag" #-}
              Plus _leftIslf _rightIslf
              {-# LINE 716 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 110 "Administration.ag" #-}
              _slf
              {-# LINE 721 "Administration.hs" #-}
              )
         ( _leftIslf,_leftIsvars) =
             left_
         ( _rightIslf,_rightIsvars) =
             right_
     in  ( _lhsOslf,_lhsOsvars))
sem_IExpr_Minus :: T_IExpr ->
                   T_IExpr ->
                   T_IExpr
sem_IExpr_Minus left_ right_ =
    (let _lhsOsvars :: (S.Set Var)
         _lhsOslf :: IExpr
         _leftIslf :: IExpr
         _leftIsvars :: (S.Set Var)
         _rightIslf :: IExpr
         _rightIsvars :: (S.Set Var)
         _lhsOsvars =
             ({-# LINE 207 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 741 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 110 "Administration.ag" #-}
              Minus _leftIslf _rightIslf
              {-# LINE 746 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 110 "Administration.ag" #-}
              _slf
              {-# LINE 751 "Administration.hs" #-}
              )
         ( _leftIslf,_leftIsvars) =
             left_
         ( _rightIslf,_rightIsvars) =
             right_
     in  ( _lhsOslf,_lhsOsvars))
sem_IExpr_Times :: T_IExpr ->
                   T_IExpr ->
                   T_IExpr
sem_IExpr_Times left_ right_ =
    (let _lhsOsvars :: (S.Set Var)
         _lhsOslf :: IExpr
         _leftIslf :: IExpr
         _leftIsvars :: (S.Set Var)
         _rightIslf :: IExpr
         _rightIsvars :: (S.Set Var)
         _lhsOsvars =
             ({-# LINE 208 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 771 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 110 "Administration.ag" #-}
              Times _leftIslf _rightIslf
              {-# LINE 776 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 110 "Administration.ag" #-}
              _slf
              {-# LINE 781 "Administration.hs" #-}
              )
         ( _leftIslf,_leftIsvars) =
             left_
         ( _rightIslf,_rightIsvars) =
             right_
     in  ( _lhsOslf,_lhsOsvars))
sem_IExpr_Divide :: T_IExpr ->
                    T_IExpr ->
                    T_IExpr
sem_IExpr_Divide left_ right_ =
    (let _lhsOsvars :: (S.Set Var)
         _lhsOslf :: IExpr
         _leftIslf :: IExpr
         _leftIsvars :: (S.Set Var)
         _rightIslf :: IExpr
         _rightIsvars :: (S.Set Var)
         _lhsOsvars =
             ({-# LINE 209 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 801 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 110 "Administration.ag" #-}
              Divide _leftIslf _rightIslf
              {-# LINE 806 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 110 "Administration.ag" #-}
              _slf
              {-# LINE 811 "Administration.hs" #-}
              )
         ( _leftIslf,_leftIsvars) =
             left_
         ( _rightIslf,_rightIsvars) =
             right_
     in  ( _lhsOslf,_lhsOsvars))
sem_IExpr_Deref :: T_IExpr ->
                   T_IExpr
sem_IExpr_Deref ptr_ =
    (let _lhsOslf :: IExpr
         _lhsOsvars :: (S.Set Var)
         _ptrIslf :: IExpr
         _ptrIsvars :: (S.Set Var)
         _slf =
             ({-# LINE 110 "Administration.ag" #-}
              Deref _ptrIslf
              {-# LINE 828 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 110 "Administration.ag" #-}
              _slf
              {-# LINE 833 "Administration.hs" #-}
              )
         _lhsOsvars =
             ({-# LINE 109 "Administration.ag" #-}
              _ptrIsvars
              {-# LINE 838 "Administration.hs" #-}
              )
         ( _ptrIslf,_ptrIsvars) =
             ptr_
     in  ( _lhsOslf,_lhsOsvars))
-- Proc --------------------------------------------------------
data Proc = Proc (String) (([String])) (String) (Stat)
          deriving ( Show)
-- cata
sem_Proc :: Proc ->
            T_Proc
sem_Proc (Proc _name _inp _out _stat) =
    (sem_Proc_Proc _name _inp _out (sem_Stat _stat))
-- semantic domain
type T_Proc = Label ->
              (M.Map String (Label,Label)) ->
              (M.Map String ([Var],Var)) ->
              ( (M.Map Label Block),Flow,InterFlow,Label,(M.Map String ([Var],Var)),(M.Map String (Label,Label)),Proc,(S.Set Var))
data Inh_Proc = Inh_Proc {label_Inh_Proc :: Label,prcs_Inh_Proc :: (M.Map String (Label,Label)),prgs_Inh_Proc :: (M.Map String ([Var],Var))}
data Syn_Proc = Syn_Proc {blocks_Syn_Proc :: (M.Map Label Block),flow_Syn_Proc :: Flow,interflow_Syn_Proc :: InterFlow,maxLabel_Syn_Proc :: Label,pargs_Syn_Proc :: (M.Map String ([Var],Var)),pmap_Syn_Proc :: (M.Map String (Label,Label)),slf_Syn_Proc :: Proc,svars_Syn_Proc :: (S.Set Var)}
wrap_Proc :: T_Proc ->
             Inh_Proc ->
             Syn_Proc
wrap_Proc sem (Inh_Proc _lhsIlabel _lhsIprcs _lhsIprgs) =
    (let ( _lhsOblocks,_lhsOflow,_lhsOinterflow,_lhsOmaxLabel,_lhsOpargs,_lhsOpmap,_lhsOslf,_lhsOsvars) = sem _lhsIlabel _lhsIprcs _lhsIprgs
     in  (Syn_Proc _lhsOblocks _lhsOflow _lhsOinterflow _lhsOmaxLabel _lhsOpargs _lhsOpmap _lhsOslf _lhsOsvars))
sem_Proc_Proc :: String ->
                 ([String]) ->
                 String ->
                 T_Stat ->
                 T_Proc
sem_Proc_Proc name_ inp_ out_ stat_ =
    (\ _lhsIlabel
       _lhsIprcs
       _lhsIprgs ->
         (let _statOlabel :: Label
              _lhsOmaxLabel :: Label
              _lhsOblocks :: (M.Map Label Block)
              _lhsOpmap :: (M.Map String (Label,Label))
              _statOprcs :: (M.Map String (Label,Label))
              _lhsOpargs :: (M.Map String ([Var],Var))
              _statOprgs :: (M.Map String ([Var],Var))
              _lhsOflow :: Flow
              _lhsOinterflow :: InterFlow
              _lhsOsvars :: (S.Set Var)
              _lhsOslf :: Proc
              _statIblocks :: (M.Map Label Block)
              _statIflabels :: ([Label])
              _statIflow :: Flow
              _statIflowLabels :: ([Label])
              _statIinitl :: Label
              _statIinterflow :: InterFlow
              _statImaxLabel :: Label
              _statIslf :: Stat
              _statIsvars :: (S.Set Var)
              _statOlabel =
                  ({-# LINE 93 "Administration.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 896 "Administration.hs" #-}
                   )
              _lhsOmaxLabel =
                  ({-# LINE 94 "Administration.ag" #-}
                   _statImaxLabel + 1
                   {-# LINE 901 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 95 "Administration.ag" #-}
                   M.union (M.union (M.singleton _lhsIlabel _block    ) (M.singleton (_statImaxLabel + 1) B_ProcExit)) _statIblocks
                   {-# LINE 906 "Administration.hs" #-}
                   )
              _block =
                  ({-# LINE 96 "Administration.ag" #-}
                   B_ProcEntry name_ inp_ out_
                   {-# LINE 911 "Administration.hs" #-}
                   )
              _pmap =
                  ({-# LINE 97 "Administration.ag" #-}
                   M.union (M.singleton name_ (_lhsIlabel,_statImaxLabel + 1)) _lhsIprcs
                   {-# LINE 916 "Administration.hs" #-}
                   )
              _lhsOpmap =
                  ({-# LINE 98 "Administration.ag" #-}
                   _pmap
                   {-# LINE 921 "Administration.hs" #-}
                   )
              _statOprcs =
                  ({-# LINE 99 "Administration.ag" #-}
                   _pmap
                   {-# LINE 926 "Administration.hs" #-}
                   )
              _prgs =
                  ({-# LINE 100 "Administration.ag" #-}
                   M.union (M.singleton name_ (inp_,out_)) _lhsIprgs
                   {-# LINE 931 "Administration.hs" #-}
                   )
              _lhsOpargs =
                  ({-# LINE 101 "Administration.ag" #-}
                   _prgs
                   {-# LINE 936 "Administration.hs" #-}
                   )
              _statOprgs =
                  ({-# LINE 102 "Administration.ag" #-}
                   _prgs
                   {-# LINE 941 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 103 "Administration.ag" #-}
                   [(_lhsIlabel,_lhsIlabel + 1)] ++ _statIflow ++ map (\x -> (x,_statImaxLabel + 1)) _statIflabels
                   {-# LINE 946 "Administration.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 104 "Administration.ag" #-}
                   _statIinterflow
                   {-# LINE 951 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 105 "Administration.ag" #-}
                   S.union (S.union (foldr (\x y -> S.insert x y) S.empty inp_) (S.singleton out_)) _statIsvars
                   {-# LINE 956 "Administration.hs" #-}
                   )
              _slf =
                  ({-# LINE 110 "Administration.ag" #-}
                   Proc name_ inp_ out_ _statIslf
                   {-# LINE 961 "Administration.hs" #-}
                   )
              _lhsOslf =
                  ({-# LINE 110 "Administration.ag" #-}
                   _slf
                   {-# LINE 966 "Administration.hs" #-}
                   )
              ( _statIblocks,_statIflabels,_statIflow,_statIflowLabels,_statIinitl,_statIinterflow,_statImaxLabel,_statIslf,_statIsvars) =
                  stat_ _statOlabel _statOprcs _statOprgs
          in  ( _lhsOblocks,_lhsOflow,_lhsOinterflow,_lhsOmaxLabel,_lhsOpargs,_lhsOpmap,_lhsOslf,_lhsOsvars)))
-- Proc' -------------------------------------------------------
data Proc' = Proc' (Int) (Int) (String) (([String])) (String) (Stat')
-- cata
sem_Proc' :: (Proc') ->
             (T_Proc')
sem_Proc' (Proc' _labelEntry _labelReturn _name _inp _out _stat) =
    (sem_Proc'_Proc' _labelEntry _labelReturn _name _inp _out (sem_Stat' _stat))
-- semantic domain
type T_Proc' = ( )
data Inh_Proc' = Inh_Proc' {}
data Syn_Proc' = Syn_Proc' {}
wrap_Proc' :: (T_Proc') ->
              (Inh_Proc') ->
              (Syn_Proc')
wrap_Proc' sem (Inh_Proc') =
    (let ( ) = sem
     in  (Syn_Proc'))
sem_Proc'_Proc' :: Int ->
                   Int ->
                   String ->
                   ([String]) ->
                   String ->
                   (T_Stat') ->
                   (T_Proc')
sem_Proc'_Proc' labelEntry_ labelReturn_ name_ inp_ out_ stat_ =
    (let
     in  ( ))
-- Procs -------------------------------------------------------
type Procs = [Proc]
-- cata
sem_Procs :: Procs ->
             T_Procs
sem_Procs list =
    (Prelude.foldr sem_Procs_Cons sem_Procs_Nil (Prelude.map sem_Proc list))
-- semantic domain
type T_Procs = Label ->
               (M.Map String (Label,Label)) ->
               (M.Map String ([Var],Var)) ->
               ( (M.Map Label Block),Flow,InterFlow,Label,(M.Map String ([Var],Var)),(M.Map String (Label,Label)),Procs,(S.Set Var))
data Inh_Procs = Inh_Procs {label_Inh_Procs :: Label,prcs_Inh_Procs :: (M.Map String (Label,Label)),prgs_Inh_Procs :: (M.Map String ([Var],Var))}
data Syn_Procs = Syn_Procs {blocks_Syn_Procs :: (M.Map Label Block),flow_Syn_Procs :: Flow,interflow_Syn_Procs :: InterFlow,maxLabel_Syn_Procs :: Label,pargs_Syn_Procs :: (M.Map String ([Var],Var)),pmap_Syn_Procs :: (M.Map String (Label,Label)),slf_Syn_Procs :: Procs,svars_Syn_Procs :: (S.Set Var)}
wrap_Procs :: T_Procs ->
              Inh_Procs ->
              Syn_Procs
wrap_Procs sem (Inh_Procs _lhsIlabel _lhsIprcs _lhsIprgs) =
    (let ( _lhsOblocks,_lhsOflow,_lhsOinterflow,_lhsOmaxLabel,_lhsOpargs,_lhsOpmap,_lhsOslf,_lhsOsvars) = sem _lhsIlabel _lhsIprcs _lhsIprgs
     in  (Syn_Procs _lhsOblocks _lhsOflow _lhsOinterflow _lhsOmaxLabel _lhsOpargs _lhsOpmap _lhsOslf _lhsOsvars))
sem_Procs_Cons :: T_Proc ->
                  T_Procs ->
                  T_Procs
sem_Procs_Cons hd_ tl_ =
    (\ _lhsIlabel
       _lhsIprcs
       _lhsIprgs ->
         (let _hdOlabel :: Label
              _tlOlabel :: Label
              _lhsOmaxLabel :: Label
              _lhsOblocks :: (M.Map Label Block)
              _lhsOpmap :: (M.Map String (Label,Label))
              _hdOprcs :: (M.Map String (Label,Label))
              _tlOprcs :: (M.Map String (Label,Label))
              _lhsOpargs :: (M.Map String ([Var],Var))
              _hdOprgs :: (M.Map String ([Var],Var))
              _tlOprgs :: (M.Map String ([Var],Var))
              _lhsOflow :: Flow
              _lhsOinterflow :: InterFlow
              _lhsOsvars :: (S.Set Var)
              _lhsOslf :: Procs
              _hdIblocks :: (M.Map Label Block)
              _hdIflow :: Flow
              _hdIinterflow :: InterFlow
              _hdImaxLabel :: Label
              _hdIpargs :: (M.Map String ([Var],Var))
              _hdIpmap :: (M.Map String (Label,Label))
              _hdIslf :: Proc
              _hdIsvars :: (S.Set Var)
              _tlIblocks :: (M.Map Label Block)
              _tlIflow :: Flow
              _tlIinterflow :: InterFlow
              _tlImaxLabel :: Label
              _tlIpargs :: (M.Map String ([Var],Var))
              _tlIpmap :: (M.Map String (Label,Label))
              _tlIslf :: Procs
              _tlIsvars :: (S.Set Var)
              _hdOlabel =
                  ({-# LINE 71 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1058 "Administration.hs" #-}
                   )
              _tlOlabel =
                  ({-# LINE 72 "Administration.ag" #-}
                   _hdImaxLabel + 1
                   {-# LINE 1063 "Administration.hs" #-}
                   )
              _lhsOmaxLabel =
                  ({-# LINE 73 "Administration.ag" #-}
                   _tlImaxLabel
                   {-# LINE 1068 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 74 "Administration.ag" #-}
                   M.union _hdIblocks _tlIblocks
                   {-# LINE 1073 "Administration.hs" #-}
                   )
              _lhsOpmap =
                  ({-# LINE 75 "Administration.ag" #-}
                   M.union _hdIpmap _tlIpmap
                   {-# LINE 1078 "Administration.hs" #-}
                   )
              _hdOprcs =
                  ({-# LINE 76 "Administration.ag" #-}
                   _lhsIprcs
                   {-# LINE 1083 "Administration.hs" #-}
                   )
              _tlOprcs =
                  ({-# LINE 77 "Administration.ag" #-}
                   _hdIpmap
                   {-# LINE 1088 "Administration.hs" #-}
                   )
              _lhsOpargs =
                  ({-# LINE 78 "Administration.ag" #-}
                   M.union _hdIpargs _tlIpargs
                   {-# LINE 1093 "Administration.hs" #-}
                   )
              _hdOprgs =
                  ({-# LINE 79 "Administration.ag" #-}
                   _lhsIprgs
                   {-# LINE 1098 "Administration.hs" #-}
                   )
              _tlOprgs =
                  ({-# LINE 80 "Administration.ag" #-}
                   _hdIpargs
                   {-# LINE 1103 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 81 "Administration.ag" #-}
                   _hdIflow ++ _tlIflow
                   {-# LINE 1108 "Administration.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 82 "Administration.ag" #-}
                   _hdIinterflow ++ _tlIinterflow
                   {-# LINE 1113 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 83 "Administration.ag" #-}
                   S.union _hdIsvars _tlIsvars
                   {-# LINE 1118 "Administration.hs" #-}
                   )
              _slf =
                  ({-# LINE 110 "Administration.ag" #-}
                   (:) _hdIslf _tlIslf
                   {-# LINE 1123 "Administration.hs" #-}
                   )
              _lhsOslf =
                  ({-# LINE 110 "Administration.ag" #-}
                   _slf
                   {-# LINE 1128 "Administration.hs" #-}
                   )
              ( _hdIblocks,_hdIflow,_hdIinterflow,_hdImaxLabel,_hdIpargs,_hdIpmap,_hdIslf,_hdIsvars) =
                  hd_ _hdOlabel _hdOprcs _hdOprgs
              ( _tlIblocks,_tlIflow,_tlIinterflow,_tlImaxLabel,_tlIpargs,_tlIpmap,_tlIslf,_tlIsvars) =
                  tl_ _tlOlabel _tlOprcs _tlOprgs
          in  ( _lhsOblocks,_lhsOflow,_lhsOinterflow,_lhsOmaxLabel,_lhsOpargs,_lhsOpmap,_lhsOslf,_lhsOsvars)))
sem_Procs_Nil :: T_Procs
sem_Procs_Nil =
    (\ _lhsIlabel
       _lhsIprcs
       _lhsIprgs ->
         (let _lhsOmaxLabel :: Label
              _lhsOblocks :: (M.Map Label Block)
              _lhsOpmap :: (M.Map String (Label,Label))
              _lhsOpargs :: (M.Map String ([Var],Var))
              _lhsOflow :: Flow
              _lhsOinterflow :: InterFlow
              _lhsOsvars :: (S.Set Var)
              _lhsOslf :: Procs
              _lhsOmaxLabel =
                  ({-# LINE 84 "Administration.ag" #-}
                   _lhsIlabel - 1
                   {-# LINE 1151 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 85 "Administration.ag" #-}
                   M.empty
                   {-# LINE 1156 "Administration.hs" #-}
                   )
              _lhsOpmap =
                  ({-# LINE 86 "Administration.ag" #-}
                   M.empty
                   {-# LINE 1161 "Administration.hs" #-}
                   )
              _lhsOpargs =
                  ({-# LINE 87 "Administration.ag" #-}
                   M.empty
                   {-# LINE 1166 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 88 "Administration.ag" #-}
                   []
                   {-# LINE 1171 "Administration.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 89 "Administration.ag" #-}
                   []
                   {-# LINE 1176 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 90 "Administration.ag" #-}
                   S.empty
                   {-# LINE 1181 "Administration.hs" #-}
                   )
              _slf =
                  ({-# LINE 110 "Administration.ag" #-}
                   []
                   {-# LINE 1186 "Administration.hs" #-}
                   )
              _lhsOslf =
                  ({-# LINE 110 "Administration.ag" #-}
                   _slf
                   {-# LINE 1191 "Administration.hs" #-}
                   )
          in  ( _lhsOblocks,_lhsOflow,_lhsOinterflow,_lhsOmaxLabel,_lhsOpargs,_lhsOpmap,_lhsOslf,_lhsOsvars)))
-- Program -----------------------------------------------------
data Program = Program (Procs) (Stat)
             deriving ( Show)
-- cata
sem_Program :: Program ->
               T_Program
sem_Program (Program _procs _stat) =
    (sem_Program_Program (sem_Procs _procs) (sem_Stat _stat))
-- semantic domain
type T_Program = ( ProgramInfo)
data Inh_Program = Inh_Program {}
data Syn_Program = Syn_Program {pinfo_Syn_Program :: ProgramInfo}
wrap_Program :: T_Program ->
                Inh_Program ->
                Syn_Program
wrap_Program sem (Inh_Program) =
    (let ( _lhsOpinfo) = sem
     in  (Syn_Program _lhsOpinfo))
sem_Program_Program :: T_Procs ->
                       T_Stat ->
                       T_Program
sem_Program_Program procs_ stat_ =
    (let _lhsOpinfo :: ProgramInfo
         _procsOlabel :: Label
         _statOlabel :: Label
         _procsOprcs :: (M.Map String (Label,Label))
         _statOprcs :: (M.Map String (Label,Label))
         _procsOprgs :: (M.Map String ([Var],Var))
         _statOprgs :: (M.Map String ([Var],Var))
         _procsIblocks :: (M.Map Label Block)
         _procsIflow :: Flow
         _procsIinterflow :: InterFlow
         _procsImaxLabel :: Label
         _procsIpargs :: (M.Map String ([Var],Var))
         _procsIpmap :: (M.Map String (Label,Label))
         _procsIslf :: Procs
         _procsIsvars :: (S.Set Var)
         _statIblocks :: (M.Map Label Block)
         _statIflabels :: ([Label])
         _statIflow :: Flow
         _statIflowLabels :: ([Label])
         _statIinitl :: Label
         _statIinterflow :: InterFlow
         _statImaxLabel :: Label
         _statIslf :: Stat
         _statIsvars :: (S.Set Var)
         _lhsOpinfo =
             ({-# LINE 40 "Administration.ag" #-}
              ProgramInfo _blocks     [1 .. _statImaxLabel] [_statIinitl] (_statIflabels) _flow     _interflow     (S.toList _svars    )
              {-# LINE 1243 "Administration.hs" #-}
              )
         _blocks =
             ({-# LINE 41 "Administration.ag" #-}
              M.union _procsIblocks _statIblocks
              {-# LINE 1248 "Administration.hs" #-}
              )
         _flow =
             ({-# LINE 42 "Administration.ag" #-}
              _procsIflow ++ _statIflow
              {-# LINE 1253 "Administration.hs" #-}
              )
         _interflow =
             ({-# LINE 43 "Administration.ag" #-}
              _procsIinterflow ++ _statIinterflow
              {-# LINE 1258 "Administration.hs" #-}
              )
         _svars =
             ({-# LINE 44 "Administration.ag" #-}
              S.union _procsIsvars _statIsvars
              {-# LINE 1263 "Administration.hs" #-}
              )
         _procsOlabel =
             ({-# LINE 45 "Administration.ag" #-}
              1
              {-# LINE 1268 "Administration.hs" #-}
              )
         _statOlabel =
             ({-# LINE 46 "Administration.ag" #-}
              _procsImaxLabel + 1
              {-# LINE 1273 "Administration.hs" #-}
              )
         _procsOprcs =
             ({-# LINE 47 "Administration.ag" #-}
              M.empty
              {-# LINE 1278 "Administration.hs" #-}
              )
         _statOprcs =
             ({-# LINE 48 "Administration.ag" #-}
              _procsIpmap
              {-# LINE 1283 "Administration.hs" #-}
              )
         _procsOprgs =
             ({-# LINE 49 "Administration.ag" #-}
              M.empty
              {-# LINE 1288 "Administration.hs" #-}
              )
         _statOprgs =
             ({-# LINE 50 "Administration.ag" #-}
              _procsIpargs
              {-# LINE 1293 "Administration.hs" #-}
              )
         ( _procsIblocks,_procsIflow,_procsIinterflow,_procsImaxLabel,_procsIpargs,_procsIpmap,_procsIslf,_procsIsvars) =
             procs_ _procsOlabel _procsOprcs _procsOprgs
         ( _statIblocks,_statIflabels,_statIflow,_statIflowLabels,_statIinitl,_statIinterflow,_statImaxLabel,_statIslf,_statIsvars) =
             stat_ _statOlabel _statOprcs _statOprgs
     in  ( _lhsOpinfo))
-- Program' ----------------------------------------------------
data Program' = Program' ((Procs')) (Stat')
-- cata
sem_Program' :: (Program') ->
                (T_Program')
sem_Program' (Program' _procs _stat) =
    (sem_Program'_Program' _procs (sem_Stat' _stat))
-- semantic domain
type T_Program' = ( )
data Inh_Program' = Inh_Program' {}
data Syn_Program' = Syn_Program' {}
wrap_Program' :: (T_Program') ->
                 (Inh_Program') ->
                 (Syn_Program')
wrap_Program' sem (Inh_Program') =
    (let ( ) = sem
     in  (Syn_Program'))
sem_Program'_Program' :: (Procs') ->
                         (T_Stat') ->
                         (T_Program')
sem_Program'_Program' procs_ stat_ =
    (let
     in  ( ))
-- Stat --------------------------------------------------------
data Stat = Skip
          | IfThenElse (BExpr) (Stat) (Stat)
          | While (BExpr) (Stat)
          | Call (String) (Exprs) (String)
          | IAssign (String) (IExpr)
          | BAssign (String) (BExpr)
          | Seq (Stat) (Stat)
          deriving ( Show)
-- cata
sem_Stat :: Stat ->
            T_Stat
sem_Stat (Skip) =
    (sem_Stat_Skip)
sem_Stat (IfThenElse _cond _stat1 _stat2) =
    (sem_Stat_IfThenElse (sem_BExpr _cond) (sem_Stat _stat1) (sem_Stat _stat2))
sem_Stat (While _cond _stat) =
    (sem_Stat_While (sem_BExpr _cond) (sem_Stat _stat))
sem_Stat (Call _name _params _out) =
    (sem_Stat_Call _name (sem_Exprs _params) _out)
sem_Stat (IAssign _name _val) =
    (sem_Stat_IAssign _name (sem_IExpr _val))
sem_Stat (BAssign _name _val) =
    (sem_Stat_BAssign _name (sem_BExpr _val))
sem_Stat (Seq _stat1 _stat2) =
    (sem_Stat_Seq (sem_Stat _stat1) (sem_Stat _stat2))
-- semantic domain
type T_Stat = Label ->
              (M.Map String (Label,Label)) ->
              (M.Map String ([Var],Var)) ->
              ( (M.Map Label Block),([Label]),Flow,([Label]),Label,InterFlow,Label,Stat,(S.Set Var))
data Inh_Stat = Inh_Stat {label_Inh_Stat :: Label,prcs_Inh_Stat :: (M.Map String (Label,Label)),prgs_Inh_Stat :: (M.Map String ([Var],Var))}
data Syn_Stat = Syn_Stat {blocks_Syn_Stat :: (M.Map Label Block),flabels_Syn_Stat :: ([Label]),flow_Syn_Stat :: Flow,flowLabels_Syn_Stat :: ([Label]),initl_Syn_Stat :: Label,interflow_Syn_Stat :: InterFlow,maxLabel_Syn_Stat :: Label,slf_Syn_Stat :: Stat,svars_Syn_Stat :: (S.Set Var)}
wrap_Stat :: T_Stat ->
             Inh_Stat ->
             Syn_Stat
wrap_Stat sem (Inh_Stat _lhsIlabel _lhsIprcs _lhsIprgs) =
    (let ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOinterflow,_lhsOmaxLabel,_lhsOslf,_lhsOsvars) = sem _lhsIlabel _lhsIprcs _lhsIprgs
     in  (Syn_Stat _lhsOblocks _lhsOflabels _lhsOflow _lhsOflowLabels _lhsOinitl _lhsOinterflow _lhsOmaxLabel _lhsOslf _lhsOsvars))
sem_Stat_Skip :: T_Stat
sem_Stat_Skip =
    (\ _lhsIlabel
       _lhsIprcs
       _lhsIprgs ->
         (let _lhsOmaxLabel :: Label
              _lhsOflowLabels :: ([Label])
              _lhsOblocks :: (M.Map Label Block)
              _lhsOsvars :: (S.Set Var)
              _lhsOflow :: Flow
              _lhsOinterflow :: InterFlow
              _lhsOinitl :: Label
              _lhsOflabels :: ([Label])
              _lhsOslf :: Stat
              _lhsOmaxLabel =
                  ({-# LINE 113 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1379 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 114 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 1384 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 115 "Administration.ag" #-}
                   M.singleton _lhsIlabel B_Skip
                   {-# LINE 1389 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 116 "Administration.ag" #-}
                   S.empty
                   {-# LINE 1394 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 117 "Administration.ag" #-}
                   []
                   {-# LINE 1399 "Administration.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 118 "Administration.ag" #-}
                   []
                   {-# LINE 1404 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 119 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1409 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 120 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 1414 "Administration.hs" #-}
                   )
              _slf =
                  ({-# LINE 110 "Administration.ag" #-}
                   Skip
                   {-# LINE 1419 "Administration.hs" #-}
                   )
              _lhsOslf =
                  ({-# LINE 110 "Administration.ag" #-}
                   _slf
                   {-# LINE 1424 "Administration.hs" #-}
                   )
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOinterflow,_lhsOmaxLabel,_lhsOslf,_lhsOsvars)))
sem_Stat_IfThenElse :: T_BExpr ->
                       T_Stat ->
                       T_Stat ->
                       T_Stat
sem_Stat_IfThenElse cond_ stat1_ stat2_ =
    (\ _lhsIlabel
       _lhsIprcs
       _lhsIprgs ->
         (let _lhsOmaxLabel :: Label
              _lhsOflowLabels :: ([Label])
              _lhsOflow :: Flow
              _lhsOinterflow :: InterFlow
              _stat1Olabel :: Label
              _stat2Olabel :: Label
              _lhsOblocks :: (M.Map Label Block)
              _lhsOsvars :: (S.Set Var)
              _lhsOinitl :: Label
              _lhsOflabels :: ([Label])
              _stat1Oprcs :: (M.Map String (Label,Label))
              _stat2Oprcs :: (M.Map String (Label,Label))
              _stat1Oprgs :: (M.Map String ([Var],Var))
              _stat2Oprgs :: (M.Map String ([Var],Var))
              _lhsOslf :: Stat
              _condIslf :: BExpr
              _condIsvars :: (S.Set Var)
              _stat1Iblocks :: (M.Map Label Block)
              _stat1Iflabels :: ([Label])
              _stat1Iflow :: Flow
              _stat1IflowLabels :: ([Label])
              _stat1Iinitl :: Label
              _stat1Iinterflow :: InterFlow
              _stat1ImaxLabel :: Label
              _stat1Islf :: Stat
              _stat1Isvars :: (S.Set Var)
              _stat2Iblocks :: (M.Map Label Block)
              _stat2Iflabels :: ([Label])
              _stat2Iflow :: Flow
              _stat2IflowLabels :: ([Label])
              _stat2Iinitl :: Label
              _stat2Iinterflow :: InterFlow
              _stat2ImaxLabel :: Label
              _stat2Islf :: Stat
              _stat2Isvars :: (S.Set Var)
              _lhsOmaxLabel =
                  ({-# LINE 121 "Administration.ag" #-}
                   _stat2ImaxLabel
                   {-# LINE 1473 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 122 "Administration.ag" #-}
                   [_label1    ,_label2    ]
                   {-# LINE 1478 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 123 "Administration.ag" #-}
                   (_lhsIlabel, _label1    ) : (_lhsIlabel, _label2    ) : _stat1Iflow ++ _stat2Iflow
                   {-# LINE 1483 "Administration.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 124 "Administration.ag" #-}
                   _stat1Iinterflow ++ _stat2Iinterflow
                   {-# LINE 1488 "Administration.hs" #-}
                   )
              _stat1Olabel =
                  ({-# LINE 125 "Administration.ag" #-}
                   _label1
                   {-# LINE 1493 "Administration.hs" #-}
                   )
              _label1 =
                  ({-# LINE 126 "Administration.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1498 "Administration.hs" #-}
                   )
              _label2 =
                  ({-# LINE 127 "Administration.ag" #-}
                   _stat1ImaxLabel + 1
                   {-# LINE 1503 "Administration.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 128 "Administration.ag" #-}
                   _label2
                   {-# LINE 1508 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 129 "Administration.ag" #-}
                   M.union (M.union (M.singleton _lhsIlabel (B_Cond _condIslf)) _stat1Iblocks) _stat2Iblocks
                   {-# LINE 1513 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 130 "Administration.ag" #-}
                   S.union _condIsvars (S.union _stat1Isvars _stat2Isvars)
                   {-# LINE 1518 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 131 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1523 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 132 "Administration.ag" #-}
                   [_stat1ImaxLabel,_stat2ImaxLabel]
                   {-# LINE 1528 "Administration.hs" #-}
                   )
              _stat1Oprcs =
                  ({-# LINE 133 "Administration.ag" #-}
                   _lhsIprcs
                   {-# LINE 1533 "Administration.hs" #-}
                   )
              _stat2Oprcs =
                  ({-# LINE 134 "Administration.ag" #-}
                   _lhsIprcs
                   {-# LINE 1538 "Administration.hs" #-}
                   )
              _stat1Oprgs =
                  ({-# LINE 135 "Administration.ag" #-}
                   _lhsIprgs
                   {-# LINE 1543 "Administration.hs" #-}
                   )
              _stat2Oprgs =
                  ({-# LINE 136 "Administration.ag" #-}
                   _lhsIprgs
                   {-# LINE 1548 "Administration.hs" #-}
                   )
              _slf =
                  ({-# LINE 110 "Administration.ag" #-}
                   IfThenElse _condIslf _stat1Islf _stat2Islf
                   {-# LINE 1553 "Administration.hs" #-}
                   )
              _lhsOslf =
                  ({-# LINE 110 "Administration.ag" #-}
                   _slf
                   {-# LINE 1558 "Administration.hs" #-}
                   )
              ( _condIslf,_condIsvars) =
                  cond_
              ( _stat1Iblocks,_stat1Iflabels,_stat1Iflow,_stat1IflowLabels,_stat1Iinitl,_stat1Iinterflow,_stat1ImaxLabel,_stat1Islf,_stat1Isvars) =
                  stat1_ _stat1Olabel _stat1Oprcs _stat1Oprgs
              ( _stat2Iblocks,_stat2Iflabels,_stat2Iflow,_stat2IflowLabels,_stat2Iinitl,_stat2Iinterflow,_stat2ImaxLabel,_stat2Islf,_stat2Isvars) =
                  stat2_ _stat2Olabel _stat2Oprcs _stat2Oprgs
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOinterflow,_lhsOmaxLabel,_lhsOslf,_lhsOsvars)))
sem_Stat_While :: T_BExpr ->
                  T_Stat ->
                  T_Stat
sem_Stat_While cond_ stat_ =
    (\ _lhsIlabel
       _lhsIprcs
       _lhsIprgs ->
         (let _lhsOmaxLabel :: Label
              _lhsOflowLabels :: ([Label])
              _lhsOflow :: Flow
              _lhsOinterflow :: InterFlow
              _statOlabel :: Label
              _lhsOblocks :: (M.Map Label Block)
              _lhsOsvars :: (S.Set Var)
              _lhsOinitl :: Label
              _lhsOflabels :: ([Label])
              _statOprcs :: (M.Map String (Label,Label))
              _statOprgs :: (M.Map String ([Var],Var))
              _lhsOslf :: Stat
              _condIslf :: BExpr
              _condIsvars :: (S.Set Var)
              _statIblocks :: (M.Map Label Block)
              _statIflabels :: ([Label])
              _statIflow :: Flow
              _statIflowLabels :: ([Label])
              _statIinitl :: Label
              _statIinterflow :: InterFlow
              _statImaxLabel :: Label
              _statIslf :: Stat
              _statIsvars :: (S.Set Var)
              _lhsOmaxLabel =
                  ({-# LINE 137 "Administration.ag" #-}
                   _statImaxLabel
                   {-# LINE 1600 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 138 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 1605 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 139 "Administration.ag" #-}
                   (_lhsIlabel, _label1    ) : _statIflow ++ map (\x -> (x,_lhsIlabel)) _statIflabels
                   {-# LINE 1610 "Administration.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 140 "Administration.ag" #-}
                   _statIinterflow
                   {-# LINE 1615 "Administration.hs" #-}
                   )
              _label1 =
                  ({-# LINE 141 "Administration.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1620 "Administration.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 142 "Administration.ag" #-}
                   _label1
                   {-# LINE 1625 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 143 "Administration.ag" #-}
                   M.union (M.singleton _lhsIlabel (B_Cond _condIslf)) _statIblocks
                   {-# LINE 1630 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 144 "Administration.ag" #-}
                   S.union _condIsvars _statIsvars
                   {-# LINE 1635 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 145 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1640 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 146 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 1645 "Administration.hs" #-}
                   )
              _statOprcs =
                  ({-# LINE 147 "Administration.ag" #-}
                   _lhsIprcs
                   {-# LINE 1650 "Administration.hs" #-}
                   )
              _statOprgs =
                  ({-# LINE 148 "Administration.ag" #-}
                   _lhsIprgs
                   {-# LINE 1655 "Administration.hs" #-}
                   )
              _slf =
                  ({-# LINE 110 "Administration.ag" #-}
                   While _condIslf _statIslf
                   {-# LINE 1660 "Administration.hs" #-}
                   )
              _lhsOslf =
                  ({-# LINE 110 "Administration.ag" #-}
                   _slf
                   {-# LINE 1665 "Administration.hs" #-}
                   )
              ( _condIslf,_condIsvars) =
                  cond_
              ( _statIblocks,_statIflabels,_statIflow,_statIflowLabels,_statIinitl,_statIinterflow,_statImaxLabel,_statIslf,_statIsvars) =
                  stat_ _statOlabel _statOprcs _statOprgs
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOinterflow,_lhsOmaxLabel,_lhsOslf,_lhsOsvars)))
sem_Stat_Call :: String ->
                 T_Exprs ->
                 String ->
                 T_Stat
sem_Stat_Call name_ params_ out_ =
    (\ _lhsIlabel
       _lhsIprcs
       _lhsIprgs ->
         (let _lhsOmaxLabel :: Label
              _lhsOflowLabels :: ([Label])
              _lhsOflow :: Flow
              _lhsOinterflow :: InterFlow
              _lhsOblocks :: (M.Map Label Block)
              _lhsOinitl :: Label
              _lhsOflabels :: ([Label])
              _lhsOsvars :: (S.Set Var)
              _lhsOslf :: Stat
              _paramsIslf :: Exprs
              _paramsIsvars :: (S.Set Var)
              _lhsOmaxLabel =
                  ({-# LINE 180 "Administration.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1694 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 181 "Administration.ag" #-}
                   [_lhsIlabel + 1]
                   {-# LINE 1699 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 182 "Administration.ag" #-}
                   [(_lhsIlabel , _pInLabel    ), (_pOutLabel    ,_lhsIlabel + 1)]
                   {-# LINE 1704 "Administration.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 183 "Administration.ag" #-}
                   [(_lhsIlabel,_pInLabel    ,_pOutLabel    ,_lhsIlabel + 1)]
                   {-# LINE 1709 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 184 "Administration.ag" #-}
                   M.union (M.singleton _lhsIlabel _entry    ) (M.singleton (_lhsIlabel + 1) _exit    )
                   {-# LINE 1714 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 185 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1719 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 186 "Administration.ag" #-}
                   [_lhsIlabel + 1]
                   {-# LINE 1724 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 187 "Administration.ag" #-}
                   S.union _paramsIsvars (S.singleton out_)
                   {-# LINE 1729 "Administration.hs" #-}
                   )
              _entry =
                  ({-# LINE 188 "Administration.ag" #-}
                   B_CallEntry name_ _paramsIslf out_ _pargs     _pout
                   {-# LINE 1734 "Administration.hs" #-}
                   )
              _exit =
                  ({-# LINE 189 "Administration.ag" #-}
                   B_CallExit name_ _paramsIslf out_ _pargs     _pout
                   {-# LINE 1739 "Administration.hs" #-}
                   )
              _pInLabel =
                  ({-# LINE 190 "Administration.ag" #-}
                   fst $ fromJust $ M.lookup name_ _lhsIprcs
                   {-# LINE 1744 "Administration.hs" #-}
                   )
              _pOutLabel =
                  ({-# LINE 191 "Administration.ag" #-}
                   snd $ fromJust $ M.lookup name_ _lhsIprcs
                   {-# LINE 1749 "Administration.hs" #-}
                   )
              _pargs =
                  ({-# LINE 192 "Administration.ag" #-}
                   fst $ fromJust $ M.lookup name_ _lhsIprgs
                   {-# LINE 1754 "Administration.hs" #-}
                   )
              _pout =
                  ({-# LINE 193 "Administration.ag" #-}
                   snd $ fromJust $ M.lookup name_ _lhsIprgs
                   {-# LINE 1759 "Administration.hs" #-}
                   )
              _slf =
                  ({-# LINE 110 "Administration.ag" #-}
                   Call name_ _paramsIslf out_
                   {-# LINE 1764 "Administration.hs" #-}
                   )
              _lhsOslf =
                  ({-# LINE 110 "Administration.ag" #-}
                   _slf
                   {-# LINE 1769 "Administration.hs" #-}
                   )
              ( _paramsIslf,_paramsIsvars) =
                  params_
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOinterflow,_lhsOmaxLabel,_lhsOslf,_lhsOsvars)))
sem_Stat_IAssign :: String ->
                    T_IExpr ->
                    T_Stat
sem_Stat_IAssign name_ val_ =
    (\ _lhsIlabel
       _lhsIprcs
       _lhsIprgs ->
         (let _lhsOmaxLabel :: Label
              _lhsOflowLabels :: ([Label])
              _lhsOblocks :: (M.Map Label Block)
              _lhsOsvars :: (S.Set Var)
              _lhsOflow :: Flow
              _lhsOinterflow :: InterFlow
              _lhsOinitl :: Label
              _lhsOflabels :: ([Label])
              _lhsOslf :: Stat
              _valIslf :: IExpr
              _valIsvars :: (S.Set Var)
              _lhsOmaxLabel =
                  ({-# LINE 149 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1795 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 150 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 1800 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 151 "Administration.ag" #-}
                   M.singleton _lhsIlabel $ B_IAssign name_ _valIslf
                   {-# LINE 1805 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 152 "Administration.ag" #-}
                   S.union (S.singleton name_) _valIsvars
                   {-# LINE 1810 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 153 "Administration.ag" #-}
                   []
                   {-# LINE 1815 "Administration.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 154 "Administration.ag" #-}
                   []
                   {-# LINE 1820 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 155 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1825 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 156 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 1830 "Administration.hs" #-}
                   )
              _slf =
                  ({-# LINE 110 "Administration.ag" #-}
                   IAssign name_ _valIslf
                   {-# LINE 1835 "Administration.hs" #-}
                   )
              _lhsOslf =
                  ({-# LINE 110 "Administration.ag" #-}
                   _slf
                   {-# LINE 1840 "Administration.hs" #-}
                   )
              ( _valIslf,_valIsvars) =
                  val_
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOinterflow,_lhsOmaxLabel,_lhsOslf,_lhsOsvars)))
sem_Stat_BAssign :: String ->
                    T_BExpr ->
                    T_Stat
sem_Stat_BAssign name_ val_ =
    (\ _lhsIlabel
       _lhsIprcs
       _lhsIprgs ->
         (let _lhsOmaxLabel :: Label
              _lhsOflowLabels :: ([Label])
              _lhsOblocks :: (M.Map Label Block)
              _lhsOsvars :: (S.Set Var)
              _lhsOflow :: Flow
              _lhsOinterflow :: InterFlow
              _lhsOinitl :: Label
              _lhsOflabels :: ([Label])
              _lhsOslf :: Stat
              _valIslf :: BExpr
              _valIsvars :: (S.Set Var)
              _lhsOmaxLabel =
                  ({-# LINE 157 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1866 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 158 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 1871 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 159 "Administration.ag" #-}
                   M.singleton _lhsIlabel $ B_BAssign name_ _valIslf
                   {-# LINE 1876 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 160 "Administration.ag" #-}
                   S.union (S.singleton name_) _valIsvars
                   {-# LINE 1881 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 161 "Administration.ag" #-}
                   []
                   {-# LINE 1886 "Administration.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 162 "Administration.ag" #-}
                   []
                   {-# LINE 1891 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 163 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1896 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 164 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 1901 "Administration.hs" #-}
                   )
              _slf =
                  ({-# LINE 110 "Administration.ag" #-}
                   BAssign name_ _valIslf
                   {-# LINE 1906 "Administration.hs" #-}
                   )
              _lhsOslf =
                  ({-# LINE 110 "Administration.ag" #-}
                   _slf
                   {-# LINE 1911 "Administration.hs" #-}
                   )
              ( _valIslf,_valIsvars) =
                  val_
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOinterflow,_lhsOmaxLabel,_lhsOslf,_lhsOsvars)))
sem_Stat_Seq :: T_Stat ->
                T_Stat ->
                T_Stat
sem_Stat_Seq stat1_ stat2_ =
    (\ _lhsIlabel
       _lhsIprcs
       _lhsIprgs ->
         (let _lhsOmaxLabel :: Label
              _lhsOflowLabels :: ([Label])
              _lhsOflow :: Flow
              _lhsOinterflow :: InterFlow
              _lhsOblocks :: (M.Map Label Block)
              _stat1Olabel :: Label
              _stat2Olabel :: Label
              _lhsOsvars :: (S.Set Var)
              _lhsOinitl :: Label
              _lhsOflabels :: ([Label])
              _stat1Oprcs :: (M.Map String (Label,Label))
              _stat2Oprcs :: (M.Map String (Label,Label))
              _stat1Oprgs :: (M.Map String ([Var],Var))
              _stat2Oprgs :: (M.Map String ([Var],Var))
              _lhsOslf :: Stat
              _stat1Iblocks :: (M.Map Label Block)
              _stat1Iflabels :: ([Label])
              _stat1Iflow :: Flow
              _stat1IflowLabels :: ([Label])
              _stat1Iinitl :: Label
              _stat1Iinterflow :: InterFlow
              _stat1ImaxLabel :: Label
              _stat1Islf :: Stat
              _stat1Isvars :: (S.Set Var)
              _stat2Iblocks :: (M.Map Label Block)
              _stat2Iflabels :: ([Label])
              _stat2Iflow :: Flow
              _stat2IflowLabels :: ([Label])
              _stat2Iinitl :: Label
              _stat2Iinterflow :: InterFlow
              _stat2ImaxLabel :: Label
              _stat2Islf :: Stat
              _stat2Isvars :: (S.Set Var)
              _lhsOmaxLabel =
                  ({-# LINE 165 "Administration.ag" #-}
                   _stat2ImaxLabel
                   {-# LINE 1959 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 166 "Administration.ag" #-}
                   _stat2IflowLabels
                   {-# LINE 1964 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 167 "Administration.ag" #-}
                   _stat1Iflow ++ map (\x -> (x,_label2    )) _stat1IflowLabels ++ _stat2Iflow
                   {-# LINE 1969 "Administration.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 168 "Administration.ag" #-}
                   _stat1Iinterflow ++ _stat2Iinterflow
                   {-# LINE 1974 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 169 "Administration.ag" #-}
                   M.union _stat1Iblocks _stat2Iblocks
                   {-# LINE 1979 "Administration.hs" #-}
                   )
              _stat1Olabel =
                  ({-# LINE 170 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1984 "Administration.hs" #-}
                   )
              _label2 =
                  ({-# LINE 171 "Administration.ag" #-}
                   _stat1ImaxLabel + 1
                   {-# LINE 1989 "Administration.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 172 "Administration.ag" #-}
                   _label2
                   {-# LINE 1994 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 173 "Administration.ag" #-}
                   S.union _stat1Isvars _stat2Isvars
                   {-# LINE 1999 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 174 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 2004 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 175 "Administration.ag" #-}
                   _stat2Iflabels
                   {-# LINE 2009 "Administration.hs" #-}
                   )
              _stat1Oprcs =
                  ({-# LINE 176 "Administration.ag" #-}
                   _lhsIprcs
                   {-# LINE 2014 "Administration.hs" #-}
                   )
              _stat2Oprcs =
                  ({-# LINE 177 "Administration.ag" #-}
                   _lhsIprcs
                   {-# LINE 2019 "Administration.hs" #-}
                   )
              _stat1Oprgs =
                  ({-# LINE 178 "Administration.ag" #-}
                   _lhsIprgs
                   {-# LINE 2024 "Administration.hs" #-}
                   )
              _stat2Oprgs =
                  ({-# LINE 179 "Administration.ag" #-}
                   _lhsIprgs
                   {-# LINE 2029 "Administration.hs" #-}
                   )
              _slf =
                  ({-# LINE 110 "Administration.ag" #-}
                   Seq _stat1Islf _stat2Islf
                   {-# LINE 2034 "Administration.hs" #-}
                   )
              _lhsOslf =
                  ({-# LINE 110 "Administration.ag" #-}
                   _slf
                   {-# LINE 2039 "Administration.hs" #-}
                   )
              ( _stat1Iblocks,_stat1Iflabels,_stat1Iflow,_stat1IflowLabels,_stat1Iinitl,_stat1Iinterflow,_stat1ImaxLabel,_stat1Islf,_stat1Isvars) =
                  stat1_ _stat1Olabel _stat1Oprcs _stat1Oprgs
              ( _stat2Iblocks,_stat2Iflabels,_stat2Iflow,_stat2IflowLabels,_stat2Iinitl,_stat2Iinterflow,_stat2ImaxLabel,_stat2Islf,_stat2Isvars) =
                  stat2_ _stat2Olabel _stat2Oprcs _stat2Oprgs
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOinterflow,_lhsOmaxLabel,_lhsOslf,_lhsOsvars)))
-- Stat' -------------------------------------------------------
data Stat' = Skip' (Int)
           | IfThenElse' (Int) (BExpr) (Stat') (Stat')
           | While' (Int) (BExpr) (Stat')
           | Call' (Int) (Int) (String) (Exprs) (String)
           | IAssign' (Int) (String) (IExpr)
           | BAssign' (Int) (String) (BExpr)
           | Seq' (Stat') (Stat')
           | Malloc' (Int) (String) (IExpr)
           | Free' (Int) (IExpr)
           | RefAssign' (Int) (IExpr) (IExpr)
           | Continue' (Int)
           | Break' (Int)
-- cata
sem_Stat' :: (Stat') ->
             (T_Stat')
sem_Stat' (Skip' _label) =
    (sem_Stat'_Skip' _label)
sem_Stat' (IfThenElse' _labelc _cond _stat1 _stat2) =
    (sem_Stat'_IfThenElse' _labelc (sem_BExpr _cond) (sem_Stat' _stat1) (sem_Stat' _stat2))
sem_Stat' (While' _labelc _cond _stat) =
    (sem_Stat'_While' _labelc (sem_BExpr _cond) (sem_Stat' _stat))
sem_Stat' (Call' _labelCall _labelExit _name _params _out) =
    (sem_Stat'_Call' _labelCall _labelExit _name (sem_Exprs _params) _out)
sem_Stat' (IAssign' _label _name _val) =
    (sem_Stat'_IAssign' _label _name (sem_IExpr _val))
sem_Stat' (BAssign' _label _name _val) =
    (sem_Stat'_BAssign' _label _name (sem_BExpr _val))
sem_Stat' (Seq' _stat1 _stat2) =
    (sem_Stat'_Seq' (sem_Stat' _stat1) (sem_Stat' _stat2))
sem_Stat' (Malloc' _label _name _size) =
    (sem_Stat'_Malloc' _label _name (sem_IExpr _size))
sem_Stat' (Free' _label _ptr) =
    (sem_Stat'_Free' _label (sem_IExpr _ptr))
sem_Stat' (RefAssign' _label _ptr _val) =
    (sem_Stat'_RefAssign' _label (sem_IExpr _ptr) (sem_IExpr _val))
sem_Stat' (Continue' _label) =
    (sem_Stat'_Continue' _label)
sem_Stat' (Break' _label) =
    (sem_Stat'_Break' _label)
-- semantic domain
type T_Stat' = ( )
data Inh_Stat' = Inh_Stat' {}
data Syn_Stat' = Syn_Stat' {}
wrap_Stat' :: (T_Stat') ->
              (Inh_Stat') ->
              (Syn_Stat')
wrap_Stat' sem (Inh_Stat') =
    (let ( ) = sem
     in  (Syn_Stat'))
sem_Stat'_Skip' :: Int ->
                   (T_Stat')
sem_Stat'_Skip' label_ =
    (let
     in  ( ))
sem_Stat'_IfThenElse' :: Int ->
                         T_BExpr ->
                         (T_Stat') ->
                         (T_Stat') ->
                         (T_Stat')
sem_Stat'_IfThenElse' labelc_ cond_ stat1_ stat2_ =
    (let _condIslf :: BExpr
         _condIsvars :: (S.Set Var)
         ( _condIslf,_condIsvars) =
             cond_
     in  ( ))
sem_Stat'_While' :: Int ->
                    T_BExpr ->
                    (T_Stat') ->
                    (T_Stat')
sem_Stat'_While' labelc_ cond_ stat_ =
    (let _condIslf :: BExpr
         _condIsvars :: (S.Set Var)
         ( _condIslf,_condIsvars) =
             cond_
     in  ( ))
sem_Stat'_Call' :: Int ->
                   Int ->
                   String ->
                   T_Exprs ->
                   String ->
                   (T_Stat')
sem_Stat'_Call' labelCall_ labelExit_ name_ params_ out_ =
    (let _paramsIslf :: Exprs
         _paramsIsvars :: (S.Set Var)
         ( _paramsIslf,_paramsIsvars) =
             params_
     in  ( ))
sem_Stat'_IAssign' :: Int ->
                      String ->
                      T_IExpr ->
                      (T_Stat')
sem_Stat'_IAssign' label_ name_ val_ =
    (let _valIslf :: IExpr
         _valIsvars :: (S.Set Var)
         ( _valIslf,_valIsvars) =
             val_
     in  ( ))
sem_Stat'_BAssign' :: Int ->
                      String ->
                      T_BExpr ->
                      (T_Stat')
sem_Stat'_BAssign' label_ name_ val_ =
    (let _valIslf :: BExpr
         _valIsvars :: (S.Set Var)
         ( _valIslf,_valIsvars) =
             val_
     in  ( ))
sem_Stat'_Seq' :: (T_Stat') ->
                  (T_Stat') ->
                  (T_Stat')
sem_Stat'_Seq' stat1_ stat2_ =
    (let
     in  ( ))
sem_Stat'_Malloc' :: Int ->
                     String ->
                     T_IExpr ->
                     (T_Stat')
sem_Stat'_Malloc' label_ name_ size_ =
    (let _sizeIslf :: IExpr
         _sizeIsvars :: (S.Set Var)
         ( _sizeIslf,_sizeIsvars) =
             size_
     in  ( ))
sem_Stat'_Free' :: Int ->
                   T_IExpr ->
                   (T_Stat')
sem_Stat'_Free' label_ ptr_ =
    (let _ptrIslf :: IExpr
         _ptrIsvars :: (S.Set Var)
         ( _ptrIslf,_ptrIsvars) =
             ptr_
     in  ( ))
sem_Stat'_RefAssign' :: Int ->
                        T_IExpr ->
                        T_IExpr ->
                        (T_Stat')
sem_Stat'_RefAssign' label_ ptr_ val_ =
    (let _ptrIslf :: IExpr
         _ptrIsvars :: (S.Set Var)
         _valIslf :: IExpr
         _valIsvars :: (S.Set Var)
         ( _ptrIslf,_ptrIsvars) =
             ptr_
         ( _valIslf,_valIsvars) =
             val_
     in  ( ))
sem_Stat'_Continue' :: Int ->
                       (T_Stat')
sem_Stat'_Continue' label_ =
    (let
     in  ( ))
sem_Stat'_Break' :: Int ->
                    (T_Stat')
sem_Stat'_Break' label_ =
    (let
     in  ( ))