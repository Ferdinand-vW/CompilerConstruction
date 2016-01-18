

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
                                flow :: Flow, interflow :: InterFlow, vars :: [Var]} deriving (Show)

data Block = 
    B_IAssign {name :: String, valI :: IExpr} |
    B_BAssign {name :: String, valB :: BExpr} |
    B_Cond {cond :: BExpr} |
    B_Skip |
    B_CallEntry {name :: String, params :: Exprs} |
    B_CallExit  {name :: String, out :: String} |
    B_ProcEntry |
    B_ProcExit
    deriving Show

type Label = Int
type Flow = [(Int, Int)]
type InterFlow = [(Int,Int,Int,Int)]
type Var = String
{-# LINE 51 "Administration.hs" #-}

{-# LINE 196 "Administration.ag" #-}


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
             ({-# LINE 185 "Administration.ag" #-}
              S.empty
              {-# LINE 123 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 94 "Administration.ag" #-}
              BConst val_
              {-# LINE 128 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 94 "Administration.ag" #-}
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
             ({-# LINE 186 "Administration.ag" #-}
              S.singleton name_
              {-# LINE 144 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 94 "Administration.ag" #-}
              BVar name_
              {-# LINE 149 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 94 "Administration.ag" #-}
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
             ({-# LINE 187 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 170 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 94 "Administration.ag" #-}
              LessThan _leftIslf _rightIslf
              {-# LINE 175 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 94 "Administration.ag" #-}
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
             ({-# LINE 188 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 200 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 94 "Administration.ag" #-}
              GreaterThan _leftIslf _rightIslf
              {-# LINE 205 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 94 "Administration.ag" #-}
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
             ({-# LINE 189 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 230 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 94 "Administration.ag" #-}
              LessEqual _leftIslf _rightIslf
              {-# LINE 235 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 94 "Administration.ag" #-}
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
             ({-# LINE 190 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 260 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 94 "Administration.ag" #-}
              GreaterEqual _leftIslf _rightIslf
              {-# LINE 265 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 94 "Administration.ag" #-}
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
             ({-# LINE 191 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 290 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 94 "Administration.ag" #-}
              IEqual _leftIslf _rightIslf
              {-# LINE 295 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 94 "Administration.ag" #-}
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
             ({-# LINE 192 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 320 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 94 "Administration.ag" #-}
              BEqual _leftIslf _rightIslf
              {-# LINE 325 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 94 "Administration.ag" #-}
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
             ({-# LINE 193 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 350 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 94 "Administration.ag" #-}
              And _leftIslf _rightIslf
              {-# LINE 355 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 94 "Administration.ag" #-}
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
             ({-# LINE 94 "Administration.ag" #-}
              Or _leftIslf _rightIslf
              {-# LINE 380 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 94 "Administration.ag" #-}
              _slf
              {-# LINE 385 "Administration.hs" #-}
              )
         _lhsOsvars =
             ({-# LINE 93 "Administration.ag" #-}
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
             ({-# LINE 194 "Administration.ag" #-}
              _valIsvars
              {-# LINE 407 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 94 "Administration.ag" #-}
              Not _valIslf
              {-# LINE 412 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 94 "Administration.ag" #-}
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
             ({-# LINE 173 "Administration.ag" #-}
              _exprIsvars
              {-# LINE 513 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 94 "Administration.ag" #-}
              B _exprIslf
              {-# LINE 518 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 94 "Administration.ag" #-}
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
             ({-# LINE 174 "Administration.ag" #-}
              _exprIsvars
              {-# LINE 538 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 94 "Administration.ag" #-}
              I _exprIslf
              {-# LINE 543 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 94 "Administration.ag" #-}
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
             ({-# LINE 169 "Administration.ag" #-}
              S.union _hdIsvars _tlIsvars
              {-# LINE 583 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 94 "Administration.ag" #-}
              (:) _hdIslf _tlIslf
              {-# LINE 588 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 94 "Administration.ag" #-}
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
             ({-# LINE 170 "Administration.ag" #-}
              S.empty
              {-# LINE 607 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 94 "Administration.ag" #-}
              []
              {-# LINE 612 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 94 "Administration.ag" #-}
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
             ({-# LINE 177 "Administration.ag" #-}
              S.empty
              {-# LINE 664 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 94 "Administration.ag" #-}
              IConst val_
              {-# LINE 669 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 94 "Administration.ag" #-}
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
             ({-# LINE 178 "Administration.ag" #-}
              S.singleton name_
              {-# LINE 685 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 94 "Administration.ag" #-}
              Var name_
              {-# LINE 690 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 94 "Administration.ag" #-}
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
             ({-# LINE 179 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 711 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 94 "Administration.ag" #-}
              Plus _leftIslf _rightIslf
              {-# LINE 716 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 94 "Administration.ag" #-}
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
             ({-# LINE 180 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 741 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 94 "Administration.ag" #-}
              Minus _leftIslf _rightIslf
              {-# LINE 746 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 94 "Administration.ag" #-}
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
             ({-# LINE 181 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 771 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 94 "Administration.ag" #-}
              Times _leftIslf _rightIslf
              {-# LINE 776 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 94 "Administration.ag" #-}
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
             ({-# LINE 182 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 801 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 94 "Administration.ag" #-}
              Divide _leftIslf _rightIslf
              {-# LINE 806 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 94 "Administration.ag" #-}
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
             ({-# LINE 94 "Administration.ag" #-}
              Deref _ptrIslf
              {-# LINE 828 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 94 "Administration.ag" #-}
              _slf
              {-# LINE 833 "Administration.hs" #-}
              )
         _lhsOsvars =
             ({-# LINE 93 "Administration.ag" #-}
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
              ( (M.Map Label Block),Flow,InterFlow,Label,(M.Map String (Label,Label)))
data Inh_Proc = Inh_Proc {label_Inh_Proc :: Label,prcs_Inh_Proc :: (M.Map String (Label,Label))}
data Syn_Proc = Syn_Proc {blocks_Syn_Proc :: (M.Map Label Block),flow_Syn_Proc :: Flow,interflow_Syn_Proc :: InterFlow,maxLabel_Syn_Proc :: Label,pmap_Syn_Proc :: (M.Map String (Label,Label))}
wrap_Proc :: T_Proc ->
             Inh_Proc ->
             Syn_Proc
wrap_Proc sem (Inh_Proc _lhsIlabel _lhsIprcs) =
    (let ( _lhsOblocks,_lhsOflow,_lhsOinterflow,_lhsOmaxLabel,_lhsOpmap) = sem _lhsIlabel _lhsIprcs
     in  (Syn_Proc _lhsOblocks _lhsOflow _lhsOinterflow _lhsOmaxLabel _lhsOpmap))
sem_Proc_Proc :: String ->
                 ([String]) ->
                 String ->
                 T_Stat ->
                 T_Proc
sem_Proc_Proc name_ inp_ out_ stat_ =
    (\ _lhsIlabel
       _lhsIprcs ->
         (let _statOlabel :: Label
              _lhsOmaxLabel :: Label
              _lhsOblocks :: (M.Map Label Block)
              _lhsOpmap :: (M.Map String (Label,Label))
              _statOprcs :: (M.Map String (Label,Label))
              _lhsOflow :: Flow
              _lhsOinterflow :: InterFlow
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
                  ({-# LINE 82 "Administration.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 890 "Administration.hs" #-}
                   )
              _lhsOmaxLabel =
                  ({-# LINE 83 "Administration.ag" #-}
                   _statImaxLabel + 1
                   {-# LINE 895 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 84 "Administration.ag" #-}
                   M.union (M.union (M.singleton _lhsIlabel B_ProcEntry) (M.singleton _statImaxLabel B_ProcExit)) _statIblocks
                   {-# LINE 900 "Administration.hs" #-}
                   )
              _pmap =
                  ({-# LINE 85 "Administration.ag" #-}
                   M.singleton name_ (_lhsIlabel,_statImaxLabel + 1)
                   {-# LINE 905 "Administration.hs" #-}
                   )
              _lhsOpmap =
                  ({-# LINE 86 "Administration.ag" #-}
                   _pmap
                   {-# LINE 910 "Administration.hs" #-}
                   )
              _statOprcs =
                  ({-# LINE 87 "Administration.ag" #-}
                   _pmap
                   {-# LINE 915 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 88 "Administration.ag" #-}
                   [(_lhsIlabel,_lhsIlabel + 1)] ++ _statIflow ++ map (\x -> (x,_statImaxLabel + 1)) _statIflabels
                   {-# LINE 920 "Administration.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 89 "Administration.ag" #-}
                   _statIinterflow
                   {-# LINE 925 "Administration.hs" #-}
                   )
              ( _statIblocks,_statIflabels,_statIflow,_statIflowLabels,_statIinitl,_statIinterflow,_statImaxLabel,_statIslf,_statIsvars) =
                  stat_ _statOlabel _statOprcs
          in  ( _lhsOblocks,_lhsOflow,_lhsOinterflow,_lhsOmaxLabel,_lhsOpmap)))
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
               ( (M.Map Label Block),Flow,InterFlow,Label,(M.Map String (Label,Label)))
data Inh_Procs = Inh_Procs {label_Inh_Procs :: Label,prcs_Inh_Procs :: (M.Map String (Label,Label))}
data Syn_Procs = Syn_Procs {blocks_Syn_Procs :: (M.Map Label Block),flow_Syn_Procs :: Flow,interflow_Syn_Procs :: InterFlow,maxLabel_Syn_Procs :: Label,pmap_Syn_Procs :: (M.Map String (Label,Label))}
wrap_Procs :: T_Procs ->
              Inh_Procs ->
              Syn_Procs
wrap_Procs sem (Inh_Procs _lhsIlabel _lhsIprcs) =
    (let ( _lhsOblocks,_lhsOflow,_lhsOinterflow,_lhsOmaxLabel,_lhsOpmap) = sem _lhsIlabel _lhsIprcs
     in  (Syn_Procs _lhsOblocks _lhsOflow _lhsOinterflow _lhsOmaxLabel _lhsOpmap))
sem_Procs_Cons :: T_Proc ->
                  T_Procs ->
                  T_Procs
sem_Procs_Cons hd_ tl_ =
    (\ _lhsIlabel
       _lhsIprcs ->
         (let _hdOlabel :: Label
              _tlOlabel :: Label
              _lhsOmaxLabel :: Label
              _lhsOblocks :: (M.Map Label Block)
              _lhsOpmap :: (M.Map String (Label,Label))
              _hdOprcs :: (M.Map String (Label,Label))
              _tlOprcs :: (M.Map String (Label,Label))
              _lhsOflow :: Flow
              _lhsOinterflow :: InterFlow
              _hdIblocks :: (M.Map Label Block)
              _hdIflow :: Flow
              _hdIinterflow :: InterFlow
              _hdImaxLabel :: Label
              _hdIpmap :: (M.Map String (Label,Label))
              _tlIblocks :: (M.Map Label Block)
              _tlIflow :: Flow
              _tlIinterflow :: InterFlow
              _tlImaxLabel :: Label
              _tlIpmap :: (M.Map String (Label,Label))
              _hdOlabel =
                  ({-# LINE 66 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1004 "Administration.hs" #-}
                   )
              _tlOlabel =
                  ({-# LINE 67 "Administration.ag" #-}
                   _hdImaxLabel + 1
                   {-# LINE 1009 "Administration.hs" #-}
                   )
              _lhsOmaxLabel =
                  ({-# LINE 68 "Administration.ag" #-}
                   _hdImaxLabel
                   {-# LINE 1014 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 69 "Administration.ag" #-}
                   M.union _hdIblocks _tlIblocks
                   {-# LINE 1019 "Administration.hs" #-}
                   )
              _lhsOpmap =
                  ({-# LINE 70 "Administration.ag" #-}
                   M.union _hdIpmap _tlIpmap
                   {-# LINE 1024 "Administration.hs" #-}
                   )
              _hdOprcs =
                  ({-# LINE 71 "Administration.ag" #-}
                   _lhsIprcs
                   {-# LINE 1029 "Administration.hs" #-}
                   )
              _tlOprcs =
                  ({-# LINE 72 "Administration.ag" #-}
                   _hdIpmap
                   {-# LINE 1034 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 73 "Administration.ag" #-}
                   _hdIflow ++ _tlIflow
                   {-# LINE 1039 "Administration.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 74 "Administration.ag" #-}
                   _hdIinterflow ++ _tlIinterflow
                   {-# LINE 1044 "Administration.hs" #-}
                   )
              ( _hdIblocks,_hdIflow,_hdIinterflow,_hdImaxLabel,_hdIpmap) =
                  hd_ _hdOlabel _hdOprcs
              ( _tlIblocks,_tlIflow,_tlIinterflow,_tlImaxLabel,_tlIpmap) =
                  tl_ _tlOlabel _tlOprcs
          in  ( _lhsOblocks,_lhsOflow,_lhsOinterflow,_lhsOmaxLabel,_lhsOpmap)))
sem_Procs_Nil :: T_Procs
sem_Procs_Nil =
    (\ _lhsIlabel
       _lhsIprcs ->
         (let _lhsOmaxLabel :: Label
              _lhsOblocks :: (M.Map Label Block)
              _lhsOpmap :: (M.Map String (Label,Label))
              _lhsOflow :: Flow
              _lhsOinterflow :: InterFlow
              _lhsOmaxLabel =
                  ({-# LINE 75 "Administration.ag" #-}
                   _lhsIlabel - 1
                   {-# LINE 1063 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 76 "Administration.ag" #-}
                   M.empty
                   {-# LINE 1068 "Administration.hs" #-}
                   )
              _lhsOpmap =
                  ({-# LINE 77 "Administration.ag" #-}
                   M.empty
                   {-# LINE 1073 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 78 "Administration.ag" #-}
                   []
                   {-# LINE 1078 "Administration.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 79 "Administration.ag" #-}
                   []
                   {-# LINE 1083 "Administration.hs" #-}
                   )
          in  ( _lhsOblocks,_lhsOflow,_lhsOinterflow,_lhsOmaxLabel,_lhsOpmap)))
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
         _procsIblocks :: (M.Map Label Block)
         _procsIflow :: Flow
         _procsIinterflow :: InterFlow
         _procsImaxLabel :: Label
         _procsIpmap :: (M.Map String (Label,Label))
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
              ProgramInfo _blocks     [1 .. _statImaxLabel] [_statIinitl] (_statIflabels) _flow     _interflow     (S.toList _statIsvars)
              {-# LINE 1130 "Administration.hs" #-}
              )
         _blocks =
             ({-# LINE 41 "Administration.ag" #-}
              M.union _procsIblocks _statIblocks
              {-# LINE 1135 "Administration.hs" #-}
              )
         _flow =
             ({-# LINE 42 "Administration.ag" #-}
              _procsIflow ++ _statIflow
              {-# LINE 1140 "Administration.hs" #-}
              )
         _interflow =
             ({-# LINE 43 "Administration.ag" #-}
              _procsIinterflow ++ _statIinterflow
              {-# LINE 1145 "Administration.hs" #-}
              )
         _procsOlabel =
             ({-# LINE 44 "Administration.ag" #-}
              1
              {-# LINE 1150 "Administration.hs" #-}
              )
         _statOlabel =
             ({-# LINE 45 "Administration.ag" #-}
              _procsImaxLabel + 1
              {-# LINE 1155 "Administration.hs" #-}
              )
         _procsOprcs =
             ({-# LINE 46 "Administration.ag" #-}
              M.empty
              {-# LINE 1160 "Administration.hs" #-}
              )
         _statOprcs =
             ({-# LINE 47 "Administration.ag" #-}
              _procsIpmap
              {-# LINE 1165 "Administration.hs" #-}
              )
         ( _procsIblocks,_procsIflow,_procsIinterflow,_procsImaxLabel,_procsIpmap) =
             procs_ _procsOlabel _procsOprcs
         ( _statIblocks,_statIflabels,_statIflow,_statIflowLabels,_statIinitl,_statIinterflow,_statImaxLabel,_statIslf,_statIsvars) =
             stat_ _statOlabel _statOprcs
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
              ( (M.Map Label Block),([Label]),Flow,([Label]),Label,InterFlow,Label,Stat,(S.Set Var))
data Inh_Stat = Inh_Stat {label_Inh_Stat :: Label,prcs_Inh_Stat :: (M.Map String (Label,Label))}
data Syn_Stat = Syn_Stat {blocks_Syn_Stat :: (M.Map Label Block),flabels_Syn_Stat :: ([Label]),flow_Syn_Stat :: Flow,flowLabels_Syn_Stat :: ([Label]),initl_Syn_Stat :: Label,interflow_Syn_Stat :: InterFlow,maxLabel_Syn_Stat :: Label,slf_Syn_Stat :: Stat,svars_Syn_Stat :: (S.Set Var)}
wrap_Stat :: T_Stat ->
             Inh_Stat ->
             Syn_Stat
wrap_Stat sem (Inh_Stat _lhsIlabel _lhsIprcs) =
    (let ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOinterflow,_lhsOmaxLabel,_lhsOslf,_lhsOsvars) = sem _lhsIlabel _lhsIprcs
     in  (Syn_Stat _lhsOblocks _lhsOflabels _lhsOflow _lhsOflowLabels _lhsOinitl _lhsOinterflow _lhsOmaxLabel _lhsOslf _lhsOsvars))
sem_Stat_Skip :: T_Stat
sem_Stat_Skip =
    (\ _lhsIlabel
       _lhsIprcs ->
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
                  ({-# LINE 97 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1249 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 98 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 1254 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 99 "Administration.ag" #-}
                   M.singleton _lhsIlabel B_Skip
                   {-# LINE 1259 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 100 "Administration.ag" #-}
                   S.empty
                   {-# LINE 1264 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 101 "Administration.ag" #-}
                   []
                   {-# LINE 1269 "Administration.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 102 "Administration.ag" #-}
                   []
                   {-# LINE 1274 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 103 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1279 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 104 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 1284 "Administration.hs" #-}
                   )
              _slf =
                  ({-# LINE 94 "Administration.ag" #-}
                   Skip
                   {-# LINE 1289 "Administration.hs" #-}
                   )
              _lhsOslf =
                  ({-# LINE 94 "Administration.ag" #-}
                   _slf
                   {-# LINE 1294 "Administration.hs" #-}
                   )
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOinterflow,_lhsOmaxLabel,_lhsOslf,_lhsOsvars)))
sem_Stat_IfThenElse :: T_BExpr ->
                       T_Stat ->
                       T_Stat ->
                       T_Stat
sem_Stat_IfThenElse cond_ stat1_ stat2_ =
    (\ _lhsIlabel
       _lhsIprcs ->
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
                  ({-# LINE 105 "Administration.ag" #-}
                   _stat2ImaxLabel
                   {-# LINE 1340 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 106 "Administration.ag" #-}
                   [_label1    ,_label2    ]
                   {-# LINE 1345 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 107 "Administration.ag" #-}
                   (_lhsIlabel, _label1    ) : (_lhsIlabel, _label2    ) : _stat1Iflow ++ _stat2Iflow
                   {-# LINE 1350 "Administration.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 108 "Administration.ag" #-}
                   _stat1Iinterflow ++ _stat2Iinterflow
                   {-# LINE 1355 "Administration.hs" #-}
                   )
              _stat1Olabel =
                  ({-# LINE 109 "Administration.ag" #-}
                   _label1
                   {-# LINE 1360 "Administration.hs" #-}
                   )
              _label1 =
                  ({-# LINE 110 "Administration.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1365 "Administration.hs" #-}
                   )
              _label2 =
                  ({-# LINE 111 "Administration.ag" #-}
                   _stat1ImaxLabel + 1
                   {-# LINE 1370 "Administration.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 112 "Administration.ag" #-}
                   _label2
                   {-# LINE 1375 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 113 "Administration.ag" #-}
                   M.union (M.union (M.singleton _lhsIlabel (B_Cond _condIslf)) _stat1Iblocks) _stat2Iblocks
                   {-# LINE 1380 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 114 "Administration.ag" #-}
                   S.union _condIsvars (S.union _stat1Isvars _stat2Isvars)
                   {-# LINE 1385 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 115 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1390 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 116 "Administration.ag" #-}
                   [_stat1ImaxLabel,_stat2ImaxLabel]
                   {-# LINE 1395 "Administration.hs" #-}
                   )
              _stat1Oprcs =
                  ({-# LINE 117 "Administration.ag" #-}
                   _lhsIprcs
                   {-# LINE 1400 "Administration.hs" #-}
                   )
              _stat2Oprcs =
                  ({-# LINE 118 "Administration.ag" #-}
                   _lhsIprcs
                   {-# LINE 1405 "Administration.hs" #-}
                   )
              _slf =
                  ({-# LINE 94 "Administration.ag" #-}
                   IfThenElse _condIslf _stat1Islf _stat2Islf
                   {-# LINE 1410 "Administration.hs" #-}
                   )
              _lhsOslf =
                  ({-# LINE 94 "Administration.ag" #-}
                   _slf
                   {-# LINE 1415 "Administration.hs" #-}
                   )
              ( _condIslf,_condIsvars) =
                  cond_
              ( _stat1Iblocks,_stat1Iflabels,_stat1Iflow,_stat1IflowLabels,_stat1Iinitl,_stat1Iinterflow,_stat1ImaxLabel,_stat1Islf,_stat1Isvars) =
                  stat1_ _stat1Olabel _stat1Oprcs
              ( _stat2Iblocks,_stat2Iflabels,_stat2Iflow,_stat2IflowLabels,_stat2Iinitl,_stat2Iinterflow,_stat2ImaxLabel,_stat2Islf,_stat2Isvars) =
                  stat2_ _stat2Olabel _stat2Oprcs
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOinterflow,_lhsOmaxLabel,_lhsOslf,_lhsOsvars)))
sem_Stat_While :: T_BExpr ->
                  T_Stat ->
                  T_Stat
sem_Stat_While cond_ stat_ =
    (\ _lhsIlabel
       _lhsIprcs ->
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
                  ({-# LINE 119 "Administration.ag" #-}
                   _statImaxLabel
                   {-# LINE 1455 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 120 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 1460 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 121 "Administration.ag" #-}
                   (_lhsIlabel, _label1    ) : _statIflow ++ map (\x -> (x,_lhsIlabel)) _statIflabels
                   {-# LINE 1465 "Administration.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 122 "Administration.ag" #-}
                   _statIinterflow
                   {-# LINE 1470 "Administration.hs" #-}
                   )
              _label1 =
                  ({-# LINE 123 "Administration.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1475 "Administration.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 124 "Administration.ag" #-}
                   _label1
                   {-# LINE 1480 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 125 "Administration.ag" #-}
                   M.union (M.singleton _lhsIlabel (B_Cond _condIslf)) _statIblocks
                   {-# LINE 1485 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 126 "Administration.ag" #-}
                   S.union _condIsvars _statIsvars
                   {-# LINE 1490 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 127 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1495 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 128 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 1500 "Administration.hs" #-}
                   )
              _statOprcs =
                  ({-# LINE 129 "Administration.ag" #-}
                   _lhsIprcs
                   {-# LINE 1505 "Administration.hs" #-}
                   )
              _slf =
                  ({-# LINE 94 "Administration.ag" #-}
                   While _condIslf _statIslf
                   {-# LINE 1510 "Administration.hs" #-}
                   )
              _lhsOslf =
                  ({-# LINE 94 "Administration.ag" #-}
                   _slf
                   {-# LINE 1515 "Administration.hs" #-}
                   )
              ( _condIslf,_condIsvars) =
                  cond_
              ( _statIblocks,_statIflabels,_statIflow,_statIflowLabels,_statIinitl,_statIinterflow,_statImaxLabel,_statIslf,_statIsvars) =
                  stat_ _statOlabel _statOprcs
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOinterflow,_lhsOmaxLabel,_lhsOslf,_lhsOsvars)))
sem_Stat_Call :: String ->
                 T_Exprs ->
                 String ->
                 T_Stat
sem_Stat_Call name_ params_ out_ =
    (\ _lhsIlabel
       _lhsIprcs ->
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
                  ({-# LINE 159 "Administration.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1543 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 160 "Administration.ag" #-}
                   [_lhsIlabel + 1]
                   {-# LINE 1548 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 161 "Administration.ag" #-}
                   [(_lhsIlabel , fst $ fromJust $ M.lookup name_ _lhsIprcs), (snd $ fromJust $ M.lookup name_ _lhsIprcs,_lhsIlabel + 1)]
                   {-# LINE 1553 "Administration.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 162 "Administration.ag" #-}
                   [(_lhsIlabel,fst $ fromJust $ M.lookup name_ _lhsIprcs,snd $ fromJust $ M.lookup name_ _lhsIprcs,_lhsIlabel + 1)]
                   {-# LINE 1558 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 163 "Administration.ag" #-}
                   M.union (M.singleton _lhsIlabel (B_CallEntry name_ _paramsIslf)) (M.singleton (_lhsIlabel + 1) (B_CallExit name_ out_))
                   {-# LINE 1563 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 164 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1568 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 165 "Administration.ag" #-}
                   [_lhsIlabel + 1]
                   {-# LINE 1573 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 166 "Administration.ag" #-}
                   S.union _paramsIsvars (S.singleton out_)
                   {-# LINE 1578 "Administration.hs" #-}
                   )
              _slf =
                  ({-# LINE 94 "Administration.ag" #-}
                   Call name_ _paramsIslf out_
                   {-# LINE 1583 "Administration.hs" #-}
                   )
              _lhsOslf =
                  ({-# LINE 94 "Administration.ag" #-}
                   _slf
                   {-# LINE 1588 "Administration.hs" #-}
                   )
              ( _paramsIslf,_paramsIsvars) =
                  params_
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOinterflow,_lhsOmaxLabel,_lhsOslf,_lhsOsvars)))
sem_Stat_IAssign :: String ->
                    T_IExpr ->
                    T_Stat
sem_Stat_IAssign name_ val_ =
    (\ _lhsIlabel
       _lhsIprcs ->
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
                  ({-# LINE 130 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1613 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 131 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 1618 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 132 "Administration.ag" #-}
                   M.singleton _lhsIlabel $ B_IAssign name_ _valIslf
                   {-# LINE 1623 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 133 "Administration.ag" #-}
                   S.union (S.singleton name_) _valIsvars
                   {-# LINE 1628 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 134 "Administration.ag" #-}
                   []
                   {-# LINE 1633 "Administration.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 135 "Administration.ag" #-}
                   []
                   {-# LINE 1638 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 136 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1643 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 137 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 1648 "Administration.hs" #-}
                   )
              _slf =
                  ({-# LINE 94 "Administration.ag" #-}
                   IAssign name_ _valIslf
                   {-# LINE 1653 "Administration.hs" #-}
                   )
              _lhsOslf =
                  ({-# LINE 94 "Administration.ag" #-}
                   _slf
                   {-# LINE 1658 "Administration.hs" #-}
                   )
              ( _valIslf,_valIsvars) =
                  val_
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOinterflow,_lhsOmaxLabel,_lhsOslf,_lhsOsvars)))
sem_Stat_BAssign :: String ->
                    T_BExpr ->
                    T_Stat
sem_Stat_BAssign name_ val_ =
    (\ _lhsIlabel
       _lhsIprcs ->
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
                  ({-# LINE 138 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1683 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 139 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 1688 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 140 "Administration.ag" #-}
                   M.singleton _lhsIlabel $ B_BAssign name_ _valIslf
                   {-# LINE 1693 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 141 "Administration.ag" #-}
                   S.union (S.singleton name_) _valIsvars
                   {-# LINE 1698 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 142 "Administration.ag" #-}
                   []
                   {-# LINE 1703 "Administration.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 143 "Administration.ag" #-}
                   []
                   {-# LINE 1708 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 144 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1713 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 145 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 1718 "Administration.hs" #-}
                   )
              _slf =
                  ({-# LINE 94 "Administration.ag" #-}
                   BAssign name_ _valIslf
                   {-# LINE 1723 "Administration.hs" #-}
                   )
              _lhsOslf =
                  ({-# LINE 94 "Administration.ag" #-}
                   _slf
                   {-# LINE 1728 "Administration.hs" #-}
                   )
              ( _valIslf,_valIsvars) =
                  val_
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOinterflow,_lhsOmaxLabel,_lhsOslf,_lhsOsvars)))
sem_Stat_Seq :: T_Stat ->
                T_Stat ->
                T_Stat
sem_Stat_Seq stat1_ stat2_ =
    (\ _lhsIlabel
       _lhsIprcs ->
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
                  ({-# LINE 146 "Administration.ag" #-}
                   _stat2ImaxLabel
                   {-# LINE 1773 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 147 "Administration.ag" #-}
                   _stat2IflowLabels
                   {-# LINE 1778 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 148 "Administration.ag" #-}
                   _stat1Iflow ++ map (\x -> (x,_label2    )) _stat1IflowLabels ++ _stat2Iflow
                   {-# LINE 1783 "Administration.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 149 "Administration.ag" #-}
                   _stat1Iinterflow ++ _stat2Iinterflow
                   {-# LINE 1788 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 150 "Administration.ag" #-}
                   M.union _stat1Iblocks _stat2Iblocks
                   {-# LINE 1793 "Administration.hs" #-}
                   )
              _stat1Olabel =
                  ({-# LINE 151 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1798 "Administration.hs" #-}
                   )
              _label2 =
                  ({-# LINE 152 "Administration.ag" #-}
                   _stat1ImaxLabel + 1
                   {-# LINE 1803 "Administration.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 153 "Administration.ag" #-}
                   _label2
                   {-# LINE 1808 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 154 "Administration.ag" #-}
                   S.union _stat1Isvars _stat2Isvars
                   {-# LINE 1813 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 155 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1818 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 156 "Administration.ag" #-}
                   _stat2Iflabels
                   {-# LINE 1823 "Administration.hs" #-}
                   )
              _stat1Oprcs =
                  ({-# LINE 157 "Administration.ag" #-}
                   _lhsIprcs
                   {-# LINE 1828 "Administration.hs" #-}
                   )
              _stat2Oprcs =
                  ({-# LINE 158 "Administration.ag" #-}
                   _lhsIprcs
                   {-# LINE 1833 "Administration.hs" #-}
                   )
              _slf =
                  ({-# LINE 94 "Administration.ag" #-}
                   Seq _stat1Islf _stat2Islf
                   {-# LINE 1838 "Administration.hs" #-}
                   )
              _lhsOslf =
                  ({-# LINE 94 "Administration.ag" #-}
                   _slf
                   {-# LINE 1843 "Administration.hs" #-}
                   )
              ( _stat1Iblocks,_stat1Iflabels,_stat1Iflow,_stat1IflowLabels,_stat1Iinitl,_stat1Iinterflow,_stat1ImaxLabel,_stat1Islf,_stat1Isvars) =
                  stat1_ _stat1Olabel _stat1Oprcs
              ( _stat2Iblocks,_stat2Iflabels,_stat2Iflow,_stat2IflowLabels,_stat2Iinitl,_stat2Iinterflow,_stat2ImaxLabel,_stat2Islf,_stat2Isvars) =
                  stat2_ _stat2Olabel _stat2Oprcs
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