

-- UUAGC 0.9.52.1 (Administration)
module Administration where
{-# LINE 4 "Administration.ag" #-}

import qualified Data.Map as M
import qualified Data.Set as S
{-# LINE 10 "Administration.hs" #-}
{-# LINE 1 "AttributeGrammar.ag" #-}

--import qualified Data.Map as M
--import qualified Data.Maybe as Maybe
--import qualified Data.List as L
{-# LINE 16 "Administration.hs" #-}

{-# LINE 83 "AttributeGrammar.ag" #-}

type Procs = [Proc]
type Procs' = [Proc']
type Exprs = [Expr]
{-# LINE 23 "Administration.hs" #-}

{-# LINE 9 "Administration.ag" #-}



toProgramInfo :: Program -> ProgramInfo
toProgramInfo program = pinfo_Syn_Program $ wrap_Program (sem_Program program) (Inh_Program)

data ProgramInfo = ProgramInfo {blocks :: M.Map Label Block, labels :: [Label] , init :: [Label], finals :: [Label], flow :: Flow, vars :: [Var]}

data Block = 
    B_IAssign {name :: String, valI :: IExpr} |
    B_BAssign {name :: String, valB :: BExpr} |
    B_Cond {cond :: BExpr} |
    B_Skip

type Label = Int
type Flow = [(Int, Int)]
type Var = String
{-# LINE 43 "Administration.hs" #-}

{-# LINE 135 "Administration.ag" #-}


--foldProcs :: Procs -> (Int, M.Map String Proc')
--foldProcs procs = foldr (\x y -> 
                --let (l,proc') = wrapproc x (fst y)
                --in (l + 1,M.insert (getName proc') proc' $ snd y)) (1,M.empty) procs

getName :: Proc' -> String
getName (Proc' _ _ name _ _ _) = name

--wrapproc :: Proc -> Int -> (Int, Proc')
--wrapproc proc label = main_Syn_Proc $ wrap_Proc (sem_Proc proc) (Inh_Proc label)
{-# LINE 58 "Administration.hs" #-}
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
           deriving ( Eq)
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
             ({-# LINE 71 "Administration.ag" #-}
              S.empty
              {-# LINE 115 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 60 "Administration.ag" #-}
              BConst val_
              {-# LINE 120 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 60 "Administration.ag" #-}
              _slf
              {-# LINE 125 "Administration.hs" #-}
              )
     in  ( _lhsOslf,_lhsOsvars))
sem_BExpr_BVar :: String ->
                  T_BExpr
sem_BExpr_BVar name_ =
    (let _lhsOsvars :: (S.Set Var)
         _lhsOslf :: BExpr
         _lhsOsvars =
             ({-# LINE 72 "Administration.ag" #-}
              S.singleton name_
              {-# LINE 136 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 60 "Administration.ag" #-}
              BVar name_
              {-# LINE 141 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 60 "Administration.ag" #-}
              _slf
              {-# LINE 146 "Administration.hs" #-}
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
             ({-# LINE 73 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 162 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 60 "Administration.ag" #-}
              LessThan _leftIslf _rightIslf
              {-# LINE 167 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 60 "Administration.ag" #-}
              _slf
              {-# LINE 172 "Administration.hs" #-}
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
             ({-# LINE 74 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 192 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 60 "Administration.ag" #-}
              GreaterThan _leftIslf _rightIslf
              {-# LINE 197 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 60 "Administration.ag" #-}
              _slf
              {-# LINE 202 "Administration.hs" #-}
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
             ({-# LINE 75 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 222 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 60 "Administration.ag" #-}
              LessEqual _leftIslf _rightIslf
              {-# LINE 227 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 60 "Administration.ag" #-}
              _slf
              {-# LINE 232 "Administration.hs" #-}
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
             ({-# LINE 76 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 252 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 60 "Administration.ag" #-}
              GreaterEqual _leftIslf _rightIslf
              {-# LINE 257 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 60 "Administration.ag" #-}
              _slf
              {-# LINE 262 "Administration.hs" #-}
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
             ({-# LINE 77 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 282 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 60 "Administration.ag" #-}
              IEqual _leftIslf _rightIslf
              {-# LINE 287 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 60 "Administration.ag" #-}
              _slf
              {-# LINE 292 "Administration.hs" #-}
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
             ({-# LINE 78 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 312 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 60 "Administration.ag" #-}
              BEqual _leftIslf _rightIslf
              {-# LINE 317 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 60 "Administration.ag" #-}
              _slf
              {-# LINE 322 "Administration.hs" #-}
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
             ({-# LINE 79 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 342 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 60 "Administration.ag" #-}
              And _leftIslf _rightIslf
              {-# LINE 347 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 60 "Administration.ag" #-}
              _slf
              {-# LINE 352 "Administration.hs" #-}
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
             ({-# LINE 60 "Administration.ag" #-}
              Or _leftIslf _rightIslf
              {-# LINE 372 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 60 "Administration.ag" #-}
              _slf
              {-# LINE 377 "Administration.hs" #-}
              )
         _lhsOsvars =
             ({-# LINE 59 "Administration.ag" #-}
              _rightIsvars
              {-# LINE 382 "Administration.hs" #-}
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
             ({-# LINE 80 "Administration.ag" #-}
              _valIsvars
              {-# LINE 399 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 60 "Administration.ag" #-}
              Not _valIslf
              {-# LINE 404 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 60 "Administration.ag" #-}
              _slf
              {-# LINE 409 "Administration.hs" #-}
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
          deriving ( Eq)
-- cata
sem_Expr :: Expr ->
            T_Expr
sem_Expr (B _bExpr) =
    (sem_Expr_B (sem_BExpr _bExpr))
sem_Expr (I _iExpr) =
    (sem_Expr_I (sem_IExpr _iExpr))
-- semantic domain
type T_Expr = ( )
data Inh_Expr = Inh_Expr {}
data Syn_Expr = Syn_Expr {}
wrap_Expr :: T_Expr ->
             Inh_Expr ->
             Syn_Expr
wrap_Expr sem (Inh_Expr) =
    (let ( ) = sem
     in  (Syn_Expr))
sem_Expr_B :: T_BExpr ->
              T_Expr
sem_Expr_B bExpr_ =
    (let _bExprIslf :: BExpr
         _bExprIsvars :: (S.Set Var)
         ( _bExprIslf,_bExprIsvars) =
             bExpr_
     in  ( ))
sem_Expr_I :: T_IExpr ->
              T_Expr
sem_Expr_I iExpr_ =
    (let _iExprIslf :: IExpr
         _iExprIsvars :: (S.Set Var)
         ( _iExprIslf,_iExprIsvars) =
             iExpr_
     in  ( ))
-- IExpr -------------------------------------------------------
data IExpr = IConst (Int)
           | Var (String)
           | Plus (IExpr) (IExpr)
           | Minus (IExpr) (IExpr)
           | Times (IExpr) (IExpr)
           | Divide (IExpr) (IExpr)
           | Deref (IExpr)
           deriving ( Eq)
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
             ({-# LINE 63 "Administration.ag" #-}
              S.empty
              {-# LINE 555 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 60 "Administration.ag" #-}
              IConst val_
              {-# LINE 560 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 60 "Administration.ag" #-}
              _slf
              {-# LINE 565 "Administration.hs" #-}
              )
     in  ( _lhsOslf,_lhsOsvars))
sem_IExpr_Var :: String ->
                 T_IExpr
sem_IExpr_Var name_ =
    (let _lhsOsvars :: (S.Set Var)
         _lhsOslf :: IExpr
         _lhsOsvars =
             ({-# LINE 64 "Administration.ag" #-}
              S.singleton name_
              {-# LINE 576 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 60 "Administration.ag" #-}
              Var name_
              {-# LINE 581 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 60 "Administration.ag" #-}
              _slf
              {-# LINE 586 "Administration.hs" #-}
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
             ({-# LINE 65 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 602 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 60 "Administration.ag" #-}
              Plus _leftIslf _rightIslf
              {-# LINE 607 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 60 "Administration.ag" #-}
              _slf
              {-# LINE 612 "Administration.hs" #-}
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
             ({-# LINE 66 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 632 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 60 "Administration.ag" #-}
              Minus _leftIslf _rightIslf
              {-# LINE 637 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 60 "Administration.ag" #-}
              _slf
              {-# LINE 642 "Administration.hs" #-}
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
             ({-# LINE 67 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 662 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 60 "Administration.ag" #-}
              Times _leftIslf _rightIslf
              {-# LINE 667 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 60 "Administration.ag" #-}
              _slf
              {-# LINE 672 "Administration.hs" #-}
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
             ({-# LINE 68 "Administration.ag" #-}
              S.union _leftIsvars _rightIsvars
              {-# LINE 692 "Administration.hs" #-}
              )
         _slf =
             ({-# LINE 60 "Administration.ag" #-}
              Divide _leftIslf _rightIslf
              {-# LINE 697 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 60 "Administration.ag" #-}
              _slf
              {-# LINE 702 "Administration.hs" #-}
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
             ({-# LINE 60 "Administration.ag" #-}
              Deref _ptrIslf
              {-# LINE 719 "Administration.hs" #-}
              )
         _lhsOslf =
             ({-# LINE 60 "Administration.ag" #-}
              _slf
              {-# LINE 724 "Administration.hs" #-}
              )
         _lhsOsvars =
             ({-# LINE 59 "Administration.ag" #-}
              _ptrIsvars
              {-# LINE 729 "Administration.hs" #-}
              )
         ( _ptrIslf,_ptrIsvars) =
             ptr_
     in  ( _lhsOslf,_lhsOsvars))
-- Proc --------------------------------------------------------
data Proc = Proc (String) (([String])) (String) (Stat)
-- cata
sem_Proc :: Proc ->
            T_Proc
sem_Proc (Proc _name _inp _out _stat) =
    (sem_Proc_Proc _name _inp _out (sem_Stat _stat))
-- semantic domain
type T_Proc = Label ->
              ( String,(M.Map String Proc))
data Inh_Proc = Inh_Proc {label_Inh_Proc :: Label}
data Syn_Proc = Syn_Proc {name_Syn_Proc :: String,pmap_Syn_Proc :: (M.Map String Proc)}
wrap_Proc :: T_Proc ->
             Inh_Proc ->
             Syn_Proc
wrap_Proc sem (Inh_Proc _lhsIlabel) =
    (let ( _lhsOname,_lhsOpmap) = sem _lhsIlabel
     in  (Syn_Proc _lhsOname _lhsOpmap))
sem_Proc_Proc :: String ->
                 ([String]) ->
                 String ->
                 T_Stat ->
                 T_Proc
sem_Proc_Proc name_ inp_ out_ stat_ =
    (\ _lhsIlabel ->
         (let _lhsOpmap :: (M.Map String Proc)
              _lhsOname :: String
              _statOlabel :: Label
              _statOprocs :: (M.Map String Proc')
              _statIblocks :: (M.Map Label Block)
              _statIflabels :: ([Label])
              _statIflow :: Flow
              _statIflowLabels :: ([Label])
              _statIinitl :: Label
              _statImaxLabel :: Label
              _statIslf :: Stat
              _statIsvars :: (S.Set Var)
              _lhsOpmap =
                  ({-# LINE 53 "Administration.ag" #-}
                   M.singleton name_ (Proc name_ inp_ out_ _statIslf)
                   {-# LINE 774 "Administration.hs" #-}
                   )
              _lhsOname =
                  ({-# LINE 54 "Administration.ag" #-}
                   name_
                   {-# LINE 779 "Administration.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 55 "Administration.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 784 "Administration.hs" #-}
                   )
              _statOprocs =
                  ({-# LINE 56 "Administration.ag" #-}
                   M.empty
                   {-# LINE 789 "Administration.hs" #-}
                   )
              ( _statIblocks,_statIflabels,_statIflow,_statIflowLabels,_statIinitl,_statImaxLabel,_statIslf,_statIsvars) =
                  stat_ _statOlabel _statOprocs
          in  ( _lhsOname,_lhsOpmap)))
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
-- Program -----------------------------------------------------
data Program = Program (Procs) (Stat)
-- cata
sem_Program :: Program ->
               T_Program
sem_Program (Program _procs _stat) =
    (sem_Program_Program _procs (sem_Stat _stat))
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
sem_Program_Program :: Procs ->
                       T_Stat ->
                       T_Program
sem_Program_Program procs_ stat_ =
    (let _lhsOpinfo :: ProgramInfo
         _statOlabel :: Label
         _statOprocs :: (M.Map String Proc')
         _statIblocks :: (M.Map Label Block)
         _statIflabels :: ([Label])
         _statIflow :: Flow
         _statIflowLabels :: ([Label])
         _statIinitl :: Label
         _statImaxLabel :: Label
         _statIslf :: Stat
         _statIsvars :: (S.Set Var)
         _lhsOpinfo =
             ({-# LINE 32 "Administration.ag" #-}
              ProgramInfo _statIblocks [1 .. _statImaxLabel] [_statIinitl] (_statIflabels) _statIflow (S.toList _statIsvars)
              {-# LINE 856 "Administration.hs" #-}
              )
         _statOlabel =
             ({-# LINE 33 "Administration.ag" #-}
              1
              {-# LINE 861 "Administration.hs" #-}
              )
         _procs =
             ({-# LINE 34 "Administration.ag" #-}
              []
              {-# LINE 866 "Administration.hs" #-}
              )
         _statOprocs =
             ({-# LINE 35 "Administration.ag" #-}
              M.empty
              {-# LINE 871 "Administration.hs" #-}
              )
         ( _statIblocks,_statIflabels,_statIflow,_statIflowLabels,_statIinitl,_statImaxLabel,_statIslf,_statIsvars) =
             stat_ _statOlabel _statOprocs
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
          | IAssign (String) (IExpr)
          | BAssign (String) (BExpr)
          | Seq (Stat) (Stat)
-- cata
sem_Stat :: Stat ->
            T_Stat
sem_Stat (Skip) =
    (sem_Stat_Skip)
sem_Stat (IfThenElse _cond _stat1 _stat2) =
    (sem_Stat_IfThenElse (sem_BExpr _cond) (sem_Stat _stat1) (sem_Stat _stat2))
sem_Stat (While _cond _stat) =
    (sem_Stat_While (sem_BExpr _cond) (sem_Stat _stat))
sem_Stat (IAssign _name _val) =
    (sem_Stat_IAssign _name (sem_IExpr _val))
sem_Stat (BAssign _name _val) =
    (sem_Stat_BAssign _name (sem_BExpr _val))
sem_Stat (Seq _stat1 _stat2) =
    (sem_Stat_Seq (sem_Stat _stat1) (sem_Stat _stat2))
-- semantic domain
type T_Stat = Label ->
              (M.Map String Proc') ->
              ( (M.Map Label Block),([Label]),Flow,([Label]),Label,Label,Stat,(S.Set Var))
data Inh_Stat = Inh_Stat {label_Inh_Stat :: Label,procs_Inh_Stat :: (M.Map String Proc')}
data Syn_Stat = Syn_Stat {blocks_Syn_Stat :: (M.Map Label Block),flabels_Syn_Stat :: ([Label]),flow_Syn_Stat :: Flow,flowLabels_Syn_Stat :: ([Label]),initl_Syn_Stat :: Label,maxLabel_Syn_Stat :: Label,slf_Syn_Stat :: Stat,svars_Syn_Stat :: (S.Set Var)}
wrap_Stat :: T_Stat ->
             Inh_Stat ->
             Syn_Stat
wrap_Stat sem (Inh_Stat _lhsIlabel _lhsIprocs) =
    (let ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOmaxLabel,_lhsOslf,_lhsOsvars) = sem _lhsIlabel _lhsIprocs
     in  (Syn_Stat _lhsOblocks _lhsOflabels _lhsOflow _lhsOflowLabels _lhsOinitl _lhsOmaxLabel _lhsOslf _lhsOsvars))
sem_Stat_Skip :: T_Stat
sem_Stat_Skip =
    (\ _lhsIlabel
       _lhsIprocs ->
         (let _lhsOmaxLabel :: Label
              _lhsOflowLabels :: ([Label])
              _lhsOblocks :: (M.Map Label Block)
              _lhsOsvars :: (S.Set Var)
              _lhsOflow :: Flow
              _lhsOinitl :: Label
              _lhsOflabels :: ([Label])
              _lhsOslf :: Stat
              _lhsOmaxLabel =
                  ({-# LINE 83 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 948 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 84 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 953 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 85 "Administration.ag" #-}
                   M.singleton _lhsIlabel B_Skip
                   {-# LINE 958 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 86 "Administration.ag" #-}
                   S.empty
                   {-# LINE 963 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 87 "Administration.ag" #-}
                   []
                   {-# LINE 968 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 88 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 973 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 89 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 978 "Administration.hs" #-}
                   )
              _slf =
                  ({-# LINE 60 "Administration.ag" #-}
                   Skip
                   {-# LINE 983 "Administration.hs" #-}
                   )
              _lhsOslf =
                  ({-# LINE 60 "Administration.ag" #-}
                   _slf
                   {-# LINE 988 "Administration.hs" #-}
                   )
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOmaxLabel,_lhsOslf,_lhsOsvars)))
sem_Stat_IfThenElse :: T_BExpr ->
                       T_Stat ->
                       T_Stat ->
                       T_Stat
sem_Stat_IfThenElse cond_ stat1_ stat2_ =
    (\ _lhsIlabel
       _lhsIprocs ->
         (let _lhsOmaxLabel :: Label
              _lhsOflowLabels :: ([Label])
              _lhsOflow :: Flow
              _stat1Olabel :: Label
              _stat2Olabel :: Label
              _lhsOblocks :: (M.Map Label Block)
              _lhsOsvars :: (S.Set Var)
              _lhsOinitl :: Label
              _lhsOflabels :: ([Label])
              _lhsOslf :: Stat
              _stat1Oprocs :: (M.Map String Proc')
              _stat2Oprocs :: (M.Map String Proc')
              _condIslf :: BExpr
              _condIsvars :: (S.Set Var)
              _stat1Iblocks :: (M.Map Label Block)
              _stat1Iflabels :: ([Label])
              _stat1Iflow :: Flow
              _stat1IflowLabels :: ([Label])
              _stat1Iinitl :: Label
              _stat1ImaxLabel :: Label
              _stat1Islf :: Stat
              _stat1Isvars :: (S.Set Var)
              _stat2Iblocks :: (M.Map Label Block)
              _stat2Iflabels :: ([Label])
              _stat2Iflow :: Flow
              _stat2IflowLabels :: ([Label])
              _stat2Iinitl :: Label
              _stat2ImaxLabel :: Label
              _stat2Islf :: Stat
              _stat2Isvars :: (S.Set Var)
              _lhsOmaxLabel =
                  ({-# LINE 90 "Administration.ag" #-}
                   _stat2ImaxLabel
                   {-# LINE 1031 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 91 "Administration.ag" #-}
                   [_label1    ,_label2    ]
                   {-# LINE 1036 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 92 "Administration.ag" #-}
                   (_lhsIlabel, _label1    ) : (_lhsIlabel, _label2    ) : _stat1Iflow ++ _stat2Iflow
                   {-# LINE 1041 "Administration.hs" #-}
                   )
              _stat1Olabel =
                  ({-# LINE 93 "Administration.ag" #-}
                   _label1
                   {-# LINE 1046 "Administration.hs" #-}
                   )
              _label1 =
                  ({-# LINE 94 "Administration.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1051 "Administration.hs" #-}
                   )
              _label2 =
                  ({-# LINE 95 "Administration.ag" #-}
                   _stat1ImaxLabel + 1
                   {-# LINE 1056 "Administration.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 96 "Administration.ag" #-}
                   _label2
                   {-# LINE 1061 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 97 "Administration.ag" #-}
                   M.union (M.union (M.singleton _lhsIlabel (B_Cond _condIslf)) _stat1Iblocks) _stat2Iblocks
                   {-# LINE 1066 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 98 "Administration.ag" #-}
                   S.union _condIsvars (S.union _stat1Isvars _stat2Isvars)
                   {-# LINE 1071 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 99 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1076 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 100 "Administration.ag" #-}
                   [_stat1ImaxLabel,_stat2ImaxLabel]
                   {-# LINE 1081 "Administration.hs" #-}
                   )
              _slf =
                  ({-# LINE 60 "Administration.ag" #-}
                   IfThenElse _condIslf _stat1Islf _stat2Islf
                   {-# LINE 1086 "Administration.hs" #-}
                   )
              _lhsOslf =
                  ({-# LINE 60 "Administration.ag" #-}
                   _slf
                   {-# LINE 1091 "Administration.hs" #-}
                   )
              _stat1Oprocs =
                  ({-# LINE 43 "Administration.ag" #-}
                   _lhsIprocs
                   {-# LINE 1096 "Administration.hs" #-}
                   )
              _stat2Oprocs =
                  ({-# LINE 43 "Administration.ag" #-}
                   _lhsIprocs
                   {-# LINE 1101 "Administration.hs" #-}
                   )
              ( _condIslf,_condIsvars) =
                  cond_
              ( _stat1Iblocks,_stat1Iflabels,_stat1Iflow,_stat1IflowLabels,_stat1Iinitl,_stat1ImaxLabel,_stat1Islf,_stat1Isvars) =
                  stat1_ _stat1Olabel _stat1Oprocs
              ( _stat2Iblocks,_stat2Iflabels,_stat2Iflow,_stat2IflowLabels,_stat2Iinitl,_stat2ImaxLabel,_stat2Islf,_stat2Isvars) =
                  stat2_ _stat2Olabel _stat2Oprocs
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOmaxLabel,_lhsOslf,_lhsOsvars)))
sem_Stat_While :: T_BExpr ->
                  T_Stat ->
                  T_Stat
sem_Stat_While cond_ stat_ =
    (\ _lhsIlabel
       _lhsIprocs ->
         (let _lhsOmaxLabel :: Label
              _lhsOflowLabels :: ([Label])
              _lhsOflow :: Flow
              _statOlabel :: Label
              _lhsOblocks :: (M.Map Label Block)
              _lhsOsvars :: (S.Set Var)
              _lhsOinitl :: Label
              _lhsOflabels :: ([Label])
              _lhsOslf :: Stat
              _statOprocs :: (M.Map String Proc')
              _condIslf :: BExpr
              _condIsvars :: (S.Set Var)
              _statIblocks :: (M.Map Label Block)
              _statIflabels :: ([Label])
              _statIflow :: Flow
              _statIflowLabels :: ([Label])
              _statIinitl :: Label
              _statImaxLabel :: Label
              _statIslf :: Stat
              _statIsvars :: (S.Set Var)
              _lhsOmaxLabel =
                  ({-# LINE 101 "Administration.ag" #-}
                   _statImaxLabel
                   {-# LINE 1139 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 102 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 1144 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 103 "Administration.ag" #-}
                   (_lhsIlabel, _label1    ) : _statIflow ++ map (\x -> (x,_lhsIlabel)) _statIflabels
                   {-# LINE 1149 "Administration.hs" #-}
                   )
              _label1 =
                  ({-# LINE 104 "Administration.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1154 "Administration.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 105 "Administration.ag" #-}
                   _label1
                   {-# LINE 1159 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 106 "Administration.ag" #-}
                   M.union (M.singleton _lhsIlabel (B_Cond _condIslf)) _statIblocks
                   {-# LINE 1164 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 107 "Administration.ag" #-}
                   S.union _condIsvars _statIsvars
                   {-# LINE 1169 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 108 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1174 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 109 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 1179 "Administration.hs" #-}
                   )
              _slf =
                  ({-# LINE 60 "Administration.ag" #-}
                   While _condIslf _statIslf
                   {-# LINE 1184 "Administration.hs" #-}
                   )
              _lhsOslf =
                  ({-# LINE 60 "Administration.ag" #-}
                   _slf
                   {-# LINE 1189 "Administration.hs" #-}
                   )
              _statOprocs =
                  ({-# LINE 43 "Administration.ag" #-}
                   _lhsIprocs
                   {-# LINE 1194 "Administration.hs" #-}
                   )
              ( _condIslf,_condIsvars) =
                  cond_
              ( _statIblocks,_statIflabels,_statIflow,_statIflowLabels,_statIinitl,_statImaxLabel,_statIslf,_statIsvars) =
                  stat_ _statOlabel _statOprocs
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOmaxLabel,_lhsOslf,_lhsOsvars)))
sem_Stat_IAssign :: String ->
                    T_IExpr ->
                    T_Stat
sem_Stat_IAssign name_ val_ =
    (\ _lhsIlabel
       _lhsIprocs ->
         (let _lhsOmaxLabel :: Label
              _lhsOflowLabels :: ([Label])
              _lhsOblocks :: (M.Map Label Block)
              _lhsOsvars :: (S.Set Var)
              _lhsOflow :: Flow
              _lhsOinitl :: Label
              _lhsOflabels :: ([Label])
              _lhsOslf :: Stat
              _valIslf :: IExpr
              _valIsvars :: (S.Set Var)
              _lhsOmaxLabel =
                  ({-# LINE 110 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1220 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 111 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 1225 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 112 "Administration.ag" #-}
                   M.singleton _lhsIlabel $ B_IAssign name_ _valIslf
                   {-# LINE 1230 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 113 "Administration.ag" #-}
                   S.union (S.singleton name_) _valIsvars
                   {-# LINE 1235 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 114 "Administration.ag" #-}
                   []
                   {-# LINE 1240 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 115 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1245 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 116 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 1250 "Administration.hs" #-}
                   )
              _slf =
                  ({-# LINE 60 "Administration.ag" #-}
                   IAssign name_ _valIslf
                   {-# LINE 1255 "Administration.hs" #-}
                   )
              _lhsOslf =
                  ({-# LINE 60 "Administration.ag" #-}
                   _slf
                   {-# LINE 1260 "Administration.hs" #-}
                   )
              ( _valIslf,_valIsvars) =
                  val_
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOmaxLabel,_lhsOslf,_lhsOsvars)))
sem_Stat_BAssign :: String ->
                    T_BExpr ->
                    T_Stat
sem_Stat_BAssign name_ val_ =
    (\ _lhsIlabel
       _lhsIprocs ->
         (let _lhsOmaxLabel :: Label
              _lhsOflowLabels :: ([Label])
              _lhsOblocks :: (M.Map Label Block)
              _lhsOsvars :: (S.Set Var)
              _lhsOflow :: Flow
              _lhsOinitl :: Label
              _lhsOflabels :: ([Label])
              _lhsOslf :: Stat
              _valIslf :: BExpr
              _valIsvars :: (S.Set Var)
              _lhsOmaxLabel =
                  ({-# LINE 117 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1284 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 118 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 1289 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 119 "Administration.ag" #-}
                   M.singleton _lhsIlabel $ B_BAssign name_ _valIslf
                   {-# LINE 1294 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 120 "Administration.ag" #-}
                   S.union (S.singleton name_) _valIsvars
                   {-# LINE 1299 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 121 "Administration.ag" #-}
                   []
                   {-# LINE 1304 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 122 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1309 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 123 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 1314 "Administration.hs" #-}
                   )
              _slf =
                  ({-# LINE 60 "Administration.ag" #-}
                   BAssign name_ _valIslf
                   {-# LINE 1319 "Administration.hs" #-}
                   )
              _lhsOslf =
                  ({-# LINE 60 "Administration.ag" #-}
                   _slf
                   {-# LINE 1324 "Administration.hs" #-}
                   )
              ( _valIslf,_valIsvars) =
                  val_
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOmaxLabel,_lhsOslf,_lhsOsvars)))
sem_Stat_Seq :: T_Stat ->
                T_Stat ->
                T_Stat
sem_Stat_Seq stat1_ stat2_ =
    (\ _lhsIlabel
       _lhsIprocs ->
         (let _lhsOmaxLabel :: Label
              _lhsOflowLabels :: ([Label])
              _lhsOflow :: Flow
              _lhsOblocks :: (M.Map Label Block)
              _stat1Olabel :: Label
              _stat2Olabel :: Label
              _lhsOsvars :: (S.Set Var)
              _lhsOinitl :: Label
              _lhsOflabels :: ([Label])
              _lhsOslf :: Stat
              _stat1Oprocs :: (M.Map String Proc')
              _stat2Oprocs :: (M.Map String Proc')
              _stat1Iblocks :: (M.Map Label Block)
              _stat1Iflabels :: ([Label])
              _stat1Iflow :: Flow
              _stat1IflowLabels :: ([Label])
              _stat1Iinitl :: Label
              _stat1ImaxLabel :: Label
              _stat1Islf :: Stat
              _stat1Isvars :: (S.Set Var)
              _stat2Iblocks :: (M.Map Label Block)
              _stat2Iflabels :: ([Label])
              _stat2Iflow :: Flow
              _stat2IflowLabels :: ([Label])
              _stat2Iinitl :: Label
              _stat2ImaxLabel :: Label
              _stat2Islf :: Stat
              _stat2Isvars :: (S.Set Var)
              _lhsOmaxLabel =
                  ({-# LINE 124 "Administration.ag" #-}
                   _stat2ImaxLabel
                   {-# LINE 1366 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 125 "Administration.ag" #-}
                   _stat2IflowLabels
                   {-# LINE 1371 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 126 "Administration.ag" #-}
                   _stat1Iflow ++ map (\x -> (x,_label2    )) _stat1IflowLabels ++ _stat2Iflow
                   {-# LINE 1376 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 127 "Administration.ag" #-}
                   M.union _stat1Iblocks _stat2Iblocks
                   {-# LINE 1381 "Administration.hs" #-}
                   )
              _stat1Olabel =
                  ({-# LINE 128 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1386 "Administration.hs" #-}
                   )
              _label2 =
                  ({-# LINE 129 "Administration.ag" #-}
                   _stat1ImaxLabel + 1
                   {-# LINE 1391 "Administration.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 130 "Administration.ag" #-}
                   _label2
                   {-# LINE 1396 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 131 "Administration.ag" #-}
                   S.union _stat1Isvars _stat2Isvars
                   {-# LINE 1401 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 132 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1406 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 133 "Administration.ag" #-}
                   _stat2Iflabels
                   {-# LINE 1411 "Administration.hs" #-}
                   )
              _slf =
                  ({-# LINE 60 "Administration.ag" #-}
                   Seq _stat1Islf _stat2Islf
                   {-# LINE 1416 "Administration.hs" #-}
                   )
              _lhsOslf =
                  ({-# LINE 60 "Administration.ag" #-}
                   _slf
                   {-# LINE 1421 "Administration.hs" #-}
                   )
              _stat1Oprocs =
                  ({-# LINE 43 "Administration.ag" #-}
                   _lhsIprocs
                   {-# LINE 1426 "Administration.hs" #-}
                   )
              _stat2Oprocs =
                  ({-# LINE 43 "Administration.ag" #-}
                   _lhsIprocs
                   {-# LINE 1431 "Administration.hs" #-}
                   )
              ( _stat1Iblocks,_stat1Iflabels,_stat1Iflow,_stat1IflowLabels,_stat1Iinitl,_stat1ImaxLabel,_stat1Islf,_stat1Isvars) =
                  stat1_ _stat1Olabel _stat1Oprocs
              ( _stat2Iblocks,_stat2Iflabels,_stat2Iflow,_stat2IflowLabels,_stat2Iinitl,_stat2ImaxLabel,_stat2Islf,_stat2Isvars) =
                  stat2_ _stat2Olabel _stat2Oprocs
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOmaxLabel,_lhsOslf,_lhsOsvars)))
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
    (sem_Stat'_Call' _labelCall _labelExit _name _params _out)
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
                   Exprs ->
                   String ->
                   (T_Stat')
sem_Stat'_Call' labelCall_ labelExit_ name_ params_ out_ =
    (let
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