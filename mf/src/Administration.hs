

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

{-# LINE 89 "AttributeGrammar.ag" #-}

type Procs = [Proc]
type Procs' = [Proc']
type Exprs = [Expr]
{-# LINE 23 "Administration.hs" #-}

{-# LINE 9 "Administration.ag" #-}



toProgramInfo :: Program -> ProgramInfo
toProgramInfo program = pinfo_Syn_Program $ wrap_Program (sem_Program program) (Inh_Program)

data ProgramInfo = ProgramInfo {blocks :: M.Map Label Block, init :: [Label], finals :: [Label], flow :: Flow, vars :: [Var]}
    deriving Show

data Block = 
    B_IAssign {name :: String, valI :: IExpr} |
    B_BAssign {name :: String, valB :: BExpr} |
    B_Cond {cond :: BExpr} |
    B_Skip deriving Show

type Label = Int
type Flow = [(Int, Int)]
type Var = String
{-# LINE 44 "Administration.hs" #-}

{-# LINE 123 "Administration.ag" #-}


foldProcs :: Procs -> (Int, M.Map String Proc')
foldProcs procs = foldr (\x y -> 
                let (l,proc') = wrapproc x (fst y)
                in (l + 1,M.insert (getName proc') proc' $ snd y)) (1,M.empty) procs

getName :: Proc' -> String
getName (Proc' _ _ name _ _ _) = name

wrapproc :: Proc -> Int -> (Int, Proc')
wrapproc proc label = main_Syn_Proc $ wrap_Proc (sem_Proc proc) (Inh_Proc label)
{-# LINE 59 "Administration.hs" #-}
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
type T_BExpr = ( )
data Inh_BExpr = Inh_BExpr {}
data Syn_BExpr = Syn_BExpr {}
wrap_BExpr :: T_BExpr ->
              Inh_BExpr ->
              Syn_BExpr
wrap_BExpr sem (Inh_BExpr) =
    (let ( ) = sem
     in  (Syn_BExpr))
sem_BExpr_BConst :: Bool ->
                    T_BExpr
sem_BExpr_BConst val_ =
    (let
     in  ( ))
sem_BExpr_BVar :: String ->
                  T_BExpr
sem_BExpr_BVar name_ =
    (let
     in  ( ))
sem_BExpr_LessThan :: T_IExpr ->
                      T_IExpr ->
                      T_BExpr
sem_BExpr_LessThan left_ right_ =
    (let
     in  ( ))
sem_BExpr_GreaterThan :: T_IExpr ->
                         T_IExpr ->
                         T_BExpr
sem_BExpr_GreaterThan left_ right_ =
    (let
     in  ( ))
sem_BExpr_LessEqual :: T_IExpr ->
                       T_IExpr ->
                       T_BExpr
sem_BExpr_LessEqual left_ right_ =
    (let
     in  ( ))
sem_BExpr_GreaterEqual :: T_IExpr ->
                          T_IExpr ->
                          T_BExpr
sem_BExpr_GreaterEqual left_ right_ =
    (let
     in  ( ))
sem_BExpr_IEqual :: T_IExpr ->
                    T_IExpr ->
                    T_BExpr
sem_BExpr_IEqual left_ right_ =
    (let
     in  ( ))
sem_BExpr_BEqual :: T_BExpr ->
                    T_BExpr ->
                    T_BExpr
sem_BExpr_BEqual left_ right_ =
    (let
     in  ( ))
sem_BExpr_And :: T_BExpr ->
                 T_BExpr ->
                 T_BExpr
sem_BExpr_And left_ right_ =
    (let
     in  ( ))
sem_BExpr_Or :: T_BExpr ->
                T_BExpr ->
                T_BExpr
sem_BExpr_Or left_ right_ =
    (let
     in  ( ))
sem_BExpr_Not :: T_BExpr ->
                 T_BExpr
sem_BExpr_Not val_ =
    (let
     in  ( ))
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
    (let
     in  ( ))
sem_Code_CIExpr :: T_IExpr ->
                   T_Code
sem_Code_CIExpr iExpr_ =
    (let
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
    (let
     in  ( ))
sem_Expr_I :: T_IExpr ->
              T_Expr
sem_Expr_I iExpr_ =
    (let
     in  ( ))
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
type T_IExpr = ( )
data Inh_IExpr = Inh_IExpr {}
data Syn_IExpr = Syn_IExpr {}
wrap_IExpr :: T_IExpr ->
              Inh_IExpr ->
              Syn_IExpr
wrap_IExpr sem (Inh_IExpr) =
    (let ( ) = sem
     in  (Syn_IExpr))
sem_IExpr_IConst :: Int ->
                    T_IExpr
sem_IExpr_IConst val_ =
    (let
     in  ( ))
sem_IExpr_Var :: String ->
                 T_IExpr
sem_IExpr_Var name_ =
    (let
     in  ( ))
sem_IExpr_Plus :: T_IExpr ->
                  T_IExpr ->
                  T_IExpr
sem_IExpr_Plus left_ right_ =
    (let
     in  ( ))
sem_IExpr_Minus :: T_IExpr ->
                   T_IExpr ->
                   T_IExpr
sem_IExpr_Minus left_ right_ =
    (let
     in  ( ))
sem_IExpr_Times :: T_IExpr ->
                   T_IExpr ->
                   T_IExpr
sem_IExpr_Times left_ right_ =
    (let
     in  ( ))
sem_IExpr_Divide :: T_IExpr ->
                    T_IExpr ->
                    T_IExpr
sem_IExpr_Divide left_ right_ =
    (let
     in  ( ))
sem_IExpr_Deref :: T_IExpr ->
                   T_IExpr
sem_IExpr_Deref ptr_ =
    (let
     in  ( ))
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
              ( ((Int,Proc')),String,(M.Map String Proc))
data Inh_Proc = Inh_Proc {label_Inh_Proc :: Label}
data Syn_Proc = Syn_Proc {main_Syn_Proc :: ((Int,Proc')),name_Syn_Proc :: String,pmap_Syn_Proc :: (M.Map String Proc)}
wrap_Proc :: T_Proc ->
             Inh_Proc ->
             Syn_Proc
wrap_Proc sem (Inh_Proc _lhsIlabel) =
    (let ( _lhsOmain,_lhsOname,_lhsOpmap) = sem _lhsIlabel
     in  (Syn_Proc _lhsOmain _lhsOname _lhsOpmap))
sem_Proc_Proc :: String ->
                 ([String]) ->
                 String ->
                 T_Stat ->
                 T_Proc
sem_Proc_Proc name_ inp_ out_ stat_ =
    (\ _lhsIlabel ->
         (let _lhsOmain :: ((Int,Proc'))
              _lhsOpmap :: (M.Map String Proc)
              _lhsOname :: String
              _statOlabel :: Label
              _statOprocs :: (M.Map String Proc')
              _statIblocks :: (M.Map Label Block)
              _statIflabels :: ([Label])
              _statIflow :: Flow
              _statIflowLabels :: ([Label])
              _statIinitl :: Label
              _statImain :: Stat'
              _statImaxLabel :: Label
              _statIst :: Stat
              _statIsvars :: (S.Set Var)
              _lhsOmain =
                  ({-# LINE 58 "Administration.ag" #-}
                   (_statImaxLabel + 1, Proc' _lhsIlabel (_statImaxLabel + 1) name_ inp_ out_ _statImain)
                   {-# LINE 374 "Administration.hs" #-}
                   )
              _lhsOpmap =
                  ({-# LINE 59 "Administration.ag" #-}
                   M.singleton name_ (Proc name_ inp_ out_ _statIst)
                   {-# LINE 379 "Administration.hs" #-}
                   )
              _lhsOname =
                  ({-# LINE 60 "Administration.ag" #-}
                   name_
                   {-# LINE 384 "Administration.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 61 "Administration.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 389 "Administration.hs" #-}
                   )
              _statOprocs =
                  ({-# LINE 62 "Administration.ag" #-}
                   M.empty
                   {-# LINE 394 "Administration.hs" #-}
                   )
              ( _statIblocks,_statIflabels,_statIflow,_statIflowLabels,_statIinitl,_statImain,_statImaxLabel,_statIst,_statIsvars) =
                  stat_ _statOlabel _statOprocs
          in  ( _lhsOmain,_lhsOname,_lhsOpmap)))
-- Proc' -------------------------------------------------------
data Proc' = Proc' (Int) (Int) (String) (([String])) (String) (Stat')
           deriving ( Show)
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
             deriving ( Show)
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
         _statImain :: Stat'
         _statImaxLabel :: Label
         _statIst :: Stat
         _statIsvars :: (S.Set Var)
         _lhsOpinfo =
             ({-# LINE 33 "Administration.ag" #-}
              ProgramInfo _statIblocks [_statIinitl] (_statIflabels) _statIflow (S.toList _statIsvars)
              {-# LINE 464 "Administration.hs" #-}
              )
         _statOlabel =
             ({-# LINE 34 "Administration.ag" #-}
              fst _procs
              {-# LINE 469 "Administration.hs" #-}
              )
         _procs =
             ({-# LINE 35 "Administration.ag" #-}
              foldProcs procs_
              {-# LINE 474 "Administration.hs" #-}
              )
         _statOprocs =
             ({-# LINE 36 "Administration.ag" #-}
              snd _procs
              {-# LINE 479 "Administration.hs" #-}
              )
         ( _statIblocks,_statIflabels,_statIflow,_statIflowLabels,_statIinitl,_statImain,_statImaxLabel,_statIst,_statIsvars) =
             stat_ _statOlabel _statOprocs
     in  ( _lhsOpinfo))
-- Program' ----------------------------------------------------
data Program' = Program' ((Procs')) (Stat')
              deriving ( Show)
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
          deriving ( Show)
-- cata
sem_Stat :: Stat ->
            T_Stat
sem_Stat (Skip) =
    (sem_Stat_Skip)
sem_Stat (IfThenElse _cond _stat1 _stat2) =
    (sem_Stat_IfThenElse _cond (sem_Stat _stat1) (sem_Stat _stat2))
sem_Stat (While _cond _stat) =
    (sem_Stat_While _cond (sem_Stat _stat))
sem_Stat (IAssign _name _val) =
    (sem_Stat_IAssign _name _val)
sem_Stat (BAssign _name _val) =
    (sem_Stat_BAssign _name _val)
sem_Stat (Seq _stat1 _stat2) =
    (sem_Stat_Seq (sem_Stat _stat1) (sem_Stat _stat2))
-- semantic domain
type T_Stat = Label ->
              (M.Map String Proc') ->
              ( (M.Map Label Block),([Label]),Flow,([Label]),Label,Stat',Label,Stat,(S.Set Var))
data Inh_Stat = Inh_Stat {label_Inh_Stat :: Label,procs_Inh_Stat :: (M.Map String Proc')}
data Syn_Stat = Syn_Stat {blocks_Syn_Stat :: (M.Map Label Block),flabels_Syn_Stat :: ([Label]),flow_Syn_Stat :: Flow,flowLabels_Syn_Stat :: ([Label]),initl_Syn_Stat :: Label,main_Syn_Stat :: Stat',maxLabel_Syn_Stat :: Label,st_Syn_Stat :: Stat,svars_Syn_Stat :: (S.Set Var)}
wrap_Stat :: T_Stat ->
             Inh_Stat ->
             Syn_Stat
wrap_Stat sem (Inh_Stat _lhsIlabel _lhsIprocs) =
    (let ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOmain,_lhsOmaxLabel,_lhsOst,_lhsOsvars) = sem _lhsIlabel _lhsIprocs
     in  (Syn_Stat _lhsOblocks _lhsOflabels _lhsOflow _lhsOflowLabels _lhsOinitl _lhsOmain _lhsOmaxLabel _lhsOst _lhsOsvars))
sem_Stat_Skip :: T_Stat
sem_Stat_Skip =
    (\ _lhsIlabel
       _lhsIprocs ->
         (let _lhsOmain :: Stat'
              _lhsOmaxLabel :: Label
              _lhsOflowLabels :: ([Label])
              _lhsOblocks :: (M.Map Label Block)
              _lhsOsvars :: (S.Set Var)
              _lhsOflow :: Flow
              _lhsOinitl :: Label
              _lhsOflabels :: ([Label])
              _lhsOst :: Stat
              _lhsOmain =
                  ({-# LINE 65 "Administration.ag" #-}
                   Skip' _lhsIlabel
                   {-# LINE 559 "Administration.hs" #-}
                   )
              _lhsOmaxLabel =
                  ({-# LINE 66 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 564 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 67 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 569 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 68 "Administration.ag" #-}
                   M.singleton _lhsIlabel B_Skip
                   {-# LINE 574 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 69 "Administration.ag" #-}
                   S.empty
                   {-# LINE 579 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 70 "Administration.ag" #-}
                   []
                   {-# LINE 584 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 71 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 589 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 72 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 594 "Administration.hs" #-}
                   )
              _st =
                  ({-# LINE 42 "Administration.ag" #-}
                   Skip
                   {-# LINE 599 "Administration.hs" #-}
                   )
              _lhsOst =
                  ({-# LINE 42 "Administration.ag" #-}
                   _st
                   {-# LINE 604 "Administration.hs" #-}
                   )
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOmain,_lhsOmaxLabel,_lhsOst,_lhsOsvars)))
sem_Stat_IfThenElse :: BExpr ->
                       T_Stat ->
                       T_Stat ->
                       T_Stat
sem_Stat_IfThenElse cond_ stat1_ stat2_ =
    (\ _lhsIlabel
       _lhsIprocs ->
         (let _lhsOmain :: Stat'
              _lhsOmaxLabel :: Label
              _lhsOflowLabels :: ([Label])
              _lhsOflow :: Flow
              _stat1Olabel :: Label
              _stat2Olabel :: Label
              _lhsOblocks :: (M.Map Label Block)
              _lhsOsvars :: (S.Set Var)
              _lhsOinitl :: Label
              _lhsOflabels :: ([Label])
              _lhsOst :: Stat
              _stat1Oprocs :: (M.Map String Proc')
              _stat2Oprocs :: (M.Map String Proc')
              _stat1Iblocks :: (M.Map Label Block)
              _stat1Iflabels :: ([Label])
              _stat1Iflow :: Flow
              _stat1IflowLabels :: ([Label])
              _stat1Iinitl :: Label
              _stat1Imain :: Stat'
              _stat1ImaxLabel :: Label
              _stat1Ist :: Stat
              _stat1Isvars :: (S.Set Var)
              _stat2Iblocks :: (M.Map Label Block)
              _stat2Iflabels :: ([Label])
              _stat2Iflow :: Flow
              _stat2IflowLabels :: ([Label])
              _stat2Iinitl :: Label
              _stat2Imain :: Stat'
              _stat2ImaxLabel :: Label
              _stat2Ist :: Stat
              _stat2Isvars :: (S.Set Var)
              _lhsOmain =
                  ({-# LINE 73 "Administration.ag" #-}
                   IfThenElse' _lhsIlabel cond_ _stat1Imain _stat2Imain
                   {-# LINE 648 "Administration.hs" #-}
                   )
              _lhsOmaxLabel =
                  ({-# LINE 74 "Administration.ag" #-}
                   _stat2ImaxLabel
                   {-# LINE 653 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 75 "Administration.ag" #-}
                   [_label1    ,_label2    ]
                   {-# LINE 658 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 76 "Administration.ag" #-}
                   (_lhsIlabel, _label1    ) : (_lhsIlabel, _label2    ) : _stat1Iflow ++ _stat2Iflow
                   {-# LINE 663 "Administration.hs" #-}
                   )
              _stat1Olabel =
                  ({-# LINE 77 "Administration.ag" #-}
                   _label1
                   {-# LINE 668 "Administration.hs" #-}
                   )
              _label1 =
                  ({-# LINE 78 "Administration.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 673 "Administration.hs" #-}
                   )
              _label2 =
                  ({-# LINE 79 "Administration.ag" #-}
                   _stat1ImaxLabel + 1
                   {-# LINE 678 "Administration.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 80 "Administration.ag" #-}
                   _label2
                   {-# LINE 683 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 81 "Administration.ag" #-}
                   M.union (M.union (M.singleton _lhsIlabel (B_Cond cond_)) _stat1Iblocks) _stat2Iblocks
                   {-# LINE 688 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 82 "Administration.ag" #-}
                   S.union _stat1Isvars _stat2Isvars
                   {-# LINE 693 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 83 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 698 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 84 "Administration.ag" #-}
                   [_stat1ImaxLabel,_stat2ImaxLabel]
                   {-# LINE 703 "Administration.hs" #-}
                   )
              _st =
                  ({-# LINE 42 "Administration.ag" #-}
                   IfThenElse cond_ _stat1Ist _stat2Ist
                   {-# LINE 708 "Administration.hs" #-}
                   )
              _lhsOst =
                  ({-# LINE 42 "Administration.ag" #-}
                   _st
                   {-# LINE 713 "Administration.hs" #-}
                   )
              _stat1Oprocs =
                  ({-# LINE 46 "Administration.ag" #-}
                   _lhsIprocs
                   {-# LINE 718 "Administration.hs" #-}
                   )
              _stat2Oprocs =
                  ({-# LINE 46 "Administration.ag" #-}
                   _lhsIprocs
                   {-# LINE 723 "Administration.hs" #-}
                   )
              ( _stat1Iblocks,_stat1Iflabels,_stat1Iflow,_stat1IflowLabels,_stat1Iinitl,_stat1Imain,_stat1ImaxLabel,_stat1Ist,_stat1Isvars) =
                  stat1_ _stat1Olabel _stat1Oprocs
              ( _stat2Iblocks,_stat2Iflabels,_stat2Iflow,_stat2IflowLabels,_stat2Iinitl,_stat2Imain,_stat2ImaxLabel,_stat2Ist,_stat2Isvars) =
                  stat2_ _stat2Olabel _stat2Oprocs
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOmain,_lhsOmaxLabel,_lhsOst,_lhsOsvars)))
sem_Stat_While :: BExpr ->
                  T_Stat ->
                  T_Stat
sem_Stat_While cond_ stat_ =
    (\ _lhsIlabel
       _lhsIprocs ->
         (let _lhsOmain :: Stat'
              _lhsOmaxLabel :: Label
              _lhsOflowLabels :: ([Label])
              _lhsOflow :: Flow
              _statOlabel :: Label
              _lhsOblocks :: (M.Map Label Block)
              _lhsOsvars :: (S.Set Var)
              _lhsOinitl :: Label
              _lhsOflabels :: ([Label])
              _lhsOst :: Stat
              _statOprocs :: (M.Map String Proc')
              _statIblocks :: (M.Map Label Block)
              _statIflabels :: ([Label])
              _statIflow :: Flow
              _statIflowLabels :: ([Label])
              _statIinitl :: Label
              _statImain :: Stat'
              _statImaxLabel :: Label
              _statIst :: Stat
              _statIsvars :: (S.Set Var)
              _lhsOmain =
                  ({-# LINE 85 "Administration.ag" #-}
                   While' _lhsIlabel cond_ _statImain
                   {-# LINE 759 "Administration.hs" #-}
                   )
              _lhsOmaxLabel =
                  ({-# LINE 86 "Administration.ag" #-}
                   _statImaxLabel
                   {-# LINE 764 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 87 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 769 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 88 "Administration.ag" #-}
                   (_lhsIlabel, _label1    ) : map (\x -> (x,_lhsIlabel)) _statIflabels ++ _statIflow
                   {-# LINE 774 "Administration.hs" #-}
                   )
              _label1 =
                  ({-# LINE 89 "Administration.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 779 "Administration.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 90 "Administration.ag" #-}
                   _label1
                   {-# LINE 784 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 91 "Administration.ag" #-}
                   M.union (M.singleton _lhsIlabel (B_Cond cond_)) _statIblocks
                   {-# LINE 789 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 92 "Administration.ag" #-}
                   _statIsvars
                   {-# LINE 794 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 93 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 799 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 94 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 804 "Administration.hs" #-}
                   )
              _st =
                  ({-# LINE 42 "Administration.ag" #-}
                   While cond_ _statIst
                   {-# LINE 809 "Administration.hs" #-}
                   )
              _lhsOst =
                  ({-# LINE 42 "Administration.ag" #-}
                   _st
                   {-# LINE 814 "Administration.hs" #-}
                   )
              _statOprocs =
                  ({-# LINE 46 "Administration.ag" #-}
                   _lhsIprocs
                   {-# LINE 819 "Administration.hs" #-}
                   )
              ( _statIblocks,_statIflabels,_statIflow,_statIflowLabels,_statIinitl,_statImain,_statImaxLabel,_statIst,_statIsvars) =
                  stat_ _statOlabel _statOprocs
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOmain,_lhsOmaxLabel,_lhsOst,_lhsOsvars)))
sem_Stat_IAssign :: String ->
                    IExpr ->
                    T_Stat
sem_Stat_IAssign name_ val_ =
    (\ _lhsIlabel
       _lhsIprocs ->
         (let _lhsOmain :: Stat'
              _lhsOmaxLabel :: Label
              _lhsOflowLabels :: ([Label])
              _lhsOblocks :: (M.Map Label Block)
              _lhsOsvars :: (S.Set Var)
              _lhsOflow :: Flow
              _lhsOinitl :: Label
              _lhsOflabels :: ([Label])
              _lhsOst :: Stat
              _lhsOmain =
                  ({-# LINE 95 "Administration.ag" #-}
                   IAssign' _lhsIlabel name_ val_
                   {-# LINE 842 "Administration.hs" #-}
                   )
              _lhsOmaxLabel =
                  ({-# LINE 96 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 847 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 97 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 852 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 98 "Administration.ag" #-}
                   M.singleton _lhsIlabel $ B_IAssign name_ val_
                   {-# LINE 857 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 99 "Administration.ag" #-}
                   S.singleton name_
                   {-# LINE 862 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 100 "Administration.ag" #-}
                   []
                   {-# LINE 867 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 101 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 872 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 102 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 877 "Administration.hs" #-}
                   )
              _st =
                  ({-# LINE 42 "Administration.ag" #-}
                   IAssign name_ val_
                   {-# LINE 882 "Administration.hs" #-}
                   )
              _lhsOst =
                  ({-# LINE 42 "Administration.ag" #-}
                   _st
                   {-# LINE 887 "Administration.hs" #-}
                   )
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOmain,_lhsOmaxLabel,_lhsOst,_lhsOsvars)))
sem_Stat_BAssign :: String ->
                    BExpr ->
                    T_Stat
sem_Stat_BAssign name_ val_ =
    (\ _lhsIlabel
       _lhsIprocs ->
         (let _lhsOmain :: Stat'
              _lhsOmaxLabel :: Label
              _lhsOflowLabels :: ([Label])
              _lhsOblocks :: (M.Map Label Block)
              _lhsOsvars :: (S.Set Var)
              _lhsOflow :: Flow
              _lhsOinitl :: Label
              _lhsOflabels :: ([Label])
              _lhsOst :: Stat
              _lhsOmain =
                  ({-# LINE 103 "Administration.ag" #-}
                   BAssign' _lhsIlabel name_ val_
                   {-# LINE 908 "Administration.hs" #-}
                   )
              _lhsOmaxLabel =
                  ({-# LINE 104 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 913 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 105 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 918 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 106 "Administration.ag" #-}
                   M.singleton _lhsIlabel $ B_BAssign name_ val_
                   {-# LINE 923 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 107 "Administration.ag" #-}
                   S.singleton name_
                   {-# LINE 928 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 108 "Administration.ag" #-}
                   []
                   {-# LINE 933 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 109 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 938 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 110 "Administration.ag" #-}
                   [_lhsIlabel]
                   {-# LINE 943 "Administration.hs" #-}
                   )
              _st =
                  ({-# LINE 42 "Administration.ag" #-}
                   BAssign name_ val_
                   {-# LINE 948 "Administration.hs" #-}
                   )
              _lhsOst =
                  ({-# LINE 42 "Administration.ag" #-}
                   _st
                   {-# LINE 953 "Administration.hs" #-}
                   )
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOmain,_lhsOmaxLabel,_lhsOst,_lhsOsvars)))
sem_Stat_Seq :: T_Stat ->
                T_Stat ->
                T_Stat
sem_Stat_Seq stat1_ stat2_ =
    (\ _lhsIlabel
       _lhsIprocs ->
         (let _lhsOmain :: Stat'
              _lhsOmaxLabel :: Label
              _lhsOflowLabels :: ([Label])
              _lhsOflow :: Flow
              _lhsOblocks :: (M.Map Label Block)
              _stat1Olabel :: Label
              _stat2Olabel :: Label
              _lhsOsvars :: (S.Set Var)
              _lhsOinitl :: Label
              _lhsOflabels :: ([Label])
              _lhsOst :: Stat
              _stat1Oprocs :: (M.Map String Proc')
              _stat2Oprocs :: (M.Map String Proc')
              _stat1Iblocks :: (M.Map Label Block)
              _stat1Iflabels :: ([Label])
              _stat1Iflow :: Flow
              _stat1IflowLabels :: ([Label])
              _stat1Iinitl :: Label
              _stat1Imain :: Stat'
              _stat1ImaxLabel :: Label
              _stat1Ist :: Stat
              _stat1Isvars :: (S.Set Var)
              _stat2Iblocks :: (M.Map Label Block)
              _stat2Iflabels :: ([Label])
              _stat2Iflow :: Flow
              _stat2IflowLabels :: ([Label])
              _stat2Iinitl :: Label
              _stat2Imain :: Stat'
              _stat2ImaxLabel :: Label
              _stat2Ist :: Stat
              _stat2Isvars :: (S.Set Var)
              _lhsOmain =
                  ({-# LINE 111 "Administration.ag" #-}
                   Seq' _stat1Imain _stat2Imain
                   {-# LINE 996 "Administration.hs" #-}
                   )
              _lhsOmaxLabel =
                  ({-# LINE 112 "Administration.ag" #-}
                   _stat2ImaxLabel
                   {-# LINE 1001 "Administration.hs" #-}
                   )
              _lhsOflowLabels =
                  ({-# LINE 113 "Administration.ag" #-}
                   _stat2IflowLabels
                   {-# LINE 1006 "Administration.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 114 "Administration.ag" #-}
                   map (\x -> (x,_label2    )) _stat1IflowLabels ++ _stat1Iflow ++ _stat2Iflow
                   {-# LINE 1011 "Administration.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 115 "Administration.ag" #-}
                   M.union _stat1Iblocks _stat2Iblocks
                   {-# LINE 1016 "Administration.hs" #-}
                   )
              _stat1Olabel =
                  ({-# LINE 116 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1021 "Administration.hs" #-}
                   )
              _label2 =
                  ({-# LINE 117 "Administration.ag" #-}
                   _stat1ImaxLabel + 1
                   {-# LINE 1026 "Administration.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 118 "Administration.ag" #-}
                   _label2
                   {-# LINE 1031 "Administration.hs" #-}
                   )
              _lhsOsvars =
                  ({-# LINE 119 "Administration.ag" #-}
                   S.union _stat1Isvars _stat2Isvars
                   {-# LINE 1036 "Administration.hs" #-}
                   )
              _lhsOinitl =
                  ({-# LINE 120 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1041 "Administration.hs" #-}
                   )
              _lhsOflabels =
                  ({-# LINE 121 "Administration.ag" #-}
                   _stat2Iflabels
                   {-# LINE 1046 "Administration.hs" #-}
                   )
              _st =
                  ({-# LINE 42 "Administration.ag" #-}
                   Seq _stat1Ist _stat2Ist
                   {-# LINE 1051 "Administration.hs" #-}
                   )
              _lhsOst =
                  ({-# LINE 42 "Administration.ag" #-}
                   _st
                   {-# LINE 1056 "Administration.hs" #-}
                   )
              _stat1Oprocs =
                  ({-# LINE 46 "Administration.ag" #-}
                   _lhsIprocs
                   {-# LINE 1061 "Administration.hs" #-}
                   )
              _stat2Oprocs =
                  ({-# LINE 46 "Administration.ag" #-}
                   _lhsIprocs
                   {-# LINE 1066 "Administration.hs" #-}
                   )
              ( _stat1Iblocks,_stat1Iflabels,_stat1Iflow,_stat1IflowLabels,_stat1Iinitl,_stat1Imain,_stat1ImaxLabel,_stat1Ist,_stat1Isvars) =
                  stat1_ _stat1Olabel _stat1Oprocs
              ( _stat2Iblocks,_stat2Iflabels,_stat2Iflow,_stat2IflowLabels,_stat2Iinitl,_stat2Imain,_stat2ImaxLabel,_stat2Ist,_stat2Isvars) =
                  stat2_ _stat2Olabel _stat2Oprocs
          in  ( _lhsOblocks,_lhsOflabels,_lhsOflow,_lhsOflowLabels,_lhsOinitl,_lhsOmain,_lhsOmaxLabel,_lhsOst,_lhsOsvars)))
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
           deriving ( Show)
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
    (let
     in  ( ))
sem_Stat'_While' :: Int ->
                    T_BExpr ->
                    (T_Stat') ->
                    (T_Stat')
sem_Stat'_While' labelc_ cond_ stat_ =
    (let
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
    (let
     in  ( ))
sem_Stat'_BAssign' :: Int ->
                      String ->
                      T_BExpr ->
                      (T_Stat')
sem_Stat'_BAssign' label_ name_ val_ =
    (let
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
    (let
     in  ( ))
sem_Stat'_Free' :: Int ->
                   T_IExpr ->
                   (T_Stat')
sem_Stat'_Free' label_ ptr_ =
    (let
     in  ( ))
sem_Stat'_RefAssign' :: Int ->
                        T_IExpr ->
                        T_IExpr ->
                        (T_Stat')
sem_Stat'_RefAssign' label_ ptr_ val_ =
    (let
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