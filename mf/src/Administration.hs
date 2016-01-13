

-- UUAGC 0.9.52.1 (Administration)
module Administration where
{-# LINE 1 "AttributeGrammar.ag" #-}

--import qualified Data.Map as M
--import qualified Data.Maybe as Maybe
--import qualified Data.List as L
{-# LINE 11 "Administration.hs" #-}

{-# LINE 89 "AttributeGrammar.ag" #-}

type Procs = [Proc]
type Procs' = [Proc']
type Exprs = [Expr]
{-# LINE 18 "Administration.hs" #-}

{-# LINE 3 "Administration.ag" #-}

to :: Program -> ProgramInfo
to program = pinfo_Syn_Program $ wrap_Program (sem_Program program) (Inh_Program)


toProgram' :: Program -> Program'
toProgram' program = program_Syn_Program $ wrap_Program (sem_Program program) (Inh_Program)

data ProgramInfo = ProgramInfo {labels :: [Label], init :: Label, finals :: [Label], flow :: Flow}
    deriving Show
{-# LINE 31 "Administration.hs" #-}

{-# LINE 151 "Administration.ag" #-}

type Label = Int
type Flow = [(Int, Int)]

insert :: Stat -> Int -> Int -> Flow -> Flow
insert s x y old = (x,y) : old


foldProcs :: Procs -> (Int, Procs')
foldProcs procs = foldr (\x y -> 
                let (l,proc') = wrapproc x (fst y)
                in (l + 1,proc' : snd y)) (0,[]) procs

wrapproc :: Proc -> Int -> (Int, Proc')
wrapproc proc label = main_Syn_Proc $ wrap_Proc (sem_Proc proc) (Inh_Proc label)

--Insert flow.
--P=parent
--r=root
--parent got a higher property
insertFinal :: Maybe Int -> Maybe Int -> Int -> Flow -> Flow
insertFinal (Just p) _       n old = (p,n) : old
insertFinal Nothing (Just r) n old = (n,r) : old
insertFinal Nothing Nothing  _ old = old

--Insert flow.
--P=parent
--r=root
insertRight :: Stat -> (Maybe Int) -> (Maybe Int) -> Int -> Flow -> Flow
insertRight (Seq _ _) _       _        _ old = [(1,2)]
insertRight s        (Just p) Nothing  n old = (p,n) : old
insertRight s        (Just p) (Just r) n old = (p,n) : (n,r) : old
insertRight s        Nothing (Just r)  n old = (n,r) : old
insertRight s        Nothing Nothing   _ old = old


--l=labels
--l=labels
increment :: Stat -> Int -> Int
increment (Seq _ _) l = l
increment (Call _ _ _) l = l + 2 --Call go call and return
increment s   l = l + 1

--get labels from flow
labels' :: Flow -> [Int]
labels' xs = [(minimum l)..(maximum l)]
    where l = concatTuple $ unzip xs

concatTuple :: ([a],[a]) -> [a]
concatTuple (a,b) = a ++ b

init' :: Flow -> Int
init' = fst . head

finals' :: Flow -> [Int]
finals' [] = []
finals' l@((_,y):xs) = if isFinal
                         then y : finals' xs
                         else finals' xs
                      where isFinal = 0 == length [y1 | (x1,y1) <- xs, x1 == y]
{-# LINE 94 "Administration.hs" #-}
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
              ( ((Int,Proc')))
data Inh_Proc = Inh_Proc {entryPoint_Inh_Proc :: Label}
data Syn_Proc = Syn_Proc {main_Syn_Proc :: ((Int,Proc'))}
wrap_Proc :: T_Proc ->
             Inh_Proc ->
             Syn_Proc
wrap_Proc sem (Inh_Proc _lhsIentryPoint) =
    (let ( _lhsOmain) = sem _lhsIentryPoint
     in  (Syn_Proc _lhsOmain))
sem_Proc_Proc :: String ->
                 ([String]) ->
                 String ->
                 T_Stat ->
                 T_Proc
sem_Proc_Proc name_ inp_ out_ stat_ =
    (\ _lhsIentryPoint ->
         (let _lhsOmain :: ((Int,Proc'))
              _statOlabel :: Label
              _statOroot :: (Maybe Label)
              _statOparent :: (Maybe Label)
              _statOflow :: Flow
              _statIfinalFlow :: Flow
              _statImain :: Stat'
              _statImaxLabel :: Int
              _statIstat :: Stat
              _lhsOmain =
                  ({-# LINE 82 "Administration.ag" #-}
                   (_statImaxLabel + 1, Proc' _lhsIentryPoint (_statImaxLabel + 1) name_ inp_ out_ _statImain)
                   {-# LINE 404 "Administration.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 83 "Administration.ag" #-}
                   _lhsIentryPoint + 1
                   {-# LINE 409 "Administration.hs" #-}
                   )
              _statOroot =
                  ({-# LINE 84 "Administration.ag" #-}
                   Nothing
                   {-# LINE 414 "Administration.hs" #-}
                   )
              _statOparent =
                  ({-# LINE 85 "Administration.ag" #-}
                   Nothing
                   {-# LINE 419 "Administration.hs" #-}
                   )
              _statOflow =
                  ({-# LINE 86 "Administration.ag" #-}
                   []
                   {-# LINE 424 "Administration.hs" #-}
                   )
              ( _statIfinalFlow,_statImain,_statImaxLabel,_statIstat) =
                  stat_ _statOflow _statOlabel _statOparent _statOroot
          in  ( _lhsOmain)))
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
type T_Program = ( ProgramInfo,Program')
data Inh_Program = Inh_Program {}
data Syn_Program = Syn_Program {pinfo_Syn_Program :: ProgramInfo,program_Syn_Program :: Program'}
wrap_Program :: T_Program ->
                Inh_Program ->
                Syn_Program
wrap_Program sem (Inh_Program) =
    (let ( _lhsOpinfo,_lhsOprogram) = sem
     in  (Syn_Program _lhsOpinfo _lhsOprogram))
sem_Program_Program :: Procs ->
                       T_Stat ->
                       T_Program
sem_Program_Program procs_ stat_ =
    (let _lhsOprogram :: Program'
         _lhsOpinfo :: ProgramInfo
         _statOlabel :: Label
         _statOroot :: (Maybe Label)
         _statOparent :: (Maybe Label)
         _statOflow :: Flow
         _statIfinalFlow :: Flow
         _statImain :: Stat'
         _statImaxLabel :: Int
         _statIstat :: Stat
         _lhsOprogram =
             ({-# LINE 59 "Administration.ag" #-}
              Program' (snd _procs    ) _statImain
              {-# LINE 492 "Administration.hs" #-}
              )
         _lhsOpinfo =
             ({-# LINE 60 "Administration.ag" #-}
              ProgramInfo [1 .. _statImaxLabel] 1 (finals' _statIfinalFlow) _statIfinalFlow
              {-# LINE 497 "Administration.hs" #-}
              )
         _statOlabel =
             ({-# LINE 61 "Administration.ag" #-}
              fst _procs
              {-# LINE 502 "Administration.hs" #-}
              )
         _procs =
             ({-# LINE 62 "Administration.ag" #-}
              foldProcs procs_
              {-# LINE 507 "Administration.hs" #-}
              )
         _statOroot =
             ({-# LINE 63 "Administration.ag" #-}
              Nothing
              {-# LINE 512 "Administration.hs" #-}
              )
         _statOparent =
             ({-# LINE 64 "Administration.ag" #-}
              Nothing
              {-# LINE 517 "Administration.hs" #-}
              )
         _statOflow =
             ({-# LINE 65 "Administration.ag" #-}
              []
              {-# LINE 522 "Administration.hs" #-}
              )
         ( _statIfinalFlow,_statImain,_statImaxLabel,_statIstat) =
             stat_ _statOflow _statOlabel _statOparent _statOroot
     in  ( _lhsOpinfo,_lhsOprogram))
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
          | Call (String) (Exprs) (String)
          | IAssign (String) (IExpr)
          | BAssign (String) (BExpr)
          | Seq (Stat) (Stat)
          | Malloc (String) (IExpr)
          | Free (IExpr)
          | RefAssign (IExpr) (IExpr)
          | Continue
          | Break
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
sem_Stat (Call _name _params _out) =
    (sem_Stat_Call _name _params _out)
sem_Stat (IAssign _name _val) =
    (sem_Stat_IAssign _name _val)
sem_Stat (BAssign _name _val) =
    (sem_Stat_BAssign _name _val)
sem_Stat (Seq _stat1 _stat2) =
    (sem_Stat_Seq (sem_Stat _stat1) (sem_Stat _stat2))
sem_Stat (Malloc _name _size) =
    (sem_Stat_Malloc _name _size)
sem_Stat (Free _ptr) =
    (sem_Stat_Free _ptr)
sem_Stat (RefAssign _ptr _val) =
    (sem_Stat_RefAssign _ptr _val)
sem_Stat (Continue) =
    (sem_Stat_Continue)
sem_Stat (Break) =
    (sem_Stat_Break)
-- semantic domain
type T_Stat = Flow ->
              Label ->
              (Maybe Label) ->
              (Maybe Label) ->
              ( Flow,Stat',Int,Stat)
data Inh_Stat = Inh_Stat {flow_Inh_Stat :: Flow,label_Inh_Stat :: Label,parent_Inh_Stat :: (Maybe Label),root_Inh_Stat :: (Maybe Label)}
data Syn_Stat = Syn_Stat {finalFlow_Syn_Stat :: Flow,main_Syn_Stat :: Stat',maxLabel_Syn_Stat :: Int,stat_Syn_Stat :: Stat}
wrap_Stat :: T_Stat ->
             Inh_Stat ->
             Syn_Stat
wrap_Stat sem (Inh_Stat _lhsIflow _lhsIlabel _lhsIparent _lhsIroot) =
    (let ( _lhsOfinalFlow,_lhsOmain,_lhsOmaxLabel,_lhsOstat) = sem _lhsIflow _lhsIlabel _lhsIparent _lhsIroot
     in  (Syn_Stat _lhsOfinalFlow _lhsOmain _lhsOmaxLabel _lhsOstat))
sem_Stat_Skip :: T_Stat
sem_Stat_Skip =
    (\ _lhsIflow
       _lhsIlabel
       _lhsIparent
       _lhsIroot ->
         (let _lhsOmain :: Stat'
              _lhsOmaxLabel :: Int
              _lhsOfinalFlow :: Flow
              _lhsOstat :: Stat
              _lhsOmain =
                  ({-# LINE 89 "Administration.ag" #-}
                   Skip' _lhsIlabel
                   {-# LINE 619 "Administration.hs" #-}
                   )
              _lhsOmaxLabel =
                  ({-# LINE 90 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 624 "Administration.hs" #-}
                   )
              _lhsOfinalFlow =
                  ({-# LINE 91 "Administration.ag" #-}
                   insertFinal _lhsIparent _lhsIroot _lhsIlabel _lhsIflow
                   {-# LINE 629 "Administration.hs" #-}
                   )
              _stat =
                  ({-# LINE 73 "Administration.ag" #-}
                   Skip
                   {-# LINE 634 "Administration.hs" #-}
                   )
              _lhsOstat =
                  ({-# LINE 73 "Administration.ag" #-}
                   _stat
                   {-# LINE 639 "Administration.hs" #-}
                   )
          in  ( _lhsOfinalFlow,_lhsOmain,_lhsOmaxLabel,_lhsOstat)))
sem_Stat_IfThenElse :: BExpr ->
                       T_Stat ->
                       T_Stat ->
                       T_Stat
sem_Stat_IfThenElse cond_ stat1_ stat2_ =
    (\ _lhsIflow
       _lhsIlabel
       _lhsIparent
       _lhsIroot ->
         (let _lhsOmain :: Stat'
              _lhsOmaxLabel :: Int
              _lhsOfinalFlow :: Flow
              _stat1Olabel :: Label
              _stat2Olabel :: Label
              _stat1Oflow :: Flow
              _stat2Oflow :: Flow
              _stat1Oparent :: (Maybe Label)
              _stat1Oroot :: (Maybe Label)
              _stat2Oparent :: (Maybe Label)
              _stat2Oroot :: (Maybe Label)
              _lhsOstat :: Stat
              _stat1IfinalFlow :: Flow
              _stat1Imain :: Stat'
              _stat1ImaxLabel :: Int
              _stat1Istat :: Stat
              _stat2IfinalFlow :: Flow
              _stat2Imain :: Stat'
              _stat2ImaxLabel :: Int
              _stat2Istat :: Stat
              _lhsOmain =
                  ({-# LINE 92 "Administration.ag" #-}
                   IfThenElse' _lhsIlabel cond_ _stat1Imain _stat2Imain
                   {-# LINE 674 "Administration.hs" #-}
                   )
              _lhsOmaxLabel =
                  ({-# LINE 93 "Administration.ag" #-}
                   _stat2ImaxLabel
                   {-# LINE 679 "Administration.hs" #-}
                   )
              _lhsOfinalFlow =
                  ({-# LINE 94 "Administration.ag" #-}
                   _finalFlow
                   {-# LINE 684 "Administration.hs" #-}
                   )
              _finalFlow =
                  ({-# LINE 95 "Administration.ag" #-}
                   insertFinal _lhsIparent _lhsIroot _lhsIlabel _lhsIflow
                   {-# LINE 689 "Administration.hs" #-}
                   )
              _stat1Olabel =
                  ({-# LINE 96 "Administration.ag" #-}
                   _label1
                   {-# LINE 694 "Administration.hs" #-}
                   )
              _label1 =
                  ({-# LINE 97 "Administration.ag" #-}
                   increment _stat1Istat _lhsIlabel
                   {-# LINE 699 "Administration.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 98 "Administration.ag" #-}
                   increment _stat2Istat _stat1ImaxLabel
                   {-# LINE 704 "Administration.hs" #-}
                   )
              _stat1Oflow =
                  ({-# LINE 99 "Administration.ag" #-}
                   insert _stat1Istat _lhsIlabel _label1           _finalFlow
                   {-# LINE 709 "Administration.hs" #-}
                   )
              _stat2Oflow =
                  ({-# LINE 100 "Administration.ag" #-}
                   insert _stat2Istat _lhsIlabel _stat1ImaxLabel _stat1IfinalFlow
                   {-# LINE 714 "Administration.hs" #-}
                   )
              _stat1Oparent =
                  ({-# LINE 101 "Administration.ag" #-}
                   Just _lhsIlabel
                   {-# LINE 719 "Administration.hs" #-}
                   )
              _stat1Oroot =
                  ({-# LINE 102 "Administration.ag" #-}
                   Nothing
                   {-# LINE 724 "Administration.hs" #-}
                   )
              _stat2Oparent =
                  ({-# LINE 103 "Administration.ag" #-}
                   Just _lhsIlabel
                   {-# LINE 729 "Administration.hs" #-}
                   )
              _stat2Oroot =
                  ({-# LINE 104 "Administration.ag" #-}
                   Nothing
                   {-# LINE 734 "Administration.hs" #-}
                   )
              _stat =
                  ({-# LINE 73 "Administration.ag" #-}
                   IfThenElse cond_ _stat1Istat _stat2Istat
                   {-# LINE 739 "Administration.hs" #-}
                   )
              _lhsOstat =
                  ({-# LINE 73 "Administration.ag" #-}
                   _stat
                   {-# LINE 744 "Administration.hs" #-}
                   )
              ( _stat1IfinalFlow,_stat1Imain,_stat1ImaxLabel,_stat1Istat) =
                  stat1_ _stat1Oflow _stat1Olabel _stat1Oparent _stat1Oroot
              ( _stat2IfinalFlow,_stat2Imain,_stat2ImaxLabel,_stat2Istat) =
                  stat2_ _stat2Oflow _stat2Olabel _stat2Oparent _stat2Oroot
          in  ( _lhsOfinalFlow,_lhsOmain,_lhsOmaxLabel,_lhsOstat)))
sem_Stat_While :: BExpr ->
                  T_Stat ->
                  T_Stat
sem_Stat_While cond_ stat_ =
    (\ _lhsIflow
       _lhsIlabel
       _lhsIparent
       _lhsIroot ->
         (let _lhsOmain :: Stat'
              _lhsOfinalFlow :: Flow
              _statOlabel :: Label
              _lhsOmaxLabel :: Int
              _statOroot :: (Maybe Label)
              _statOparent :: (Maybe Label)
              _statOflow :: Flow
              _lhsOstat :: Stat
              _statIfinalFlow :: Flow
              _statImain :: Stat'
              _statImaxLabel :: Int
              _statIstat :: Stat
              _lhsOmain =
                  ({-# LINE 105 "Administration.ag" #-}
                   While' _lhsIlabel cond_ _statImain
                   {-# LINE 774 "Administration.hs" #-}
                   )
              _lhsOfinalFlow =
                  ({-# LINE 106 "Administration.ag" #-}
                   insertFinal _lhsIparent _lhsIroot _lhsIlabel _lhsIflow
                   {-# LINE 779 "Administration.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 107 "Administration.ag" #-}
                   _label1
                   {-# LINE 784 "Administration.hs" #-}
                   )
              _label1 =
                  ({-# LINE 108 "Administration.ag" #-}
                   increment _statIstat _lhsIlabel
                   {-# LINE 789 "Administration.hs" #-}
                   )
              _lhsOmaxLabel =
                  ({-# LINE 109 "Administration.ag" #-}
                   _statImaxLabel
                   {-# LINE 794 "Administration.hs" #-}
                   )
              _statOroot =
                  ({-# LINE 110 "Administration.ag" #-}
                   Just _lhsIlabel
                   {-# LINE 799 "Administration.hs" #-}
                   )
              _statOparent =
                  ({-# LINE 111 "Administration.ag" #-}
                   Nothing
                   {-# LINE 804 "Administration.hs" #-}
                   )
              _statOflow =
                  ({-# LINE 112 "Administration.ag" #-}
                   insert _statIstat _lhsIlabel _label1     _lhsIflow
                   {-# LINE 809 "Administration.hs" #-}
                   )
              _stat =
                  ({-# LINE 73 "Administration.ag" #-}
                   While cond_ _statIstat
                   {-# LINE 814 "Administration.hs" #-}
                   )
              _lhsOstat =
                  ({-# LINE 73 "Administration.ag" #-}
                   _stat
                   {-# LINE 819 "Administration.hs" #-}
                   )
              ( _statIfinalFlow,_statImain,_statImaxLabel,_statIstat) =
                  stat_ _statOflow _statOlabel _statOparent _statOroot
          in  ( _lhsOfinalFlow,_lhsOmain,_lhsOmaxLabel,_lhsOstat)))
sem_Stat_Call :: String ->
                 Exprs ->
                 String ->
                 T_Stat
sem_Stat_Call name_ params_ out_ =
    (\ _lhsIflow
       _lhsIlabel
       _lhsIparent
       _lhsIroot ->
         (let _lhsOmain :: Stat'
              _lhsOmaxLabel :: Int
              _lhsOfinalFlow :: Flow
              _lhsOstat :: Stat
              _lhsOmain =
                  ({-# LINE 113 "Administration.ag" #-}
                   Call' (_lhsIlabel - 1) _lhsIlabel name_ params_ out_
                   {-# LINE 840 "Administration.hs" #-}
                   )
              _lhsOmaxLabel =
                  ({-# LINE 114 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 845 "Administration.hs" #-}
                   )
              _lhsOfinalFlow =
                  ({-# LINE 115 "Administration.ag" #-}
                   insertFinal _lhsIparent _lhsIroot _lhsIlabel _lhsIflow
                   {-# LINE 850 "Administration.hs" #-}
                   )
              _stat =
                  ({-# LINE 73 "Administration.ag" #-}
                   Call name_ params_ out_
                   {-# LINE 855 "Administration.hs" #-}
                   )
              _lhsOstat =
                  ({-# LINE 73 "Administration.ag" #-}
                   _stat
                   {-# LINE 860 "Administration.hs" #-}
                   )
          in  ( _lhsOfinalFlow,_lhsOmain,_lhsOmaxLabel,_lhsOstat)))
sem_Stat_IAssign :: String ->
                    IExpr ->
                    T_Stat
sem_Stat_IAssign name_ val_ =
    (\ _lhsIflow
       _lhsIlabel
       _lhsIparent
       _lhsIroot ->
         (let _lhsOmain :: Stat'
              _lhsOmaxLabel :: Int
              _lhsOfinalFlow :: Flow
              _lhsOstat :: Stat
              _lhsOmain =
                  ({-# LINE 116 "Administration.ag" #-}
                   IAssign' _lhsIlabel name_ val_
                   {-# LINE 878 "Administration.hs" #-}
                   )
              _lhsOmaxLabel =
                  ({-# LINE 117 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 883 "Administration.hs" #-}
                   )
              _lhsOfinalFlow =
                  ({-# LINE 118 "Administration.ag" #-}
                   insertFinal _lhsIparent _lhsIroot _lhsIlabel _lhsIflow
                   {-# LINE 888 "Administration.hs" #-}
                   )
              _stat =
                  ({-# LINE 73 "Administration.ag" #-}
                   IAssign name_ val_
                   {-# LINE 893 "Administration.hs" #-}
                   )
              _lhsOstat =
                  ({-# LINE 73 "Administration.ag" #-}
                   _stat
                   {-# LINE 898 "Administration.hs" #-}
                   )
          in  ( _lhsOfinalFlow,_lhsOmain,_lhsOmaxLabel,_lhsOstat)))
sem_Stat_BAssign :: String ->
                    BExpr ->
                    T_Stat
sem_Stat_BAssign name_ val_ =
    (\ _lhsIflow
       _lhsIlabel
       _lhsIparent
       _lhsIroot ->
         (let _lhsOmain :: Stat'
              _lhsOmaxLabel :: Int
              _lhsOfinalFlow :: Flow
              _lhsOstat :: Stat
              _lhsOmain =
                  ({-# LINE 119 "Administration.ag" #-}
                   BAssign' _lhsIlabel name_ val_
                   {-# LINE 916 "Administration.hs" #-}
                   )
              _lhsOmaxLabel =
                  ({-# LINE 120 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 921 "Administration.hs" #-}
                   )
              _lhsOfinalFlow =
                  ({-# LINE 121 "Administration.ag" #-}
                   insertFinal _lhsIparent _lhsIroot _lhsIlabel _lhsIflow
                   {-# LINE 926 "Administration.hs" #-}
                   )
              _stat =
                  ({-# LINE 73 "Administration.ag" #-}
                   BAssign name_ val_
                   {-# LINE 931 "Administration.hs" #-}
                   )
              _lhsOstat =
                  ({-# LINE 73 "Administration.ag" #-}
                   _stat
                   {-# LINE 936 "Administration.hs" #-}
                   )
          in  ( _lhsOfinalFlow,_lhsOmain,_lhsOmaxLabel,_lhsOstat)))
sem_Stat_Seq :: T_Stat ->
                T_Stat ->
                T_Stat
sem_Stat_Seq stat1_ stat2_ =
    (\ _lhsIflow
       _lhsIlabel
       _lhsIparent
       _lhsIroot ->
         (let _lhsOmain :: Stat'
              _lhsOmaxLabel :: Int
              _lhsOfinalFlow :: Flow
              _stat1Olabel :: Label
              _stat2Olabel :: Label
              _stat1Oparent :: (Maybe Label)
              _stat2Oparent :: (Maybe Label)
              _stat1Oroot :: (Maybe Label)
              _stat2Oroot :: (Maybe Label)
              _stat1Oflow :: Flow
              _stat2Oflow :: Flow
              _lhsOstat :: Stat
              _stat1IfinalFlow :: Flow
              _stat1Imain :: Stat'
              _stat1ImaxLabel :: Int
              _stat1Istat :: Stat
              _stat2IfinalFlow :: Flow
              _stat2Imain :: Stat'
              _stat2ImaxLabel :: Int
              _stat2Istat :: Stat
              _lhsOmain =
                  ({-# LINE 122 "Administration.ag" #-}
                   Seq' _stat1Imain _stat2Imain
                   {-# LINE 970 "Administration.hs" #-}
                   )
              _lhsOmaxLabel =
                  ({-# LINE 123 "Administration.ag" #-}
                   _stat2ImaxLabel
                   {-# LINE 975 "Administration.hs" #-}
                   )
              _lhsOfinalFlow =
                  ({-# LINE 124 "Administration.ag" #-}
                   _stat2IfinalFlow
                   {-# LINE 980 "Administration.hs" #-}
                   )
              _stat1Olabel =
                  ({-# LINE 125 "Administration.ag" #-}
                   increment _stat1Istat _lhsIlabel
                   {-# LINE 985 "Administration.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 126 "Administration.ag" #-}
                   increment _stat2Istat _stat1ImaxLabel
                   {-# LINE 990 "Administration.hs" #-}
                   )
              _stat1Oparent =
                  ({-# LINE 127 "Administration.ag" #-}
                   _lhsIparent
                   {-# LINE 995 "Administration.hs" #-}
                   )
              _stat2Oparent =
                  ({-# LINE 128 "Administration.ag" #-}
                   Just _stat1ImaxLabel
                   {-# LINE 1000 "Administration.hs" #-}
                   )
              _stat1Oroot =
                  ({-# LINE 129 "Administration.ag" #-}
                   Nothing
                   {-# LINE 1005 "Administration.hs" #-}
                   )
              _stat2Oroot =
                  ({-# LINE 130 "Administration.ag" #-}
                   _lhsIroot
                   {-# LINE 1010 "Administration.hs" #-}
                   )
              _stat1Oflow =
                  ({-# LINE 131 "Administration.ag" #-}
                   _lhsIflow
                   {-# LINE 1015 "Administration.hs" #-}
                   )
              _stat2Oflow =
                  ({-# LINE 132 "Administration.ag" #-}
                   _stat1IfinalFlow
                   {-# LINE 1020 "Administration.hs" #-}
                   )
              _stat =
                  ({-# LINE 73 "Administration.ag" #-}
                   Seq _stat1Istat _stat2Istat
                   {-# LINE 1025 "Administration.hs" #-}
                   )
              _lhsOstat =
                  ({-# LINE 73 "Administration.ag" #-}
                   _stat
                   {-# LINE 1030 "Administration.hs" #-}
                   )
              ( _stat1IfinalFlow,_stat1Imain,_stat1ImaxLabel,_stat1Istat) =
                  stat1_ _stat1Oflow _stat1Olabel _stat1Oparent _stat1Oroot
              ( _stat2IfinalFlow,_stat2Imain,_stat2ImaxLabel,_stat2Istat) =
                  stat2_ _stat2Oflow _stat2Olabel _stat2Oparent _stat2Oroot
          in  ( _lhsOfinalFlow,_lhsOmain,_lhsOmaxLabel,_lhsOstat)))
sem_Stat_Malloc :: String ->
                   IExpr ->
                   T_Stat
sem_Stat_Malloc name_ size_ =
    (\ _lhsIflow
       _lhsIlabel
       _lhsIparent
       _lhsIroot ->
         (let _lhsOmain :: Stat'
              _lhsOmaxLabel :: Int
              _lhsOfinalFlow :: Flow
              _lhsOstat :: Stat
              _lhsOmain =
                  ({-# LINE 133 "Administration.ag" #-}
                   Malloc' _lhsIlabel name_ size_
                   {-# LINE 1052 "Administration.hs" #-}
                   )
              _lhsOmaxLabel =
                  ({-# LINE 134 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1057 "Administration.hs" #-}
                   )
              _lhsOfinalFlow =
                  ({-# LINE 135 "Administration.ag" #-}
                   insertFinal _lhsIparent _lhsIroot _lhsIlabel _lhsIflow
                   {-# LINE 1062 "Administration.hs" #-}
                   )
              _stat =
                  ({-# LINE 73 "Administration.ag" #-}
                   Malloc name_ size_
                   {-# LINE 1067 "Administration.hs" #-}
                   )
              _lhsOstat =
                  ({-# LINE 73 "Administration.ag" #-}
                   _stat
                   {-# LINE 1072 "Administration.hs" #-}
                   )
          in  ( _lhsOfinalFlow,_lhsOmain,_lhsOmaxLabel,_lhsOstat)))
sem_Stat_Free :: IExpr ->
                 T_Stat
sem_Stat_Free ptr_ =
    (\ _lhsIflow
       _lhsIlabel
       _lhsIparent
       _lhsIroot ->
         (let _lhsOmain :: Stat'
              _lhsOmaxLabel :: Int
              _lhsOfinalFlow :: Flow
              _lhsOstat :: Stat
              _lhsOmain =
                  ({-# LINE 136 "Administration.ag" #-}
                   Free' _lhsIlabel ptr_
                   {-# LINE 1089 "Administration.hs" #-}
                   )
              _lhsOmaxLabel =
                  ({-# LINE 137 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1094 "Administration.hs" #-}
                   )
              _lhsOfinalFlow =
                  ({-# LINE 138 "Administration.ag" #-}
                   insertFinal _lhsIparent _lhsIroot _lhsIlabel _lhsIflow
                   {-# LINE 1099 "Administration.hs" #-}
                   )
              _stat =
                  ({-# LINE 73 "Administration.ag" #-}
                   Free ptr_
                   {-# LINE 1104 "Administration.hs" #-}
                   )
              _lhsOstat =
                  ({-# LINE 73 "Administration.ag" #-}
                   _stat
                   {-# LINE 1109 "Administration.hs" #-}
                   )
          in  ( _lhsOfinalFlow,_lhsOmain,_lhsOmaxLabel,_lhsOstat)))
sem_Stat_RefAssign :: IExpr ->
                      IExpr ->
                      T_Stat
sem_Stat_RefAssign ptr_ val_ =
    (\ _lhsIflow
       _lhsIlabel
       _lhsIparent
       _lhsIroot ->
         (let _lhsOmain :: Stat'
              _lhsOmaxLabel :: Int
              _lhsOfinalFlow :: Flow
              _lhsOstat :: Stat
              _lhsOmain =
                  ({-# LINE 139 "Administration.ag" #-}
                   RefAssign' _lhsIlabel ptr_ val_
                   {-# LINE 1127 "Administration.hs" #-}
                   )
              _lhsOmaxLabel =
                  ({-# LINE 140 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1132 "Administration.hs" #-}
                   )
              _lhsOfinalFlow =
                  ({-# LINE 141 "Administration.ag" #-}
                   insertFinal _lhsIparent _lhsIroot _lhsIlabel _lhsIflow
                   {-# LINE 1137 "Administration.hs" #-}
                   )
              _stat =
                  ({-# LINE 73 "Administration.ag" #-}
                   RefAssign ptr_ val_
                   {-# LINE 1142 "Administration.hs" #-}
                   )
              _lhsOstat =
                  ({-# LINE 73 "Administration.ag" #-}
                   _stat
                   {-# LINE 1147 "Administration.hs" #-}
                   )
          in  ( _lhsOfinalFlow,_lhsOmain,_lhsOmaxLabel,_lhsOstat)))
sem_Stat_Continue :: T_Stat
sem_Stat_Continue =
    (\ _lhsIflow
       _lhsIlabel
       _lhsIparent
       _lhsIroot ->
         (let _lhsOmain :: Stat'
              _lhsOmaxLabel :: Int
              _lhsOfinalFlow :: Flow
              _lhsOstat :: Stat
              _lhsOmain =
                  ({-# LINE 142 "Administration.ag" #-}
                   Continue' _lhsIlabel
                   {-# LINE 1163 "Administration.hs" #-}
                   )
              _lhsOmaxLabel =
                  ({-# LINE 143 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1168 "Administration.hs" #-}
                   )
              _lhsOfinalFlow =
                  ({-# LINE 144 "Administration.ag" #-}
                   insertFinal _lhsIparent _lhsIroot _lhsIlabel _lhsIflow
                   {-# LINE 1173 "Administration.hs" #-}
                   )
              _stat =
                  ({-# LINE 73 "Administration.ag" #-}
                   Continue
                   {-# LINE 1178 "Administration.hs" #-}
                   )
              _lhsOstat =
                  ({-# LINE 73 "Administration.ag" #-}
                   _stat
                   {-# LINE 1183 "Administration.hs" #-}
                   )
          in  ( _lhsOfinalFlow,_lhsOmain,_lhsOmaxLabel,_lhsOstat)))
sem_Stat_Break :: T_Stat
sem_Stat_Break =
    (\ _lhsIflow
       _lhsIlabel
       _lhsIparent
       _lhsIroot ->
         (let _lhsOmain :: Stat'
              _lhsOmaxLabel :: Int
              _lhsOfinalFlow :: Flow
              _lhsOstat :: Stat
              _lhsOmain =
                  ({-# LINE 145 "Administration.ag" #-}
                   Break' _lhsIlabel
                   {-# LINE 1199 "Administration.hs" #-}
                   )
              _lhsOmaxLabel =
                  ({-# LINE 146 "Administration.ag" #-}
                   _lhsIlabel
                   {-# LINE 1204 "Administration.hs" #-}
                   )
              _lhsOfinalFlow =
                  ({-# LINE 147 "Administration.ag" #-}
                   insertFinal _lhsIparent _lhsIroot _lhsIlabel _lhsIflow
                   {-# LINE 1209 "Administration.hs" #-}
                   )
              _stat =
                  ({-# LINE 73 "Administration.ag" #-}
                   Break
                   {-# LINE 1214 "Administration.hs" #-}
                   )
              _lhsOstat =
                  ({-# LINE 73 "Administration.ag" #-}
                   _stat
                   {-# LINE 1219 "Administration.hs" #-}
                   )
          in  ( _lhsOfinalFlow,_lhsOmain,_lhsOmaxLabel,_lhsOstat)))
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