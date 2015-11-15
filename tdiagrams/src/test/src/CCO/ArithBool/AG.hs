

-- UUAGC 0.9.52.1 (AG.ag)
module CCO.ArithBool.AG where

{-# LINE 2 "AG\\Typing.ag" #-}

import CCO.Printing
import CCO.SourcePos        (SourcePos)
import CCO.Tree             (ATerm (App), Tree (fromTree, toTree))
import CCO.Tree.Parser      (parseTree, app)
import Control.Applicative  (Applicative (pure))
{-# LINE 14 "AG.hs" #-}

{-# LINE 2 "AG\\Printing.ag" #-}

import CCO.Printing
{-# LINE 19 "AG.hs" #-}

{-# LINE 2 "AG\\Pos.ag" #-}

import CCO.SourcePos  (SourcePos)
{-# LINE 24 "AG.hs" #-}

{-# LINE 2 "AG\\Evaluation.ag" #-}

import CCO.Feedback         (Feedback, errorMessage)
import CCO.Printing
import CCO.SourcePos        (Source (..), Pos (..), SourcePos (..))
import CCO.Tree             (ATerm (App), Tree (fromTree, toTree))
import CCO.Tree.Parser      (parseTree, app, arg)
import Control.Applicative  (Applicative (pure), (<$>))
import Control.Monad        (when, unless)
{-# LINE 35 "AG.hs" #-}

{-# LINE 2 "AG\\Base.ag" #-}

import CCO.SourcePos  (SourcePos)
{-# LINE 40 "AG.hs" #-}
{-# LINE 14 "AG\\Typing.ag" #-}

-- | Type of types.
-- Join-semilattice with greatest element 'Top'.
data Ty = Nat | Bool | Top deriving (Eq, Show)

instance Tree Ty where
  fromTree Nat  = App "Nat"  []
  fromTree Bool = App "Bool" []
  fromTree Top  = App "Top"  []

  toTree = parseTree [ app "Nat"  (pure Nat )
                     , app "Bool" (pure Bool)
                     , app "Top"  (pure Top )
                     ]

-- | Retrieves whether two 'Ty's match.
-- Two 'Ty's match if they are the same or if one of them is 'Top'.
match :: Ty -> Ty -> Bool
match Top _   = True
match _ Top   = True
match ty1 ty2 = ty1 == ty2

-- | Retrieves the least upper bound of two 'Ty's.
lub :: Ty -> Ty -> Ty
lub ty1 ty2 = if ty1 == ty2 then ty1 else Top
{-# LINE 67 "AG.hs" #-}

{-# LINE 61 "AG\\Typing.ag" #-}

-- | Checks the type of the guard of a conditional.
checkTyGuard :: SourcePos -> Ty -> [TyErr]
checkTyGuard _   ty | ty `match` Bool = []
checkTyGuard pos ty                   = [TyErr pos descr Bool ty]
  where
    descr = "guard of a conditional should be a boolean"

-- | Checks that both branches of a conditional have the same type.
checkTyBranches :: SourcePos -> Ty -> Ty -> [TyErr]
checkTyBranches pos tyThen tyElse
  | tyThen `match` tyElse = []
  | otherwise             = [TyErr pos descr tyThen tyElse]
  where
    descr = "branches of a conditional should have the same type"
{-# LINE 85 "AG.hs" #-}

{-# LINE 83 "AG\\Typing.ag" #-}

-- | Checks the type of an operand of an arithmetic operator.
checkTyArithOp :: SourcePos -> Ty -> [TyErr]
checkTyArithOp _   ty | ty `match` Nat  = []
checkTyArithOp pos ty                   = [TyErr pos descr Nat ty]
  where
    descr = "operand of an arithmetic operator should be a natural number"
{-# LINE 95 "AG.hs" #-}

{-# LINE 97 "AG\\Typing.ag" #-}

-- | Checks the type of an operand of a relational operator.
checkTyRelOp :: SourcePos -> Ty -> [TyErr]
checkTyRelOp _   ty | ty `match` Nat = []
checkTyRelOp pos ty                  = [TyErr pos descr Nat ty]
  where
    descr = "operand of a relational operator should be a natural number"
{-# LINE 105 "AG.hs" #-}

{-# LINE 110 "AG\\Typing.ag" #-}

-- | Type of type errors.
data TyErr
  = TyErr SourcePos String Ty Ty  -- ^ Holds a source position, a description,
                                  --   the expected type and the inferred
                                  --   type.

instance Printable TyErr where
  pp = ppTyErr
{-# LINE 117 "AG.hs" #-}

{-# LINE 125 "AG\\Typing.ag" #-}

-- | Pretty prints a type error message.
ppTyErr :: TyErr -> Doc
ppTyErr (TyErr pos descr expected inferred)
  = above [ppHeader, text " ", ppExpected, ppInferred]
  where
    ppHeader   = wrapped $
                 describeSourcePos pos ++ ": Type error: " ++ descr ++ "."
    ppExpected = text "? expected : " >|< showable expected
    ppInferred = text "? inferred : " >|< showable inferred
{-# LINE 130 "AG.hs" #-}

{-# LINE 21 "AG\\Printing.ag" #-}

-- | Pretty prints a conditional.
ppIf :: Doc -> Doc -> Doc -> Doc
ppIf guard then_ else_ = singleLine >//< multiLine >//< indented
  where
    singleLine = ppIf_ >#< guard >#<
                 ppThen >#< then_ >#<
                 ppElse >#< else_ >#<
                 ppFi
    multiLine  = ppIf_ >|< text "   " >|< guard >-<
                 ppThen >#< then_ >-<
                 ppElse >#< else_ >-<
                 ppFi
    indented   = ppIf_ >-< indent 2 guard >-<
                 ppThen >-< indent 2 then_ >-<
                 ppElse >-< indent 2 else_ >-<
                 ppFi
    ppIf_      = text "if"
    ppThen     = text "then"
    ppElse     = text "else"
    ppFi       = text "fi"
{-# LINE 154 "AG.hs" #-}

{-# LINE 57 "AG\\Printing.ag" #-}

-- | Type of precedence levels.
type Prec = Int
{-# LINE 160 "AG.hs" #-}

{-# LINE 76 "AG\\Printing.ag" #-}

-- | Pretty prints, given the precedence level of its immediate context, a term
-- constructed from a binary operator of a specified precedence level.
-- 
-- A term is enclosed in parentheses if the precedence level of its operator 
-- is less than the precedence level of the enclosing context.

ppInfix :: Prec -> (String, Prec) -> Doc -> Doc -> Doc
ppInfix ctx (op, prec) l r = modifier $ l >#< ppOp >#< r
  where
    modifier = if prec < ctx then parens else id
    ppOp     = text op
{-# LINE 175 "AG.hs" #-}

{-# LINE 26 "AG\\Pos.ag" #-}

-- | Retrieves a textual description of a 'SourcePos'.
describeSourcePos :: SourcePos -> String
describeSourcePos (SourcePos (File file) (Pos ln col))
                                                 = file ++
                                                   ":line " ++ show ln ++
                                                   ":column " ++ show col
describeSourcePos (SourcePos (File file) EOF)    = file ++
                                                   ":<at end of file>"
describeSourcePos (SourcePos Stdin (Pos ln col)) = "line " ++ show ln ++
                                                   ":column " ++ show col
describeSourcePos (SourcePos Stdin EOF)          = "<at end of input>"
{-# LINE 190 "AG.hs" #-}

{-# LINE 16 "AG\\Evaluation.ag" #-}

-- | Type of values.
data Val
  = VNum Num_  -- ^ Numeral.
  | VFalse     -- ^ False.
  | VTrue      -- ^ True.

instance Tree Val where
  fromTree (VNum n) = App "Num"   [fromTree n]
  fromTree VFalse   = App "False" []
  fromTree VTrue    = App "True"  []

  toTree = parseTree [ app "Num"   (VNum <$> arg)
                     , app "False" (pure VFalse)
                     , app "True"  (pure VTrue)
                     ]

instance Printable Val where
  pp (VNum n) = showable n
  pp VFalse   = text "false"
  pp VTrue    = text "true"

-- | Retrieves whether a 'Val' is a 'VNum'.
isVNum :: Val -> Bool
isVNum (VNum _) = True
isVNum _        = False

-- | Retrieves whether a 'Val' is 'VFalse'.
isVFalse :: Val -> Bool
isVFalse VFalse = True
isVFalse _      = False

-- | Retrieves whether a 'Val' is 'VTrue'.
isVTrue :: Val -> Bool
isVTrue VTrue = True
isVTrue _     = False

-- | Retrieves whether a 'Val' is either 'VFalse' or 'VTrue'.
isBool :: Val -> Bool
isBool v = isVFalse v || isVTrue v

-- | Retrieves a textual description of the type of a 'Val'.
describeTy :: Val -> String
describeTy (VNum _) = "natural number"
describeTy VFalse   = "boolean"
describeTy VTrue    = "boolean"
{-# LINE 239 "AG.hs" #-}

{-# LINE 103 "AG\\Evaluation.ag" #-}

-- | Checks whether a 'Val' is a numeral.
-- If the check succeeds, the numeral is returned; if not, a run-time type
-- error is issued.
matchNat :: SourcePos -> Feedback Val -> Feedback Num_
matchNat pos fv = do
  v@(~(VNum n)) <- fv
  unless (isVNum v) (errTyMismatch pos "natural number" (describeTy v))
  return n

-- | Checks whether a 'Val' is a boolean constant.
-- If the check succeeds, one of two continuations (for 'VTrue' and 'VFalse',
-- respectively) is selected; if not, a run-time type error is issued.
matchBool :: SourcePos -> Feedback Val -> Feedback a -> Feedback a ->
             Feedback a
matchBool pos fv ft ff = do
  v <- fv
  unless (isBool v) (errTyMismatch pos "boolean" (describeTy v))
  case v of
    VFalse -> ff
    VTrue  -> ft
{-# LINE 263 "AG.hs" #-}

{-# LINE 130 "AG\\Evaluation.ag" #-}

-- | Produces a division-by-zero error.
errDivByZero :: SourcePos -> Feedback ()
errDivByZero pos =
  errorMessage . wrapped $
  describeSourcePos pos ++ ": Run-time error: division by zero."

-- | Given a source position, a textual description of the expected type, and
-- a textual description of the actual type, produces a run-time type error.
errTyMismatch :: SourcePos -> String -> String -> Feedback ()
errTyMismatch pos expected encountered
   = errorMessage (ppHeader >-< text " " >-< ppExpected >-< ppEncountered)
  where
    ppHeader      = wrapped $
                    describeSourcePos pos ++ ": Run-time error: type mismatch."
    ppExpected    = text "? expected    : " >|< wrapped expected
    ppEncountered = text "? encountered : " >|< wrapped encountered
{-# LINE 283 "AG.hs" #-}

{-# LINE 10 "AG\\Base.ag" #-}

-- | Type of numerals.
type Num_ = Int
{-# LINE 289 "AG.hs" #-}
-- Tm ----------------------------------------------------------
data Tm = Tm (SourcePos) (Tm_)
-- cata
sem_Tm :: Tm ->
          T_Tm
sem_Tm (Tm _pos _t) =
    (sem_Tm_Tm _pos (sem_Tm_ _t))
-- semantic domain
type T_Tm = Int ->
            ( SourcePos,Doc,Ty,([TyErr]),(Feedback Val))
data Inh_Tm = Inh_Tm {prec_Inh_Tm :: Int}
data Syn_Tm = Syn_Tm {pos_Syn_Tm :: SourcePos,pp_Syn_Tm :: Doc,ty_Syn_Tm :: Ty,tyErrs_Syn_Tm :: ([TyErr]),val_Syn_Tm :: (Feedback Val)}
wrap_Tm :: T_Tm ->
           Inh_Tm ->
           Syn_Tm
wrap_Tm sem (Inh_Tm _lhsIprec) =
    (let ( _lhsOpos,_lhsOpp,_lhsOty,_lhsOtyErrs,_lhsOval) = sem _lhsIprec
     in  (Syn_Tm _lhsOpos _lhsOpp _lhsOty _lhsOtyErrs _lhsOval))
sem_Tm_Tm :: SourcePos ->
             T_Tm_ ->
             T_Tm
sem_Tm_Tm pos_ t_ =
    (\ _lhsIprec ->
         (let _lhsOpos :: SourcePos
              _tOpos :: SourcePos
              _lhsOtyErrs :: ([TyErr])
              _lhsOpp :: Doc
              _lhsOty :: Ty
              _lhsOval :: (Feedback Val)
              _tOprec :: Int
              _tIpp :: Doc
              _tIty :: Ty
              _tItyErrs :: ([TyErr])
              _tIval :: (Feedback Val)
              _lhsOpos =
                  ({-# LINE 14 "AG\\Pos.ag" #-}
                   pos_
                   {-# LINE 327 "AG.hs" #-}
                   )
              _tOpos =
                  ({-# LINE 20 "AG\\Pos.ag" #-}
                   pos_
                   {-# LINE 332 "AG.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 47 "AG\\Typing.ag" #-}
                   _tItyErrs
                   {-# LINE 337 "AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "AG\\Printing.ag" #-}
                   _tIpp
                   {-# LINE 342 "AG.hs" #-}
                   )
              _lhsOty =
                  ({-# LINE 46 "AG\\Typing.ag" #-}
                   _tIty
                   {-# LINE 347 "AG.hs" #-}
                   )
              _lhsOval =
                  ({-# LINE 69 "AG\\Evaluation.ag" #-}
                   _tIval
                   {-# LINE 352 "AG.hs" #-}
                   )
              _tOprec =
                  ({-# LINE 63 "AG\\Printing.ag" #-}
                   _lhsIprec
                   {-# LINE 357 "AG.hs" #-}
                   )
              ( _tIpp,_tIty,_tItyErrs,_tIval) =
                  t_ _tOpos _tOprec
          in  ( _lhsOpos,_lhsOpp,_lhsOty,_lhsOtyErrs,_lhsOval)))
-- Tm_ ---------------------------------------------------------
data Tm_ = Num (Num_)
         | False_
         | True_
         | If (Tm) (Tm) (Tm)
         | Add (Tm) (Tm)
         | Sub (Tm) (Tm)
         | Mul (Tm) (Tm)
         | Div (Tm) (Tm)
         | Lt (Tm) (Tm)
         | Eq (Tm) (Tm)
         | Gt (Tm) (Tm)
-- cata
sem_Tm_ :: Tm_ ->
           T_Tm_
sem_Tm_ (Num _n) =
    (sem_Tm__Num _n)
sem_Tm_ (False_) =
    (sem_Tm__False_)
sem_Tm_ (True_) =
    (sem_Tm__True_)
sem_Tm_ (If _t1 _t2 _t3) =
    (sem_Tm__If (sem_Tm _t1) (sem_Tm _t2) (sem_Tm _t3))
sem_Tm_ (Add _t1 _t2) =
    (sem_Tm__Add (sem_Tm _t1) (sem_Tm _t2))
sem_Tm_ (Sub _t1 _t2) =
    (sem_Tm__Sub (sem_Tm _t1) (sem_Tm _t2))
sem_Tm_ (Mul _t1 _t2) =
    (sem_Tm__Mul (sem_Tm _t1) (sem_Tm _t2))
sem_Tm_ (Div _t1 _t2) =
    (sem_Tm__Div (sem_Tm _t1) (sem_Tm _t2))
sem_Tm_ (Lt _t1 _t2) =
    (sem_Tm__Lt (sem_Tm _t1) (sem_Tm _t2))
sem_Tm_ (Eq _t1 _t2) =
    (sem_Tm__Eq (sem_Tm _t1) (sem_Tm _t2))
sem_Tm_ (Gt _t1 _t2) =
    (sem_Tm__Gt (sem_Tm _t1) (sem_Tm _t2))
-- semantic domain
type T_Tm_ = SourcePos ->
             Int ->
             ( Doc,Ty,([TyErr]),(Feedback Val))
data Inh_Tm_ = Inh_Tm_ {pos_Inh_Tm_ :: SourcePos,prec_Inh_Tm_ :: Int}
data Syn_Tm_ = Syn_Tm_ {pp_Syn_Tm_ :: Doc,ty_Syn_Tm_ :: Ty,tyErrs_Syn_Tm_ :: ([TyErr]),val_Syn_Tm_ :: (Feedback Val)}
wrap_Tm_ :: T_Tm_ ->
            Inh_Tm_ ->
            Syn_Tm_
wrap_Tm_ sem (Inh_Tm_ _lhsIpos _lhsIprec) =
    (let ( _lhsOpp,_lhsOty,_lhsOtyErrs,_lhsOval) = sem _lhsIpos _lhsIprec
     in  (Syn_Tm_ _lhsOpp _lhsOty _lhsOtyErrs _lhsOval))
sem_Tm__Num :: Num_ ->
               T_Tm_
sem_Tm__Num n_ =
    (\ _lhsIpos
       _lhsIprec ->
         (let _lhsOty :: Ty
              _lhsOpp :: Doc
              _lhsOval :: (Feedback Val)
              _lhsOtyErrs :: ([TyErr])
              _lhsOty =
                  ({-# LINE 50 "AG\\Typing.ag" #-}
                   Nat
                   {-# LINE 423 "AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 14 "AG\\Printing.ag" #-}
                   showable n_
                   {-# LINE 428 "AG.hs" #-}
                   )
              _lhsOval =
                  ({-# LINE 72 "AG\\Evaluation.ag" #-}
                   return (VNum n_)
                   {-# LINE 433 "AG.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 47 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 438 "AG.hs" #-}
                   )
          in  ( _lhsOpp,_lhsOty,_lhsOtyErrs,_lhsOval)))
sem_Tm__False_ :: T_Tm_
sem_Tm__False_ =
    (\ _lhsIpos
       _lhsIprec ->
         (let _lhsOty :: Ty
              _lhsOpp :: Doc
              _lhsOval :: (Feedback Val)
              _lhsOtyErrs :: ([TyErr])
              _lhsOty =
                  ({-# LINE 51 "AG\\Typing.ag" #-}
                   Bool
                   {-# LINE 452 "AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 15 "AG\\Printing.ag" #-}
                   text "false"
                   {-# LINE 457 "AG.hs" #-}
                   )
              _lhsOval =
                  ({-# LINE 73 "AG\\Evaluation.ag" #-}
                   return VFalse
                   {-# LINE 462 "AG.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 47 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 467 "AG.hs" #-}
                   )
          in  ( _lhsOpp,_lhsOty,_lhsOtyErrs,_lhsOval)))
sem_Tm__True_ :: T_Tm_
sem_Tm__True_ =
    (\ _lhsIpos
       _lhsIprec ->
         (let _lhsOty :: Ty
              _lhsOpp :: Doc
              _lhsOval :: (Feedback Val)
              _lhsOtyErrs :: ([TyErr])
              _lhsOty =
                  ({-# LINE 51 "AG\\Typing.ag" #-}
                   Bool
                   {-# LINE 481 "AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 16 "AG\\Printing.ag" #-}
                   text "true"
                   {-# LINE 486 "AG.hs" #-}
                   )
              _lhsOval =
                  ({-# LINE 74 "AG\\Evaluation.ag" #-}
                   return VTrue
                   {-# LINE 491 "AG.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 47 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 496 "AG.hs" #-}
                   )
          in  ( _lhsOpp,_lhsOty,_lhsOtyErrs,_lhsOval)))
sem_Tm__If :: T_Tm ->
              T_Tm ->
              T_Tm ->
              T_Tm_
sem_Tm__If t1_ t2_ t3_ =
    (\ _lhsIpos
       _lhsIprec ->
         (let _lhsOty :: Ty
              _lhsOtyErrs :: ([TyErr])
              _lhsOpp :: Doc
              _t1Oprec :: Int
              _t2Oprec :: Int
              _t3Oprec :: Int
              _lhsOval :: (Feedback Val)
              _t1Ipos :: SourcePos
              _t1Ipp :: Doc
              _t1Ity :: Ty
              _t1ItyErrs :: ([TyErr])
              _t1Ival :: (Feedback Val)
              _t2Ipos :: SourcePos
              _t2Ipp :: Doc
              _t2Ity :: Ty
              _t2ItyErrs :: ([TyErr])
              _t2Ival :: (Feedback Val)
              _t3Ipos :: SourcePos
              _t3Ipp :: Doc
              _t3Ity :: Ty
              _t3ItyErrs :: ([TyErr])
              _t3Ival :: (Feedback Val)
              _lhsOty =
                  ({-# LINE 52 "AG\\Typing.ag" #-}
                   _t2Ity `lub` _t3Ity
                   {-# LINE 531 "AG.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 57 "AG\\Typing.ag" #-}
                   _t1ItyErrs ++ _t2ItyErrs ++ _t3ItyErrs ++
                   checkTyGuard _t1Ipos _t1Ity ++
                   checkTyBranches _t3Ipos _t2Ity _t3Ity
                   {-# LINE 538 "AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 19 "AG\\Printing.ag" #-}
                   ppIf _t1Ipp _t2Ipp _t3Ipp
                   {-# LINE 543 "AG.hs" #-}
                   )
              _t1Oprec =
                  ({-# LINE 66 "AG\\Printing.ag" #-}
                   0
                   {-# LINE 548 "AG.hs" #-}
                   )
              _t2Oprec =
                  ({-# LINE 67 "AG\\Printing.ag" #-}
                   0
                   {-# LINE 553 "AG.hs" #-}
                   )
              _t3Oprec =
                  ({-# LINE 68 "AG\\Printing.ag" #-}
                   0
                   {-# LINE 558 "AG.hs" #-}
                   )
              _lhsOval =
                  ({-# LINE 75 "AG\\Evaluation.ag" #-}
                   matchBool _t1Ipos _t1Ival _t2Ival _t3Ival
                   {-# LINE 563 "AG.hs" #-}
                   )
              ( _t1Ipos,_t1Ipp,_t1Ity,_t1ItyErrs,_t1Ival) =
                  t1_ _t1Oprec
              ( _t2Ipos,_t2Ipp,_t2Ity,_t2ItyErrs,_t2Ival) =
                  t2_ _t2Oprec
              ( _t3Ipos,_t3Ipp,_t3Ity,_t3ItyErrs,_t3Ival) =
                  t3_ _t3Oprec
          in  ( _lhsOpp,_lhsOty,_lhsOtyErrs,_lhsOval)))
sem_Tm__Add :: T_Tm ->
               T_Tm ->
               T_Tm_
sem_Tm__Add t1_ t2_ =
    (\ _lhsIpos
       _lhsIprec ->
         (let _lhsOty :: Ty
              _lhsOtyErrs :: ([TyErr])
              _lhsOpp :: Doc
              _t1Oprec :: Int
              _t2Oprec :: Int
              _lhsOval :: (Feedback Val)
              _t1Ipos :: SourcePos
              _t1Ipp :: Doc
              _t1Ity :: Ty
              _t1ItyErrs :: ([TyErr])
              _t1Ival :: (Feedback Val)
              _t2Ipos :: SourcePos
              _t2Ipp :: Doc
              _t2Ity :: Ty
              _t2ItyErrs :: ([TyErr])
              _t2Ival :: (Feedback Val)
              _lhsOty =
                  ({-# LINE 53 "AG\\Typing.ag" #-}
                   Nat
                   {-# LINE 597 "AG.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 79 "AG\\Typing.ag" #-}
                   _t1ItyErrs ++ _t2ItyErrs ++
                   checkTyArithOp _t1Ipos _t1Ity ++
                   checkTyArithOp _t2Ipos _t2Ity
                   {-# LINE 604 "AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 45 "AG\\Printing.ag" #-}
                   ppInfix _lhsIprec ("+" , 6) _t1Ipp _t2Ipp
                   {-# LINE 609 "AG.hs" #-}
                   )
              _t1Oprec =
                  ({-# LINE 69 "AG\\Printing.ag" #-}
                   6
                   {-# LINE 614 "AG.hs" #-}
                   )
              _t2Oprec =
                  ({-# LINE 70 "AG\\Printing.ag" #-}
                   7
                   {-# LINE 619 "AG.hs" #-}
                   )
              _lhsOval =
                  ({-# LINE 76 "AG\\Evaluation.ag" #-}
                   do n1 <- matchNat _t1Ipos _t1Ival
                      n2 <- matchNat _t2Ipos _t2Ival
                      return (VNum (n1 + n2))
                   {-# LINE 626 "AG.hs" #-}
                   )
              ( _t1Ipos,_t1Ipp,_t1Ity,_t1ItyErrs,_t1Ival) =
                  t1_ _t1Oprec
              ( _t2Ipos,_t2Ipp,_t2Ity,_t2ItyErrs,_t2Ival) =
                  t2_ _t2Oprec
          in  ( _lhsOpp,_lhsOty,_lhsOtyErrs,_lhsOval)))
sem_Tm__Sub :: T_Tm ->
               T_Tm ->
               T_Tm_
sem_Tm__Sub t1_ t2_ =
    (\ _lhsIpos
       _lhsIprec ->
         (let _lhsOty :: Ty
              _lhsOtyErrs :: ([TyErr])
              _lhsOpp :: Doc
              _t1Oprec :: Int
              _t2Oprec :: Int
              _lhsOval :: (Feedback Val)
              _t1Ipos :: SourcePos
              _t1Ipp :: Doc
              _t1Ity :: Ty
              _t1ItyErrs :: ([TyErr])
              _t1Ival :: (Feedback Val)
              _t2Ipos :: SourcePos
              _t2Ipp :: Doc
              _t2Ity :: Ty
              _t2ItyErrs :: ([TyErr])
              _t2Ival :: (Feedback Val)
              _lhsOty =
                  ({-# LINE 53 "AG\\Typing.ag" #-}
                   Nat
                   {-# LINE 658 "AG.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 79 "AG\\Typing.ag" #-}
                   _t1ItyErrs ++ _t2ItyErrs ++
                   checkTyArithOp _t1Ipos _t1Ity ++
                   checkTyArithOp _t2Ipos _t2Ity
                   {-# LINE 665 "AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 46 "AG\\Printing.ag" #-}
                   ppInfix _lhsIprec ("-" , 6) _t1Ipp _t2Ipp
                   {-# LINE 670 "AG.hs" #-}
                   )
              _t1Oprec =
                  ({-# LINE 69 "AG\\Printing.ag" #-}
                   6
                   {-# LINE 675 "AG.hs" #-}
                   )
              _t2Oprec =
                  ({-# LINE 70 "AG\\Printing.ag" #-}
                   7
                   {-# LINE 680 "AG.hs" #-}
                   )
              _lhsOval =
                  ({-# LINE 79 "AG\\Evaluation.ag" #-}
                   do n1 <- matchNat _t1Ipos _t1Ival
                      n2 <- matchNat _t2Ipos _t2Ival
                      return (VNum (if n2 > n1 then 0 else n1 - n2))
                   {-# LINE 687 "AG.hs" #-}
                   )
              ( _t1Ipos,_t1Ipp,_t1Ity,_t1ItyErrs,_t1Ival) =
                  t1_ _t1Oprec
              ( _t2Ipos,_t2Ipp,_t2Ity,_t2ItyErrs,_t2Ival) =
                  t2_ _t2Oprec
          in  ( _lhsOpp,_lhsOty,_lhsOtyErrs,_lhsOval)))
sem_Tm__Mul :: T_Tm ->
               T_Tm ->
               T_Tm_
sem_Tm__Mul t1_ t2_ =
    (\ _lhsIpos
       _lhsIprec ->
         (let _lhsOty :: Ty
              _lhsOtyErrs :: ([TyErr])
              _lhsOpp :: Doc
              _t1Oprec :: Int
              _t2Oprec :: Int
              _lhsOval :: (Feedback Val)
              _t1Ipos :: SourcePos
              _t1Ipp :: Doc
              _t1Ity :: Ty
              _t1ItyErrs :: ([TyErr])
              _t1Ival :: (Feedback Val)
              _t2Ipos :: SourcePos
              _t2Ipp :: Doc
              _t2Ity :: Ty
              _t2ItyErrs :: ([TyErr])
              _t2Ival :: (Feedback Val)
              _lhsOty =
                  ({-# LINE 53 "AG\\Typing.ag" #-}
                   Nat
                   {-# LINE 719 "AG.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 79 "AG\\Typing.ag" #-}
                   _t1ItyErrs ++ _t2ItyErrs ++
                   checkTyArithOp _t1Ipos _t1Ity ++
                   checkTyArithOp _t2Ipos _t2Ity
                   {-# LINE 726 "AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 47 "AG\\Printing.ag" #-}
                   ppInfix _lhsIprec ("*" , 7) _t1Ipp _t2Ipp
                   {-# LINE 731 "AG.hs" #-}
                   )
              _t1Oprec =
                  ({-# LINE 71 "AG\\Printing.ag" #-}
                   7
                   {-# LINE 736 "AG.hs" #-}
                   )
              _t2Oprec =
                  ({-# LINE 72 "AG\\Printing.ag" #-}
                   8
                   {-# LINE 741 "AG.hs" #-}
                   )
              _lhsOval =
                  ({-# LINE 82 "AG\\Evaluation.ag" #-}
                   do n1 <- matchNat _t1Ipos _t1Ival
                      n2 <- matchNat _t2Ipos _t2Ival
                      return (VNum (n1 * n2))
                   {-# LINE 748 "AG.hs" #-}
                   )
              ( _t1Ipos,_t1Ipp,_t1Ity,_t1ItyErrs,_t1Ival) =
                  t1_ _t1Oprec
              ( _t2Ipos,_t2Ipp,_t2Ity,_t2ItyErrs,_t2Ival) =
                  t2_ _t2Oprec
          in  ( _lhsOpp,_lhsOty,_lhsOtyErrs,_lhsOval)))
sem_Tm__Div :: T_Tm ->
               T_Tm ->
               T_Tm_
sem_Tm__Div t1_ t2_ =
    (\ _lhsIpos
       _lhsIprec ->
         (let _lhsOty :: Ty
              _lhsOtyErrs :: ([TyErr])
              _lhsOpp :: Doc
              _t1Oprec :: Int
              _t2Oprec :: Int
              _lhsOval :: (Feedback Val)
              _t1Ipos :: SourcePos
              _t1Ipp :: Doc
              _t1Ity :: Ty
              _t1ItyErrs :: ([TyErr])
              _t1Ival :: (Feedback Val)
              _t2Ipos :: SourcePos
              _t2Ipp :: Doc
              _t2Ity :: Ty
              _t2ItyErrs :: ([TyErr])
              _t2Ival :: (Feedback Val)
              _lhsOty =
                  ({-# LINE 53 "AG\\Typing.ag" #-}
                   Nat
                   {-# LINE 780 "AG.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 79 "AG\\Typing.ag" #-}
                   _t1ItyErrs ++ _t2ItyErrs ++
                   checkTyArithOp _t1Ipos _t1Ity ++
                   checkTyArithOp _t2Ipos _t2Ity
                   {-# LINE 787 "AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 48 "AG\\Printing.ag" #-}
                   ppInfix _lhsIprec ("/" , 7) _t1Ipp _t2Ipp
                   {-# LINE 792 "AG.hs" #-}
                   )
              _t1Oprec =
                  ({-# LINE 71 "AG\\Printing.ag" #-}
                   7
                   {-# LINE 797 "AG.hs" #-}
                   )
              _t2Oprec =
                  ({-# LINE 72 "AG\\Printing.ag" #-}
                   8
                   {-# LINE 802 "AG.hs" #-}
                   )
              _lhsOval =
                  ({-# LINE 85 "AG\\Evaluation.ag" #-}
                   do n1 <- matchNat _t1Ipos _t1Ival
                      n2 <- matchNat _t2Ipos _t2Ival
                      when (n2 == 0) (errDivByZero _lhsIpos)
                      return (VNum (n1 `div` n2))
                   {-# LINE 810 "AG.hs" #-}
                   )
              ( _t1Ipos,_t1Ipp,_t1Ity,_t1ItyErrs,_t1Ival) =
                  t1_ _t1Oprec
              ( _t2Ipos,_t2Ipp,_t2Ity,_t2ItyErrs,_t2Ival) =
                  t2_ _t2Oprec
          in  ( _lhsOpp,_lhsOty,_lhsOtyErrs,_lhsOval)))
sem_Tm__Lt :: T_Tm ->
              T_Tm ->
              T_Tm_
sem_Tm__Lt t1_ t2_ =
    (\ _lhsIpos
       _lhsIprec ->
         (let _lhsOty :: Ty
              _lhsOtyErrs :: ([TyErr])
              _lhsOpp :: Doc
              _t1Oprec :: Int
              _t2Oprec :: Int
              _lhsOval :: (Feedback Val)
              _t1Ipos :: SourcePos
              _t1Ipp :: Doc
              _t1Ity :: Ty
              _t1ItyErrs :: ([TyErr])
              _t1Ival :: (Feedback Val)
              _t2Ipos :: SourcePos
              _t2Ipp :: Doc
              _t2Ity :: Ty
              _t2ItyErrs :: ([TyErr])
              _t2Ival :: (Feedback Val)
              _lhsOty =
                  ({-# LINE 54 "AG\\Typing.ag" #-}
                   Bool
                   {-# LINE 842 "AG.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 93 "AG\\Typing.ag" #-}
                   _t1ItyErrs ++ _t2ItyErrs ++
                   checkTyRelOp _t1Ipos _t1Ity ++
                   checkTyRelOp _t2Ipos _t2Ity
                   {-# LINE 849 "AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 49 "AG\\Printing.ag" #-}
                   ppInfix _lhsIprec ("<" , 4) _t1Ipp _t2Ipp
                   {-# LINE 854 "AG.hs" #-}
                   )
              _t1Oprec =
                  ({-# LINE 73 "AG\\Printing.ag" #-}
                   4
                   {-# LINE 859 "AG.hs" #-}
                   )
              _t2Oprec =
                  ({-# LINE 74 "AG\\Printing.ag" #-}
                   4
                   {-# LINE 864 "AG.hs" #-}
                   )
              _lhsOval =
                  ({-# LINE 89 "AG\\Evaluation.ag" #-}
                   do n1 <- matchNat _t1Ipos _t1Ival
                      n2 <- matchNat _t2Ipos _t2Ival
                      return (if n1 < n2 then VTrue else VFalse)
                   {-# LINE 871 "AG.hs" #-}
                   )
              ( _t1Ipos,_t1Ipp,_t1Ity,_t1ItyErrs,_t1Ival) =
                  t1_ _t1Oprec
              ( _t2Ipos,_t2Ipp,_t2Ity,_t2ItyErrs,_t2Ival) =
                  t2_ _t2Oprec
          in  ( _lhsOpp,_lhsOty,_lhsOtyErrs,_lhsOval)))
sem_Tm__Eq :: T_Tm ->
              T_Tm ->
              T_Tm_
sem_Tm__Eq t1_ t2_ =
    (\ _lhsIpos
       _lhsIprec ->
         (let _lhsOty :: Ty
              _lhsOtyErrs :: ([TyErr])
              _lhsOpp :: Doc
              _t1Oprec :: Int
              _t2Oprec :: Int
              _lhsOval :: (Feedback Val)
              _t1Ipos :: SourcePos
              _t1Ipp :: Doc
              _t1Ity :: Ty
              _t1ItyErrs :: ([TyErr])
              _t1Ival :: (Feedback Val)
              _t2Ipos :: SourcePos
              _t2Ipp :: Doc
              _t2Ity :: Ty
              _t2ItyErrs :: ([TyErr])
              _t2Ival :: (Feedback Val)
              _lhsOty =
                  ({-# LINE 54 "AG\\Typing.ag" #-}
                   Bool
                   {-# LINE 903 "AG.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 93 "AG\\Typing.ag" #-}
                   _t1ItyErrs ++ _t2ItyErrs ++
                   checkTyRelOp _t1Ipos _t1Ity ++
                   checkTyRelOp _t2Ipos _t2Ity
                   {-# LINE 910 "AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 50 "AG\\Printing.ag" #-}
                   ppInfix _lhsIprec ("==", 4) _t1Ipp _t2Ipp
                   {-# LINE 915 "AG.hs" #-}
                   )
              _t1Oprec =
                  ({-# LINE 73 "AG\\Printing.ag" #-}
                   4
                   {-# LINE 920 "AG.hs" #-}
                   )
              _t2Oprec =
                  ({-# LINE 74 "AG\\Printing.ag" #-}
                   4
                   {-# LINE 925 "AG.hs" #-}
                   )
              _lhsOval =
                  ({-# LINE 92 "AG\\Evaluation.ag" #-}
                   do n1 <- matchNat _t1Ipos _t1Ival
                      n2 <- matchNat _t2Ipos _t2Ival
                      return (if n1 == n2 then VTrue else VFalse)
                   {-# LINE 932 "AG.hs" #-}
                   )
              ( _t1Ipos,_t1Ipp,_t1Ity,_t1ItyErrs,_t1Ival) =
                  t1_ _t1Oprec
              ( _t2Ipos,_t2Ipp,_t2Ity,_t2ItyErrs,_t2Ival) =
                  t2_ _t2Oprec
          in  ( _lhsOpp,_lhsOty,_lhsOtyErrs,_lhsOval)))
sem_Tm__Gt :: T_Tm ->
              T_Tm ->
              T_Tm_
sem_Tm__Gt t1_ t2_ =
    (\ _lhsIpos
       _lhsIprec ->
         (let _lhsOty :: Ty
              _lhsOtyErrs :: ([TyErr])
              _lhsOpp :: Doc
              _t1Oprec :: Int
              _t2Oprec :: Int
              _lhsOval :: (Feedback Val)
              _t1Ipos :: SourcePos
              _t1Ipp :: Doc
              _t1Ity :: Ty
              _t1ItyErrs :: ([TyErr])
              _t1Ival :: (Feedback Val)
              _t2Ipos :: SourcePos
              _t2Ipp :: Doc
              _t2Ity :: Ty
              _t2ItyErrs :: ([TyErr])
              _t2Ival :: (Feedback Val)
              _lhsOty =
                  ({-# LINE 54 "AG\\Typing.ag" #-}
                   Bool
                   {-# LINE 964 "AG.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 93 "AG\\Typing.ag" #-}
                   _t1ItyErrs ++ _t2ItyErrs ++
                   checkTyRelOp _t1Ipos _t1Ity ++
                   checkTyRelOp _t2Ipos _t2Ity
                   {-# LINE 971 "AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 51 "AG\\Printing.ag" #-}
                   ppInfix _lhsIprec (">" , 4) _t1Ipp _t2Ipp
                   {-# LINE 976 "AG.hs" #-}
                   )
              _t1Oprec =
                  ({-# LINE 73 "AG\\Printing.ag" #-}
                   4
                   {-# LINE 981 "AG.hs" #-}
                   )
              _t2Oprec =
                  ({-# LINE 74 "AG\\Printing.ag" #-}
                   4
                   {-# LINE 986 "AG.hs" #-}
                   )
              _lhsOval =
                  ({-# LINE 95 "AG\\Evaluation.ag" #-}
                   do n1 <- matchNat _t1Ipos _t1Ival
                      n2 <- matchNat _t2Ipos _t2Ival
                      return (if n1 > n2 then VTrue else VFalse)
                   {-# LINE 993 "AG.hs" #-}
                   )
              ( _t1Ipos,_t1Ipp,_t1Ity,_t1ItyErrs,_t1Ival) =
                  t1_ _t1Oprec
              ( _t2Ipos,_t2Ipp,_t2Ity,_t2ItyErrs,_t2Ival) =
                  t2_ _t2Oprec
          in  ( _lhsOpp,_lhsOty,_lhsOtyErrs,_lhsOval)))