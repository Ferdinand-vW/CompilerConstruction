

-- UUAGC 0.9.52.1 (Ag.ag)
module CCO.Diag.AG where

{-# LINE 2 "AG\\Typing.ag" #-}

import CCO.Feedback
import CCO.Printing
import CCO.SourcePos        (SourcePos)
import CCO.Tree             (ATerm (App), Tree (fromTree, toTree))
import CCO.Tree.Parser      (parseTree, app)
import Control.Applicative  (Applicative (pure))
{-# LINE 14 "Ag.hs" #-}

{-# LINE 2 "AG\\Pos.ag" #-}

import CCO.SourcePos
{-# LINE 19 "Ag.hs" #-}

{-# LINE 2 "AG\\Base.ag" #-}

import CCO.SourcePos        (SourcePos)
import CCO.Tree             (ATerm (App), Tree (fromTree, toTree))
import CCO.Tree.Parser      (parseTree, app, arg)
import Control.Applicative  (Applicative ((<*>)), (<$>))
{-# LINE 27 "Ag.hs" #-}
{-# LINE 13 "AG\\Typing.ag" #-}

data Ty = Prog | Interp | Comp | PlatF | Runnable | Framework | Top deriving (Eq, Show)

instance Tree Ty where
    fromTree Prog = App "Prog" []
    fromTree Interp = App "Interp" []
    fromTree Comp = App "Comp" []
    fromTree PlatF = App "PlatF" []
    fromTree Runnable = App "Runnable" []
    fromTree Framework = App "Framework" []
    fromTree Top = App "Top" []

    toTree = parseTree [ app "Prog" (pure Prog),
                         app "Interp" (pure Interp),
                         app "Comp" (pure Comp),
                         app "PlatF" (pure PlatF),
                         app "Runnable" (pure Runnable),
                         app "Framework" (pure Framework),
                         app "Top" (pure Top)]



match :: Ty -> Ty -> Bool
match _ Top = True
match Top _ = True
match Runnable Prog = True
match Runnable Interp = True
match Runnable Comp = True
match Prog Runnable = True
match Interp Runnable = True
match Comp Runnable = True
match Framework Interp = True
match Framework PlatF = True
match Interp Framework = True
match PlatF Framework = True
match ty1 ty2 = ty1 == ty2

typeOfExecute :: Ty -> Ty -> Ty
typeOfExecute ty1 ty2
    | not (match ty1 PlatF) && (match ty2 PlatF || match ty2 Interp) = ty1
    | otherwise = Top

typeOfCompile :: Ty -> Ty -> Ty
typeOfCompile ty1 ty2
    | not (match ty1 PlatF) && (match ty2 Comp) = ty1
    | otherwise = Top

{-# LINE 76 "Ag.hs" #-}

{-# LINE 83 "AG\\Typing.ag" #-}

checkRunnable :: SourcePos -> Ty -> [TyErr]
checkRunnable _ ty | match ty Runnable = []
checkRunnable pos ty                   = [TyErr pos nonExe Runnable ty]
    where
        nonExe = "Cannot execute a non-runnable"

checkFramework :: SourcePos -> Ty -> [TyErr]
checkFramework _ ty | match ty Framework = []
checkFramework pos ty                    = [TyErr pos nonFrame Framework ty]
    where
        nonFrame = "Cannot execute on non-Framework"
{-# LINE 91 "Ag.hs" #-}

{-# LINE 102 "AG\\Typing.ag" #-}

checkComp :: SourcePos -> Ty -> [TyErr]
checkComp _ ty | match ty Comp = []
checkComp pos ty                   = [TyErr pos nonComp Comp ty]
    where
        nonComp = "Must be compiled using a compiler"
{-# LINE 100 "Ag.hs" #-}

{-# LINE 113 "AG\\Typing.ag" #-}

data TyErr = TyErr SourcePos String Ty Ty

instance Printable TyErr where
    pp = ppTyErr
{-# LINE 108 "Ag.hs" #-}

{-# LINE 121 "AG\\Typing.ag" #-}

-- | Pretty prints a type error message.
ppTyErr :: TyErr -> Doc
ppTyErr (TyErr pos descr expected inferred)
  = above [ppHeader, text " ", ppExpected, ppInferred]
  where
    ppHeader   = wrapped $
                 describeSourcePos pos ++ ": Type error: " ++ descr ++ "."
    ppExpected = text "? expected : " >|< showable expected
    ppInferred = text "? inferred : " >|< showable inferred
{-# LINE 121 "Ag.hs" #-}

{-# LINE 22 "AG\\Pos.ag" #-}

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
{-# LINE 136 "Ag.hs" #-}

{-# LINE 13 "AG\\Base.ag" #-}

type Ident = String
{-# LINE 141 "Ag.hs" #-}

{-# LINE 32 "AG\\Base.ag" #-}

instance Tree Diag where
  fromTree (Diag pos d) = App "Diag" [fromTree pos, fromTree d]
  toTree = parseTree [app "Diag" (Diag <$> arg <*> arg)]

instance Tree Diag_ where
  fromTree (Program p l)        = App "Program"  [fromTree p, fromTree l]
  fromTree (Platform m)         = App "Platform" [fromTree m]
  fromTree (Interpreter i l m)  = App "Interpreter"
                                    [fromTree i, fromTree l, fromTree m]
  fromTree (Compiler c l1 l2 m) =
    App "Compiler" [fromTree c, fromTree l1, fromTree l2, fromTree m]
  fromTree (Execute d1 d2)      = App "Execute" [fromTree d1, fromTree d2]
  fromTree (Compile d1 d2)      = App "Compile" [fromTree d1, fromTree d2]

  toTree = parseTree 
             [ app "Program"     (Program     <$> arg <*> arg                )
             , app "Platform"    (Platform    <$> arg                        )
             , app "Interpreter" (Interpreter <$> arg <*> arg <*> arg        )
             , app "Compiler"    (Compiler    <$> arg <*> arg <*> arg <*> arg)
             , app "Execute"     (Execute     <$> arg <*> arg                )
             , app "Compile"     (Compile     <$> arg <*> arg                )
             ]
{-# LINE 167 "Ag.hs" #-}
-- Diag --------------------------------------------------------
data Diag = Diag (SourcePos) (Diag_)
-- cata
sem_Diag :: Diag ->
            T_Diag
sem_Diag (Diag _pos _d) =
    (sem_Diag_Diag _pos (sem_Diag_ _d))
-- semantic domain
type T_Diag = ( SourcePos,Ty,([TyErr]))
data Inh_Diag = Inh_Diag {}
data Syn_Diag = Syn_Diag {pos_Syn_Diag :: SourcePos,ty_Syn_Diag :: Ty,tyErrs_Syn_Diag :: ([TyErr])}
wrap_Diag :: T_Diag ->
             Inh_Diag ->
             Syn_Diag
wrap_Diag sem (Inh_Diag) =
    (let ( _lhsOpos,_lhsOty,_lhsOtyErrs) = sem
     in  (Syn_Diag _lhsOpos _lhsOty _lhsOtyErrs))
sem_Diag_Diag :: SourcePos ->
                 T_Diag_ ->
                 T_Diag
sem_Diag_Diag pos_ d_ =
    (let _lhsOpos :: SourcePos
         _dOpos :: SourcePos
         _lhsOtyErrs :: ([TyErr])
         _lhsOty :: Ty
         _dIty :: Ty
         _dItyErrs :: ([TyErr])
         _lhsOpos =
             ({-# LINE 14 "AG\\Pos.ag" #-}
              pos_
              {-# LINE 198 "Ag.hs" #-}
              )
         _dOpos =
             ({-# LINE 20 "AG\\Pos.ag" #-}
              pos_
              {-# LINE 203 "Ag.hs" #-}
              )
         _lhsOtyErrs =
             ({-# LINE 68 "AG\\Typing.ag" #-}
              _dItyErrs
              {-# LINE 208 "Ag.hs" #-}
              )
         _lhsOty =
             ({-# LINE 67 "AG\\Typing.ag" #-}
              _dIty
              {-# LINE 213 "Ag.hs" #-}
              )
         ( _dIty,_dItyErrs) =
             d_ _dOpos
     in  ( _lhsOpos,_lhsOty,_lhsOtyErrs))
-- Diag_ -------------------------------------------------------
data Diag_ = Program (Ident) (Ident)
           | Platform (Ident)
           | Interpreter (Ident) (Ident) (Ident)
           | Compiler (Ident) (Ident) (Ident) (Ident)
           | Execute (Diag) (Diag)
           | Compile (Diag) (Diag)
-- cata
sem_Diag_ :: Diag_ ->
             T_Diag_
sem_Diag_ (Program _p _l) =
    (sem_Diag__Program _p _l)
sem_Diag_ (Platform _m) =
    (sem_Diag__Platform _m)
sem_Diag_ (Interpreter _i _l _m) =
    (sem_Diag__Interpreter _i _l _m)
sem_Diag_ (Compiler _c _l1 _l2 _m) =
    (sem_Diag__Compiler _c _l1 _l2 _m)
sem_Diag_ (Execute _d1 _d2) =
    (sem_Diag__Execute (sem_Diag _d1) (sem_Diag _d2))
sem_Diag_ (Compile _d1 _d2) =
    (sem_Diag__Compile (sem_Diag _d1) (sem_Diag _d2))
-- semantic domain
type T_Diag_ = SourcePos ->
               ( Ty,([TyErr]))
data Inh_Diag_ = Inh_Diag_ {pos_Inh_Diag_ :: SourcePos}
data Syn_Diag_ = Syn_Diag_ {ty_Syn_Diag_ :: Ty,tyErrs_Syn_Diag_ :: ([TyErr])}
wrap_Diag_ :: T_Diag_ ->
              Inh_Diag_ ->
              Syn_Diag_
wrap_Diag_ sem (Inh_Diag_ _lhsIpos) =
    (let ( _lhsOty,_lhsOtyErrs) = sem _lhsIpos
     in  (Syn_Diag_ _lhsOty _lhsOtyErrs))
sem_Diag__Program :: Ident ->
                     Ident ->
                     T_Diag_
sem_Diag__Program p_ l_ =
    (\ _lhsIpos ->
         (let _lhsOty :: Ty
              _lhsOtyErrs :: ([TyErr])
              _lhsOty =
                  ({-# LINE 71 "AG\\Typing.ag" #-}
                   Prog
                   {-# LINE 261 "Ag.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 68 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 266 "Ag.hs" #-}
                   )
          in  ( _lhsOty,_lhsOtyErrs)))
sem_Diag__Platform :: Ident ->
                      T_Diag_
sem_Diag__Platform m_ =
    (\ _lhsIpos ->
         (let _lhsOty :: Ty
              _lhsOtyErrs :: ([TyErr])
              _lhsOty =
                  ({-# LINE 74 "AG\\Typing.ag" #-}
                   PlatF
                   {-# LINE 278 "Ag.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 68 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 283 "Ag.hs" #-}
                   )
          in  ( _lhsOty,_lhsOtyErrs)))
sem_Diag__Interpreter :: Ident ->
                         Ident ->
                         Ident ->
                         T_Diag_
sem_Diag__Interpreter i_ l_ m_ =
    (\ _lhsIpos ->
         (let _lhsOty :: Ty
              _lhsOtyErrs :: ([TyErr])
              _lhsOty =
                  ({-# LINE 72 "AG\\Typing.ag" #-}
                   Interp
                   {-# LINE 297 "Ag.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 68 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 302 "Ag.hs" #-}
                   )
          in  ( _lhsOty,_lhsOtyErrs)))
sem_Diag__Compiler :: Ident ->
                      Ident ->
                      Ident ->
                      Ident ->
                      T_Diag_
sem_Diag__Compiler c_ l1_ l2_ m_ =
    (\ _lhsIpos ->
         (let _lhsOty :: Ty
              _lhsOtyErrs :: ([TyErr])
              _lhsOty =
                  ({-# LINE 73 "AG\\Typing.ag" #-}
                   Comp
                   {-# LINE 317 "Ag.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 68 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 322 "Ag.hs" #-}
                   )
          in  ( _lhsOty,_lhsOtyErrs)))
sem_Diag__Execute :: T_Diag ->
                     T_Diag ->
                     T_Diag_
sem_Diag__Execute d1_ d2_ =
    (\ _lhsIpos ->
         (let _lhsOty :: Ty
              _lhsOtyErrs :: ([TyErr])
              _d1Ipos :: SourcePos
              _d1Ity :: Ty
              _d1ItyErrs :: ([TyErr])
              _d2Ipos :: SourcePos
              _d2Ity :: Ty
              _d2ItyErrs :: ([TyErr])
              _lhsOty =
                  ({-# LINE 75 "AG\\Typing.ag" #-}
                   typeOfExecute _d1Ity _d2Ity
                   {-# LINE 341 "Ag.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 79 "AG\\Typing.ag" #-}
                   _d1ItyErrs ++ _d2ItyErrs ++
                   checkRunnable _d1Ipos _d1Ity ++
                   checkFramework _d2Ipos _d2Ity
                   {-# LINE 348 "Ag.hs" #-}
                   )
              ( _d1Ipos,_d1Ity,_d1ItyErrs) =
                  d1_
              ( _d2Ipos,_d2Ity,_d2ItyErrs) =
                  d2_
          in  ( _lhsOty,_lhsOtyErrs)))
sem_Diag__Compile :: T_Diag ->
                     T_Diag ->
                     T_Diag_
sem_Diag__Compile d1_ d2_ =
    (\ _lhsIpos ->
         (let _lhsOty :: Ty
              _lhsOtyErrs :: ([TyErr])
              _d1Ipos :: SourcePos
              _d1Ity :: Ty
              _d1ItyErrs :: ([TyErr])
              _d2Ipos :: SourcePos
              _d2Ity :: Ty
              _d2ItyErrs :: ([TyErr])
              _lhsOty =
                  ({-# LINE 76 "AG\\Typing.ag" #-}
                   typeOfCompile _d1Ity _d2Ity
                   {-# LINE 371 "Ag.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 98 "AG\\Typing.ag" #-}
                   _d1ItyErrs ++ _d2ItyErrs ++
                   checkRunnable _d1Ipos _d1Ity ++
                   checkComp _d2Ipos _d2Ity
                   {-# LINE 378 "Ag.hs" #-}
                   )
              ( _d1Ipos,_d1Ity,_d1ItyErrs) =
                  d1_
              ( _d2Ipos,_d2Ity,_d2ItyErrs) =
                  d2_
          in  ( _lhsOty,_lhsOtyErrs)))


checkTy :: Diag -> Feedback Ty
checkTy t = do let syn = wrap_Diag (sem_Diag t) Inh_Diag
               messages [Error (pp tyErr) | tyErr <- tyErrs_Syn_Diag syn]
               return (ty_Syn_Diag syn)

