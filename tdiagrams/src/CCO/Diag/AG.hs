

-- UUAGC 0.9.52.1 (ag.ag)
module CCO.Diag.AG where

{-# LINE 2 "AG\\Pos.ag" #-}

import CCO.SourcePos
{-# LINE 10 "ag.hs" #-}

{-# LINE 2 "AG\\Base.ag" #-}

import CCO.Feedback
import CCO.Printing
import CCO.SourcePos        (SourcePos)
import CCO.Tree             (ATerm (App), Tree (fromTree, toTree))
import CCO.Tree.Parser      (parseTree, app, arg)
import Control.Applicative  (Applicative ((<*>)), (<$>))
{-# LINE 20 "ag.hs" #-}
{-# LINE 3 "AG\\Typing.ag" #-}

data Ty = Prog
        | Interp
        | Comp
        | PlatF
        | Runnable
        | Framework
         deriving (Eq, Show)

instance Tree Ty where
    fromTree Prog = App "Prog" []
    fromTree Interp = App "Interp" []
    fromTree Comp = App "Comp" []
    fromTree PlatF = App "PlatF" []
    fromTree Runnable = App "Runnable" []
    fromTree Framework = App "Framework" []

    toTree = parseTree [ app "Prog" (pure Prog),
                         app "Interp" (pure Interp),
                         app "Comp" (pure Comp),
                         app "PlatF" (pure PlatF),
                         app "Runnable" (pure Runnable),
                         app "Framework" (pure Framework)
                       ]



match :: Ty -> Ty -> Bool
match Runnable Runnable = True
match Runnable _ = False
match Framework Framework = True
match Framework _ = False
match Prog Runnable = True
match Interp Runnable = True
match Comp Runnable = True
match Interp Framework = True
match PlatF Framework = True
match ty1 ty2 = ty1 == ty2



findTypeInfo :: TypeInfo -> TypeInfo -> TypeInfo
findTypeInfo  _ _ = TypeInfo [] [] []
{-# LINE 65 "ag.hs" #-}

{-# LINE 78 "AG\\Typing.ag" #-}

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
{-# LINE 80 "ag.hs" #-}

{-# LINE 97 "AG\\Typing.ag" #-}

checkComp :: SourcePos -> Ty -> [TyErr]
checkComp _ ty | match ty Comp = []
checkComp pos ty               = [TyErr pos nonComp Comp ty]
    where
        nonComp = "Must be compiled using a compiler"

checkLang :: SourcePos -> Ty -> Ty -> [TyErr]
checkLang _ ty1 ty2 | match ty1 ty2 = []
checkLang pos ty1 ty2              = [TyErr pos nonExe Runnable ty1]
    where
        nonExe = "Does not belong to the correct language"
{-# LINE 95 "ag.hs" #-}

{-# LINE 112 "AG\\Typing.ag" #-}

data TypeInfo = TypeInfo {source :: [Ident], target :: [Ident], platform :: [Ident]}
{-# LINE 100 "ag.hs" #-}

{-# LINE 119 "AG\\Typing.ag" #-}

data TyErr = TyErr SourcePos String Ty Ty

instance Printable TyErr where
    pp = ppTyErr
{-# LINE 108 "ag.hs" #-}

{-# LINE 127 "AG\\Typing.ag" #-}

-- | Pretty prints a type error message.
ppTyErr :: TyErr -> Doc
ppTyErr (TyErr pos descr expected inferred)
  = above [ppHeader, text " ", ppExpected, ppInferred]
  where
    ppHeader   = wrapped $
                 describeSourcePos pos ++ ": Type error: " ++ descr ++ "."
    ppExpected = text "? expected : " >|< showable expected
    ppInferred = text "? inferred : " >|< showable inferred
{-# LINE 121 "ag.hs" #-}

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
{-# LINE 136 "ag.hs" #-}

{-# LINE 15 "AG\\Base.ag" #-}

type Ident = String
{-# LINE 141 "ag.hs" #-}

{-# LINE 34 "AG\\Base.ag" #-}

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
{-# LINE 167 "ag.hs" #-}
-- Diag --------------------------------------------------------
data Diag = Diag (SourcePos) (Diag_)
-- cata
sem_Diag :: Diag ->
            T_Diag
sem_Diag (Diag _pos _d) =
    (sem_Diag_Diag _pos (sem_Diag_ _d))
-- semantic domain
type T_Diag = ( SourcePos,Ty,([TyErr]),TypeInfo)
data Inh_Diag = Inh_Diag {}
data Syn_Diag = Syn_Diag {pos_Syn_Diag :: SourcePos,ty_Syn_Diag :: Ty,tyErrs_Syn_Diag :: ([TyErr]),tyInfo_Syn_Diag :: TypeInfo}
wrap_Diag :: T_Diag ->
             Inh_Diag ->
             Syn_Diag
wrap_Diag sem (Inh_Diag) =
    (let ( _lhsOpos,_lhsOty,_lhsOtyErrs,_lhsOtyInfo) = sem
     in  (Syn_Diag _lhsOpos _lhsOty _lhsOtyErrs _lhsOtyInfo))
sem_Diag_Diag :: SourcePos ->
                 T_Diag_ ->
                 T_Diag
sem_Diag_Diag pos_ d_ =
    (let _lhsOpos :: SourcePos
         _dOpos :: SourcePos
         _lhsOtyErrs :: ([TyErr])
         _lhsOty :: Ty
         _lhsOtyInfo :: TypeInfo
         _dIty :: Ty
         _dItyErrs :: ([TyErr])
         _dItyInfo :: TypeInfo
         _lhsOpos =
             ({-# LINE 14 "AG\\Pos.ag" #-}
              pos_
              {-# LINE 200 "ag.hs" #-}
              )
         _dOpos =
             ({-# LINE 20 "AG\\Pos.ag" #-}
              pos_
              {-# LINE 205 "ag.hs" #-}
              )
         _lhsOtyErrs =
             ({-# LINE 53 "AG\\Typing.ag" #-}
              _dItyErrs
              {-# LINE 210 "ag.hs" #-}
              )
         _lhsOty =
             ({-# LINE 52 "AG\\Typing.ag" #-}
              _dIty
              {-# LINE 215 "ag.hs" #-}
              )
         _lhsOtyInfo =
             ({-# LINE 54 "AG\\Typing.ag" #-}
              _dItyInfo
              {-# LINE 220 "ag.hs" #-}
              )
         ( _dIty,_dItyErrs,_dItyInfo) =
             d_ _dOpos
     in  ( _lhsOpos,_lhsOty,_lhsOtyErrs,_lhsOtyInfo))
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
               ( Ty,([TyErr]),TypeInfo)
data Inh_Diag_ = Inh_Diag_ {pos_Inh_Diag_ :: SourcePos}
data Syn_Diag_ = Syn_Diag_ {ty_Syn_Diag_ :: Ty,tyErrs_Syn_Diag_ :: ([TyErr]),tyInfo_Syn_Diag_ :: TypeInfo}
wrap_Diag_ :: T_Diag_ ->
              Inh_Diag_ ->
              Syn_Diag_
wrap_Diag_ sem (Inh_Diag_ _lhsIpos) =
    (let ( _lhsOty,_lhsOtyErrs,_lhsOtyInfo) = sem _lhsIpos
     in  (Syn_Diag_ _lhsOty _lhsOtyErrs _lhsOtyInfo))
sem_Diag__Program :: Ident ->
                     Ident ->
                     T_Diag_
sem_Diag__Program p_ l_ =
    (\ _lhsIpos ->
         (let _lhsOty :: Ty
              _lhsOtyInfo :: TypeInfo
              _lhsOtyErrs :: ([TyErr])
              _lhsOty =
                  ({-# LINE 57 "AG\\Typing.ag" #-}
                   Prog
                   {-# LINE 269 "ag.hs" #-}
                   )
              _lhsOtyInfo =
                  ({-# LINE 65 "AG\\Typing.ag" #-}
                   TypeInfo [l_] [] []
                   {-# LINE 274 "ag.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 53 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 279 "ag.hs" #-}
                   )
          in  ( _lhsOty,_lhsOtyErrs,_lhsOtyInfo)))
sem_Diag__Platform :: Ident ->
                      T_Diag_
sem_Diag__Platform m_ =
    (\ _lhsIpos ->
         (let _lhsOty :: Ty
              _lhsOtyInfo :: TypeInfo
              _lhsOtyErrs :: ([TyErr])
              _lhsOty =
                  ({-# LINE 60 "AG\\Typing.ag" #-}
                   PlatF
                   {-# LINE 292 "ag.hs" #-}
                   )
              _lhsOtyInfo =
                  ({-# LINE 68 "AG\\Typing.ag" #-}
                   TypeInfo [] [] [m_]
                   {-# LINE 297 "ag.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 53 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 302 "ag.hs" #-}
                   )
          in  ( _lhsOty,_lhsOtyErrs,_lhsOtyInfo)))
sem_Diag__Interpreter :: Ident ->
                         Ident ->
                         Ident ->
                         T_Diag_
sem_Diag__Interpreter i_ l_ m_ =
    (\ _lhsIpos ->
         (let _lhsOty :: Ty
              _lhsOtyInfo :: TypeInfo
              _lhsOtyErrs :: ([TyErr])
              _lhsOty =
                  ({-# LINE 58 "AG\\Typing.ag" #-}
                   Interp
                   {-# LINE 317 "ag.hs" #-}
                   )
              _lhsOtyInfo =
                  ({-# LINE 66 "AG\\Typing.ag" #-}
                   TypeInfo [l_] [] [m_]
                   {-# LINE 322 "ag.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 53 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 327 "ag.hs" #-}
                   )
          in  ( _lhsOty,_lhsOtyErrs,_lhsOtyInfo)))
sem_Diag__Compiler :: Ident ->
                      Ident ->
                      Ident ->
                      Ident ->
                      T_Diag_
sem_Diag__Compiler c_ l1_ l2_ m_ =
    (\ _lhsIpos ->
         (let _lhsOty :: Ty
              _lhsOtyInfo :: TypeInfo
              _lhsOtyErrs :: ([TyErr])
              _lhsOty =
                  ({-# LINE 59 "AG\\Typing.ag" #-}
                   Comp
                   {-# LINE 343 "ag.hs" #-}
                   )
              _lhsOtyInfo =
                  ({-# LINE 67 "AG\\Typing.ag" #-}
                   TypeInfo [l1_] [l2_] [m_]
                   {-# LINE 348 "ag.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 53 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 353 "ag.hs" #-}
                   )
          in  ( _lhsOty,_lhsOtyErrs,_lhsOtyInfo)))
sem_Diag__Execute :: T_Diag ->
                     T_Diag ->
                     T_Diag_
sem_Diag__Execute d1_ d2_ =
    (\ _lhsIpos ->
         (let _lhsOty :: Ty
              _lhsOtyInfo :: TypeInfo
              _lhsOtyErrs :: ([TyErr])
              _d1Ipos :: SourcePos
              _d1Ity :: Ty
              _d1ItyErrs :: ([TyErr])
              _d1ItyInfo :: TypeInfo
              _d2Ipos :: SourcePos
              _d2Ity :: Ty
              _d2ItyErrs :: ([TyErr])
              _d2ItyInfo :: TypeInfo
              _lhsOty =
                  ({-# LINE 61 "AG\\Typing.ag" #-}
                   _d1Ity
                   {-# LINE 375 "ag.hs" #-}
                   )
              _lhsOtyInfo =
                  ({-# LINE 70 "AG\\Typing.ag" #-}
                   findTypeInfo _d1ItyInfo _d2ItyInfo
                   {-# LINE 380 "ag.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 73 "AG\\Typing.ag" #-}
                   _d1ItyErrs ++ _d2ItyErrs ++
                   checkRunnable _d1Ipos _d1Ity ++
                   checkFramework _d2Ipos _d2Ity ++
                   checkLang _d1Ipos _d1Ity _d2Ity
                   {-# LINE 388 "ag.hs" #-}
                   )
              ( _d1Ipos,_d1Ity,_d1ItyErrs,_d1ItyInfo) =
                  d1_
              ( _d2Ipos,_d2Ity,_d2ItyErrs,_d2ItyInfo) =
                  d2_
          in  ( _lhsOty,_lhsOtyErrs,_lhsOtyInfo)))
sem_Diag__Compile :: T_Diag ->
                     T_Diag ->
                     T_Diag_
sem_Diag__Compile d1_ d2_ =
    (\ _lhsIpos ->
         (let _lhsOty :: Ty
              _lhsOtyInfo :: TypeInfo
              _lhsOtyErrs :: ([TyErr])
              _d1Ipos :: SourcePos
              _d1Ity :: Ty
              _d1ItyErrs :: ([TyErr])
              _d1ItyInfo :: TypeInfo
              _d2Ipos :: SourcePos
              _d2Ity :: Ty
              _d2ItyErrs :: ([TyErr])
              _d2ItyInfo :: TypeInfo
              _lhsOty =
                  ({-# LINE 62 "AG\\Typing.ag" #-}
                   _d1Ity
                   {-# LINE 414 "ag.hs" #-}
                   )
              _lhsOtyInfo =
                  ({-# LINE 69 "AG\\Typing.ag" #-}
                   findTypeInfo _d1ItyInfo _d2ItyInfo
                   {-# LINE 419 "ag.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 93 "AG\\Typing.ag" #-}
                   _d1ItyErrs ++ _d2ItyErrs ++
                   checkRunnable _d1Ipos _d1Ity ++
                   checkComp _d2Ipos _d2Ity
                   {-# LINE 426 "ag.hs" #-}
                   )
              ( _d1Ipos,_d1Ity,_d1ItyErrs,_d1ItyInfo) =
                  d1_
              ( _d2Ipos,_d2Ity,_d2ItyErrs,_d2ItyInfo) =
                  d2_
          in  ( _lhsOty,_lhsOtyErrs,_lhsOtyInfo)))