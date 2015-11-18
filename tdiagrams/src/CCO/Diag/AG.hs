

-- UUAGC 0.9.52.1 (AG.ag)
module CCO.Diag.AG where

{-# LINE 2 "AG\\Pos.ag" #-}

import CCO.SourcePos
{-# LINE 10 "AG.hs" #-}

{-# LINE 2 "AG\\Base.ag" #-}

import CCO.Feedback
import CCO.Printing
import CCO.SourcePos        (SourcePos)
import CCO.Tree             (ATerm (App), Tree (fromTree, toTree))
import CCO.Tree.Parser      (parseTree, app, arg)
import Control.Applicative  (Applicative ((<*>)), (<$>))
import Data.Maybe
{-# LINE 21 "AG.hs" #-}
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

matchInfo :: Maybe Ident -> Maybe Ident -> Bool
matchInfo (Just i) (Just j) = i == j
matchInfo _ _ = True
{-# LINE 65 "AG.hs" #-}

{-# LINE 77 "AG\\Typing.ag" #-}

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
{-# LINE 80 "AG.hs" #-}

{-# LINE 96 "AG\\Typing.ag" #-}

checkComp :: SourcePos -> Ty -> [TyErr]
checkComp _ ty | match ty Comp = []
checkComp pos ty               = [TyErr pos nonComp Comp ty]
    where
        nonComp = "Must be compiled using a compiler"

{-# LINE 90 "AG.hs" #-}

{-# LINE 111 "AG\\Typing.ag" #-}

checkIfMatches :: SourcePos -> Ty -> Ty -> TyInfo -> TyInfo -> [TyInfoErr]
checkIfMatches _ Prog Interp (TyInfo s1 t1 m1) (TyInfo s2 t2 m2) | matchInfo s1 s2 = []
checkIfMatches pos Prog Interp (TyInfo s1 t1 m1) (TyInfo s2 t2 m2) = genTyInfoErr pos s1 s2
checkIfMatches _ _ Interp (TyInfo s1 t1 m1) (TyInfo s2 t2 m2) | matchInfo m1 s2 = []
checkIfMatches pos _ Interp (TyInfo s1 t1 m1) (TyInfo s2 t2 m2) = genTyInfoErr pos m1 s2
checkIfMatches _ Prog PlatF (TyInfo s1 t1 m1) (TyInfo s2 t2 m2) | matchInfo s1 m2 = []
checkIfMatches pos Prog PlatF (TyInfo s1 t1 m1) (TyInfo s2 t2 m2) = genTyInfoErr pos s1 m2
checkIfMatches _ _ PlatF (TyInfo s1 t1 m1) (TyInfo s2 t2 m2) | matchInfo m1 m2 = []
checkIfMatches pos _ PlatF (TyInfo s1 t1 m1) (TyInfo s2 t2 m2) = genTyInfoErr pos m1 m2
checkIfMatches pos Prog Comp (TyInfo s1 t1 m1) (TyInfo s2 t2 m2)   | matchInfo s1 s2 = []
                                                                   | otherwise = genTyInfoErr pos s1 s2
checkIfMatches pos _ Comp (TyInfo s1 t1 m1) (TyInfo s2 t2 m2)      | matchInfo m1 s2 = []
                                                                   | otherwise = genTyInfoErr pos m1 s2
checkIfMatches _ _ _ _ _ = [] --Type error has occurred so we cannot match type infos



genTyInfoErr :: SourcePos -> Maybe Ident -> Maybe Ident -> [TyInfoErr]
genTyInfoErr pos mi mj = [TyInfoErr pos descr mi mj]
    where
        descr = "Cannot execute runnable on a non-matching platform or interpreter"
{-# LINE 115 "AG.hs" #-}

{-# LINE 136 "AG\\Typing.ag" #-}

data TyInfo = TyInfo {source :: Maybe Ident, target :: Maybe Ident, platform :: Maybe Ident}

data TyInfoErr = TyInfoErr SourcePos String (Maybe Ident) (Maybe Ident)

instance Printable TyInfoErr where
    pp = ppTyInfoErr
{-# LINE 125 "AG.hs" #-}

{-# LINE 148 "AG\\Typing.ag" #-}

data TyErr = TyErr SourcePos String Ty Ty

instance Printable TyErr where
    pp = ppTyErr
{-# LINE 133 "AG.hs" #-}

{-# LINE 156 "AG\\Typing.ag" #-}

-- | Pretty prints a type error message.
ppTyErr :: TyErr -> Doc
ppTyErr (TyErr pos descr expected inferred)
  = ppErr "Type error" pos descr expected inferred

-- | Pretty prints a type info error message
ppTyInfoErr :: TyInfoErr -> Doc
ppTyInfoErr (TyInfoErr pos descr expected inferred)
    = ppErr "Type info error" pos descr expected inferred

ppErr :: (Show a, Show b) => String -> SourcePos -> String -> a -> b -> Doc
ppErr msg pos descr a b =
    above [ppHeader, text " ", ppExpected, ppInferred]
    where
        ppHeader = wrapped $
                    describeSourcePos pos ++ ": " ++ msg ++ ": " ++ descr ++ "."
        ppExpected = text "? expected : " >|< showable a
        ppInferred = text "? inferred : " >|< showable b
{-# LINE 155 "AG.hs" #-}

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
{-# LINE 170 "AG.hs" #-}

{-# LINE 16 "AG\\Base.ag" #-}

type Ident = String
{-# LINE 175 "AG.hs" #-}

{-# LINE 35 "AG\\Base.ag" #-}

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
{-# LINE 201 "AG.hs" #-}
-- Diag --------------------------------------------------------
data Diag = Diag (SourcePos) (Diag_)
-- cata
sem_Diag :: Diag ->
            T_Diag
sem_Diag (Diag _pos _d) =
    (sem_Diag_Diag _pos (sem_Diag_ _d))
-- semantic domain
type T_Diag = ( SourcePos,Ty,([TyErr]),TyInfo,([TyInfoErr]))
data Inh_Diag = Inh_Diag {}
data Syn_Diag = Syn_Diag {pos_Syn_Diag :: SourcePos,ty_Syn_Diag :: Ty,tyErrs_Syn_Diag :: ([TyErr]),tyInfo_Syn_Diag :: TyInfo,tyInfoErrs_Syn_Diag :: ([TyInfoErr])}
wrap_Diag :: T_Diag ->
             Inh_Diag ->
             Syn_Diag
wrap_Diag sem (Inh_Diag) =
    (let ( _lhsOpos,_lhsOty,_lhsOtyErrs,_lhsOtyInfo,_lhsOtyInfoErrs) = sem
     in  (Syn_Diag _lhsOpos _lhsOty _lhsOtyErrs _lhsOtyInfo _lhsOtyInfoErrs))
sem_Diag_Diag :: SourcePos ->
                 T_Diag_ ->
                 T_Diag
sem_Diag_Diag pos_ d_ =
    (let _lhsOpos :: SourcePos
         _dOpos :: SourcePos
         _lhsOtyErrs :: ([TyErr])
         _lhsOtyInfoErrs :: ([TyInfoErr])
         _lhsOty :: Ty
         _lhsOtyInfo :: TyInfo
         _dIty :: Ty
         _dItyErrs :: ([TyErr])
         _dItyInfo :: TyInfo
         _dItyInfoErrs :: ([TyInfoErr])
         _lhsOpos =
             ({-# LINE 14 "AG\\Pos.ag" #-}
              pos_
              {-# LINE 236 "AG.hs" #-}
              )
         _dOpos =
             ({-# LINE 20 "AG\\Pos.ag" #-}
              pos_
              {-# LINE 241 "AG.hs" #-}
              )
         _lhsOtyErrs =
             ({-# LINE 52 "AG\\Typing.ag" #-}
              _dItyErrs
              {-# LINE 246 "AG.hs" #-}
              )
         _lhsOtyInfoErrs =
             ({-# LINE 54 "AG\\Typing.ag" #-}
              _dItyInfoErrs
              {-# LINE 251 "AG.hs" #-}
              )
         _lhsOty =
             ({-# LINE 51 "AG\\Typing.ag" #-}
              _dIty
              {-# LINE 256 "AG.hs" #-}
              )
         _lhsOtyInfo =
             ({-# LINE 53 "AG\\Typing.ag" #-}
              _dItyInfo
              {-# LINE 261 "AG.hs" #-}
              )
         ( _dIty,_dItyErrs,_dItyInfo,_dItyInfoErrs) =
             d_ _dOpos
     in  ( _lhsOpos,_lhsOty,_lhsOtyErrs,_lhsOtyInfo,_lhsOtyInfoErrs))
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
               ( Ty,([TyErr]),TyInfo,([TyInfoErr]))
data Inh_Diag_ = Inh_Diag_ {pos_Inh_Diag_ :: SourcePos}
data Syn_Diag_ = Syn_Diag_ {ty_Syn_Diag_ :: Ty,tyErrs_Syn_Diag_ :: ([TyErr]),tyInfo_Syn_Diag_ :: TyInfo,tyInfoErrs_Syn_Diag_ :: ([TyInfoErr])}
wrap_Diag_ :: T_Diag_ ->
              Inh_Diag_ ->
              Syn_Diag_
wrap_Diag_ sem (Inh_Diag_ _lhsIpos) =
    (let ( _lhsOty,_lhsOtyErrs,_lhsOtyInfo,_lhsOtyInfoErrs) = sem _lhsIpos
     in  (Syn_Diag_ _lhsOty _lhsOtyErrs _lhsOtyInfo _lhsOtyInfoErrs))
sem_Diag__Program :: Ident ->
                     Ident ->
                     T_Diag_
sem_Diag__Program p_ l_ =
    (\ _lhsIpos ->
         (let _lhsOty :: Ty
              _lhsOtyInfo :: TyInfo
              _lhsOtyErrs :: ([TyErr])
              _lhsOtyInfoErrs :: ([TyInfoErr])
              _lhsOty =
                  ({-# LINE 57 "AG\\Typing.ag" #-}
                   Prog
                   {-# LINE 311 "AG.hs" #-}
                   )
              _lhsOtyInfo =
                  ({-# LINE 65 "AG\\Typing.ag" #-}
                   TyInfo (Just l_) Nothing Nothing
                   {-# LINE 316 "AG.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 52 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 321 "AG.hs" #-}
                   )
              _lhsOtyInfoErrs =
                  ({-# LINE 54 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 326 "AG.hs" #-}
                   )
          in  ( _lhsOty,_lhsOtyErrs,_lhsOtyInfo,_lhsOtyInfoErrs)))
sem_Diag__Platform :: Ident ->
                      T_Diag_
sem_Diag__Platform m_ =
    (\ _lhsIpos ->
         (let _lhsOty :: Ty
              _lhsOtyInfo :: TyInfo
              _lhsOtyErrs :: ([TyErr])
              _lhsOtyInfoErrs :: ([TyInfoErr])
              _lhsOty =
                  ({-# LINE 60 "AG\\Typing.ag" #-}
                   PlatF
                   {-# LINE 340 "AG.hs" #-}
                   )
              _lhsOtyInfo =
                  ({-# LINE 68 "AG\\Typing.ag" #-}
                   TyInfo Nothing Nothing (Just m_)
                   {-# LINE 345 "AG.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 52 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 350 "AG.hs" #-}
                   )
              _lhsOtyInfoErrs =
                  ({-# LINE 54 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 355 "AG.hs" #-}
                   )
          in  ( _lhsOty,_lhsOtyErrs,_lhsOtyInfo,_lhsOtyInfoErrs)))
sem_Diag__Interpreter :: Ident ->
                         Ident ->
                         Ident ->
                         T_Diag_
sem_Diag__Interpreter i_ l_ m_ =
    (\ _lhsIpos ->
         (let _lhsOty :: Ty
              _lhsOtyInfo :: TyInfo
              _lhsOtyErrs :: ([TyErr])
              _lhsOtyInfoErrs :: ([TyInfoErr])
              _lhsOty =
                  ({-# LINE 58 "AG\\Typing.ag" #-}
                   Interp
                   {-# LINE 371 "AG.hs" #-}
                   )
              _lhsOtyInfo =
                  ({-# LINE 66 "AG\\Typing.ag" #-}
                   TyInfo (Just l_) Nothing (Just m_)
                   {-# LINE 376 "AG.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 52 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 381 "AG.hs" #-}
                   )
              _lhsOtyInfoErrs =
                  ({-# LINE 54 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 386 "AG.hs" #-}
                   )
          in  ( _lhsOty,_lhsOtyErrs,_lhsOtyInfo,_lhsOtyInfoErrs)))
sem_Diag__Compiler :: Ident ->
                      Ident ->
                      Ident ->
                      Ident ->
                      T_Diag_
sem_Diag__Compiler c_ l1_ l2_ m_ =
    (\ _lhsIpos ->
         (let _lhsOty :: Ty
              _lhsOtyInfo :: TyInfo
              _lhsOtyErrs :: ([TyErr])
              _lhsOtyInfoErrs :: ([TyInfoErr])
              _lhsOty =
                  ({-# LINE 59 "AG\\Typing.ag" #-}
                   Comp
                   {-# LINE 403 "AG.hs" #-}
                   )
              _lhsOtyInfo =
                  ({-# LINE 67 "AG\\Typing.ag" #-}
                   TyInfo (Just l1_) (Just l2_) (Just m_)
                   {-# LINE 408 "AG.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 52 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 413 "AG.hs" #-}
                   )
              _lhsOtyInfoErrs =
                  ({-# LINE 54 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 418 "AG.hs" #-}
                   )
          in  ( _lhsOty,_lhsOtyErrs,_lhsOtyInfo,_lhsOtyInfoErrs)))
sem_Diag__Execute :: T_Diag ->
                     T_Diag ->
                     T_Diag_
sem_Diag__Execute d1_ d2_ =
    (\ _lhsIpos ->
         (let _lhsOty :: Ty
              _lhsOtyInfo :: TyInfo
              _lhsOtyErrs :: ([TyErr])
              _lhsOtyInfoErrs :: ([TyInfoErr])
              _d1Ipos :: SourcePos
              _d1Ity :: Ty
              _d1ItyErrs :: ([TyErr])
              _d1ItyInfo :: TyInfo
              _d1ItyInfoErrs :: ([TyInfoErr])
              _d2Ipos :: SourcePos
              _d2Ity :: Ty
              _d2ItyErrs :: ([TyErr])
              _d2ItyInfo :: TyInfo
              _d2ItyInfoErrs :: ([TyInfoErr])
              _lhsOty =
                  ({-# LINE 61 "AG\\Typing.ag" #-}
                   _d2Ity
                   {-# LINE 443 "AG.hs" #-}
                   )
              _lhsOtyInfo =
                  ({-# LINE 69 "AG\\Typing.ag" #-}
                   _d2ItyInfo
                   {-# LINE 448 "AG.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 73 "AG\\Typing.ag" #-}
                   _d1ItyErrs ++ _d2ItyErrs ++
                   checkRunnable _d1Ipos _d1Ity ++
                   checkFramework _d2Ipos _d2Ity
                   {-# LINE 455 "AG.hs" #-}
                   )
              _lhsOtyInfoErrs =
                  ({-# LINE 106 "AG\\Typing.ag" #-}
                   _d1ItyInfoErrs ++ _d2ItyInfoErrs ++
                    checkIfMatches _d1Ipos _d1Ity _d2Ity _d1ItyInfo _d2ItyInfo
                   {-# LINE 461 "AG.hs" #-}
                   )
              ( _d1Ipos,_d1Ity,_d1ItyErrs,_d1ItyInfo,_d1ItyInfoErrs) =
                  d1_
              ( _d2Ipos,_d2Ity,_d2ItyErrs,_d2ItyInfo,_d2ItyInfoErrs) =
                  d2_
          in  ( _lhsOty,_lhsOtyErrs,_lhsOtyInfo,_lhsOtyInfoErrs)))
sem_Diag__Compile :: T_Diag ->
                     T_Diag ->
                     T_Diag_
sem_Diag__Compile d1_ d2_ =
    (\ _lhsIpos ->
         (let _lhsOty :: Ty
              _lhsOtyInfo :: TyInfo
              _lhsOtyErrs :: ([TyErr])
              _lhsOtyInfoErrs :: ([TyInfoErr])
              _d1Ipos :: SourcePos
              _d1Ity :: Ty
              _d1ItyErrs :: ([TyErr])
              _d1ItyInfo :: TyInfo
              _d1ItyInfoErrs :: ([TyInfoErr])
              _d2Ipos :: SourcePos
              _d2Ity :: Ty
              _d2ItyErrs :: ([TyErr])
              _d2ItyInfo :: TyInfo
              _d2ItyInfoErrs :: ([TyInfoErr])
              _lhsOty =
                  ({-# LINE 62 "AG\\Typing.ag" #-}
                   _d1Ity
                   {-# LINE 490 "AG.hs" #-}
                   )
              _lhsOtyInfo =
                  ({-# LINE 70 "AG\\Typing.ag" #-}
                   _d1ItyInfo
                   {-# LINE 495 "AG.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 92 "AG\\Typing.ag" #-}
                   _d1ItyErrs ++ _d2ItyErrs ++
                   checkRunnable _d1Ipos _d1Ity ++
                   checkComp _d2Ipos _d2Ity
                   {-# LINE 502 "AG.hs" #-}
                   )
              _lhsOtyInfoErrs =
                  ({-# LINE 108 "AG\\Typing.ag" #-}
                   _d1ItyInfoErrs ++ _d2ItyInfoErrs ++
                    checkIfMatches _d1Ipos _d1Ity _d2Ity _d1ItyInfo _d2ItyInfo
                   {-# LINE 508 "AG.hs" #-}
                   )
              ( _d1Ipos,_d1Ity,_d1ItyErrs,_d1ItyInfo,_d1ItyInfoErrs) =
                  d1_
              ( _d2Ipos,_d2Ity,_d2ItyErrs,_d2ItyInfo,_d2ItyInfoErrs) =
                  d2_
          in  ( _lhsOty,_lhsOtyErrs,_lhsOtyInfo,_lhsOtyInfoErrs)))