

-- UUAGC 0.9.52.1 (AG.ag)
module CCO.Diag.AG where

{-# LINE 2 "AG\\Pos.ag" #-}

import CCO.SourcePos
{-# LINE 10 "AG.hs" #-}

{-# LINE 2 "..\\Diag.ag" #-}

import CCO.Feedback
import CCO.Printing
import CCO.SourcePos        (SourcePos)
import CCO.Tree             (ATerm (App), Tree (fromTree, toTree))
import CCO.Tree.Parser      (parseTree, app, arg)
import Control.Applicative  (Applicative ((<*>)), (<$>))
import Data.Maybe
{-# LINE 21 "AG.hs" #-}
{-# LINE 3 "AG\\Typing.ag" #-}

data TyCons = Prog
        | Interp
        | Comp
        | PlatF
        | Runnable
        | Framework
         deriving (Eq, Show)

data Ty = Ty {cons :: TyCons, source :: Maybe Ident, target :: Maybe Ident, platform :: Maybe Ident}deriving (Show, Eq)

show' :: Maybe String -> String
show' (Just x) = show x
show' Nothing  = ""   

instance Tree Ty where
    fromTree (Ty c s t p) = App "Ty" [fromTree c, fromTree s, fromTree t, fromTree p]
    toTree = parseTree [ app "Ty" (Ty <$> arg <*> arg <*> arg <*> arg)]

instance Tree TyCons where
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

match :: TyCons -> TyCons -> Bool
match Prog Runnable       = True
match Interp Runnable     = True
match Comp Runnable       = True
match Interp Framework    = True
match PlatF Framework     = True
match ty1 ty2             = ty1 == ty2 


matchInfo :: Maybe Ident -> Maybe Ident -> Bool
matchInfo (Just i) (Just j) = i == j
matchInfo _ _ = True

translate :: Ty -> Ty -> Ty
translate (Ty Prog s1 t1 m1) (Ty Comp s2 t2 m2)   = Ty Prog t2 t1 m1
translate (Ty Interp s1 t1 m1) (Ty Comp s2 t2 m2) = Ty Interp s1 t1 t2
translate (Ty Comp s1 t1 m1) (Ty Comp s2 t2 m2) = Ty Comp s1 t1 t2
translate tyinfo1 _ = tyinfo1

{-# LINE 77 "AG.hs" #-}

{-# LINE 81 "AG\\Typing.ag" #-}

checkRunnable :: SourcePos -> TyCons -> [TyErr]
checkRunnable pos ty | match ty Runnable = []
                     | otherwise          = [TyErr pos nonExe (show Runnable) (show ty)]
    where
        nonExe = "Cannot execute a non-runnable"

checkFramework :: SourcePos -> TyCons -> [TyErr]
checkFramework pos ty | match ty Framework = []
                          | otherwise          = [TyErr pos nonFrame (show Framework) (show ty)]
    where
        nonFrame = "Cannot execute on non-Framework"
{-# LINE 92 "AG.hs" #-}

{-# LINE 101 "AG\\Typing.ag" #-}

checkComp :: SourcePos -> TyCons -> [TyErr]
checkComp pos ty | match ty Comp = []
                 | otherwise = [TyErr pos nonComp (show Comp) (show ty)]
    where
        nonComp = "Must be compiled using a compiler"

checkIfMatches :: SourcePos -> Ty -> Ty -> [TyErr]
checkIfMatches pos (Ty Prog s1 _ m1) (Ty Interp s2 _ m2) | matchInfo s1 s2 = []
                                                         | otherwise = genTyInfoErr pos s1 s2
checkIfMatches pos (Ty _    s1 _ m1) (Ty Interp s2 _ m2) | matchInfo m1 s2 = []
                                                         | otherwise = genTyInfoErr pos m1 s2
checkIfMatches pos (Ty Prog s1 _ m1) (Ty PlatF  s2 _ m2) | matchInfo s1 m2 = []
                                                         | otherwise = genTyInfoErr pos s1 m2
checkIfMatches pos (Ty _    s1 _ m1) (Ty PlatF  s2 _ m2) | matchInfo m1 m2 = []
                                                         | otherwise = genTyInfoErr pos m1 m2
checkIfMatches pos (Ty Prog s1 _ m1) (Ty Comp   s2 _ m2) | matchInfo s1 s2 = []
                                                         | otherwise = genTyInfoErr pos s1 s2
checkIfMatches pos (Ty _    s1 _ m1) (Ty Comp   s2 _ m2) | matchInfo m1 s2 = []
                                                         | otherwise = genTyInfoErr pos m1 s2
checkIfMatches _ _ _ = [] --Type error has occurred so we cannot match type infos


genTyInfoErr :: SourcePos -> Maybe Ident -> Maybe Ident -> [TyErr]
genTyInfoErr pos mi mj = [TyErr pos descr (show' mi) (show' mj)]
    where
        descr = "Cannot execute runnable on a non-matching platform or interpreter"
{-# LINE 122 "AG.hs" #-}

{-# LINE 132 "AG\\Typing.ag" #-}

data TyErr = TyErr SourcePos String String String

instance Printable TyErr where
    pp = ppTyErr
{-# LINE 130 "AG.hs" #-}

{-# LINE 140 "AG\\Typing.ag" #-}

-- | Pretty prints a type error message.
ppTyErr :: TyErr -> Doc
ppTyErr (TyErr pos descr expected inferred)
  = ppErr "Type error" pos descr expected inferred

ppErr :: String -> SourcePos -> String -> String -> String -> Doc
ppErr msg pos descr a b =
    above [ppHeader, text " ", ppExpected, ppInferred]
    where
        ppHeader = wrapped $
                    describeSourcePos pos ++ ": " ++ msg ++ ": " ++ descr ++ "."
        ppExpected = text "? expected : " >|< text a
        ppInferred = text "? inferred : " >|< text b
{-# LINE 147 "AG.hs" #-}

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
{-# LINE 162 "AG.hs" #-}

{-# LINE 16 "..\\Diag.ag" #-}

type Ident = String
{-# LINE 167 "AG.hs" #-}

{-# LINE 35 "..\\Diag.ag" #-}

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
{-# LINE 193 "AG.hs" #-}
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
              {-# LINE 224 "AG.hs" #-}
              )
         _dOpos =
             ({-# LINE 20 "AG\\Pos.ag" #-}
              pos_
              {-# LINE 229 "AG.hs" #-}
              )
         _lhsOtyErrs =
             ({-# LINE 64 "AG\\Typing.ag" #-}
              _dItyErrs
              {-# LINE 234 "AG.hs" #-}
              )
         _lhsOty =
             ({-# LINE 63 "AG\\Typing.ag" #-}
              _dIty
              {-# LINE 239 "AG.hs" #-}
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
                  ({-# LINE 67 "AG\\Typing.ag" #-}
                   Ty Prog (Just l_) Nothing Nothing
                   {-# LINE 287 "AG.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 64 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 292 "AG.hs" #-}
                   )
          in  ( _lhsOty,_lhsOtyErrs)))
sem_Diag__Platform :: Ident ->
                      T_Diag_
sem_Diag__Platform m_ =
    (\ _lhsIpos ->
         (let _lhsOty :: Ty
              _lhsOtyErrs :: ([TyErr])
              _lhsOty =
                  ({-# LINE 70 "AG\\Typing.ag" #-}
                   Ty PlatF Nothing Nothing (Just m_)
                   {-# LINE 304 "AG.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 64 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 309 "AG.hs" #-}
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
                  ({-# LINE 68 "AG\\Typing.ag" #-}
                   Ty Interp (Just l_) Nothing (Just m_)
                   {-# LINE 323 "AG.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 64 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 328 "AG.hs" #-}
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
                  ({-# LINE 69 "AG\\Typing.ag" #-}
                   Ty Comp (Just l1_) (Just l2_) (Just m_)
                   {-# LINE 343 "AG.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 64 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 348 "AG.hs" #-}
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
                  ({-# LINE 71 "AG\\Typing.ag" #-}
                   _d2Ity
                   {-# LINE 367 "AG.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 76 "AG\\Typing.ag" #-}
                   _d1ItyErrs ++ _d2ItyErrs ++
                   checkRunnable _d1Ipos (cons _d1Ity) ++
                   checkFramework _d2Ipos (cons _d2Ity) ++
                   checkIfMatches _d1Ipos _d1Ity _d2Ity
                   {-# LINE 375 "AG.hs" #-}
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
                  ({-# LINE 72 "AG\\Typing.ag" #-}
                   translate _d1Ity _d2Ity
                   {-# LINE 398 "AG.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 96 "AG\\Typing.ag" #-}
                   _d1ItyErrs  ++ _d2ItyErrs ++
                   checkRunnable  _d1Ipos (cons _d1Ity) ++
                   checkComp      _d2Ipos (cons _d2Ity) ++
                   checkIfMatches _d1Ipos _d1Ity _d2Ity
                   {-# LINE 406 "AG.hs" #-}
                   )
              ( _d1Ipos,_d1Ity,_d1ItyErrs) =
                  d1_
              ( _d2Ipos,_d2Ity,_d2ItyErrs) =
                  d2_
          in  ( _lhsOty,_lhsOtyErrs)))