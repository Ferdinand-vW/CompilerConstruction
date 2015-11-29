

-- UUAGC 0.9.52.1 (Ag.ag)
module CCO.Diag.AG where

{-# LINE 2 "AG\\Pos.ag" #-}

import CCO.SourcePos
{-# LINE 10 "Ag.hs" #-}

{-# LINE 2 "..\\Diag.ag" #-}

import CCO.Feedback
import CCO.Printing
import CCO.SourcePos        (SourcePos)
import CCO.Tree             (ATerm (App), Tree (fromTree, toTree))
import CCO.Tree.Parser      (parseTree, app, arg)
import Control.Applicative  (Applicative ((<*>)), (<$>))
import Data.Maybe
{-# LINE 21 "Ag.hs" #-}
{-# LINE 3 "AG\\Typing.ag" #-}

data TyCons = Prog
        | Interp
        | Comp
        | PlatF
        | Runnable
        | Framework
        | Executed
        | Compiled
        | Not_Executed
        deriving (Eq,Show)

data Recursive = Left_recursive
        | Right_recursive
        | Not_recursive
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

matchR :: Recursive -> Recursive -> Bool
matchR Left_recursive Not_recursive = True
matchR Right_recursive Not_recursive = True
matchR Not_recursive Left_recursive = True
matchR Not_recursive Right_recursive = True
matchR r1 r2 = r1 == r2

matchInfo :: Maybe Ident -> Maybe Ident -> Bool
matchInfo (Just i) (Just j) = i == j
matchInfo _ _ = True

translate :: Ty -> Ty -> Ty
translate (Ty Prog s1 t1 m1) (Ty Comp s2 t2 m2)   = Ty Prog t2 t1 m1
translate (Ty Interp s1 t1 m1) (Ty Comp s2 t2 m2) = Ty Interp s1 t1 t2
translate (Ty Comp s1 t1 m1) (Ty Comp s2 t2 m2) = Ty Comp s1 t1 t2
translate tyinfo1 _ = tyinfo1

rightRecursion :: TyCons -> Recursive
rightRecursion Compiled = Right_recursive
rightRecursion _ = Not_recursive

leftRecursion :: TyCons -> Recursive 
leftRecursion Compiled = Left_recursive
leftRecursion _ = Not_recursive

{-# LINE 99 "Ag.hs" #-}

{-# LINE 118 "AG\\Typing.ag" #-}

checkRunnable :: SourcePos -> TyCons -> [TyErr]
checkRunnable pos ty | match ty Runnable = []
                     | otherwise          = [TyErr pos nonExe (show Runnable) (show ty)]
    where
        nonExe = "Cannot execute or compile a non-runnable"

checkFramework :: SourcePos -> TyCons -> [TyErr]
checkFramework pos ty | match ty Framework = []
                          | otherwise          = [TyErr pos nonFrame (show Framework) (show ty)]
    where
        nonFrame = "Cannot execute on non-Framework"

checkExeOrCompile :: SourcePos -> TyCons -> [TyErr]
checkExeOrCompile pos ty
    | match ty Executed || match ty Compiled = [TyErr pos descr (show Framework) (show ty)]
    | otherwise = []
    where
        descr = "Cannot execute runnable on a compilation or execution"
{-# LINE 121 "Ag.hs" #-}

{-# LINE 150 "AG\\Typing.ag" #-}

checkComp :: SourcePos -> TyCons -> [TyErr]
checkComp pos ty | match ty Comp = []
                 | otherwise = [TyErr pos nonComp (show Comp) (show ty)]
    where
        nonComp = "Must be compiled using a compiler"

checkExeInCompile :: SourcePos -> TyCons -> [TyErr]
checkExeInCompile pos ty | match ty Executed = [TyErr pos descr (show Not_Executed) (show ty)]
                         | otherwise = []
    where
        descr = "Cannot have an execution within a compilation"

checkLandRrecurs :: SourcePos -> Recursive -> Recursive -> [TyErr]
checkLandRrecurs pos ty1 ty2
    | ty1 == Right_recursive && ty2 == Left_recursive 
            = [TyErr pos descr (show Right_recursive ++ " !AND " ++ show Left_recursive) (show Right_recursive ++ " AND " ++ show Left_recursive)]
    | otherwise = []
    where
        descr = "Cannot have left recursive and right recursive compilations within a compile at the same time" 

checkLeftRightRecurs :: SourcePos -> Recursive -> Recursive -> [TyErr]
checkLeftRightRecurs pos child par 
    | matchR par child = []
    | otherwise = [TyErr pos (descr par) (show par ++ " OR " ++ show Not_recursive) (show child)]
    where
        descr Left_recursive = "Cannot have right recursive compilation in a left recursive compilation"
        descr Right_recursive = "Cannot have left recursive compilation in a right recursive compilation"

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
        descr = "Cannot execute or compile runnable on a non-matching platform or interpreter"
{-# LINE 173 "Ag.hs" #-}

{-# LINE 203 "AG\\Typing.ag" #-}

data TyErr = TyErr SourcePos String String String

instance Printable TyErr where
    pp = ppTyErr
{-# LINE 181 "Ag.hs" #-}

{-# LINE 211 "AG\\Typing.ag" #-}

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
{-# LINE 198 "Ag.hs" #-}

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
{-# LINE 213 "Ag.hs" #-}

{-# LINE 16 "..\\Diag.ag" #-}

type Ident = String
{-# LINE 218 "Ag.hs" #-}

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
{-# LINE 244 "Ag.hs" #-}
-- Diag --------------------------------------------------------
data Diag = Diag (SourcePos) (Diag_)
-- cata
sem_Diag :: Diag ->
            T_Diag
sem_Diag (Diag _pos _d) =
    (sem_Diag_Diag _pos (sem_Diag_ _d))
-- semantic domain
type T_Diag = Recursive ->
              ( SourcePos,Ty,([TyErr]),TyCons)
data Inh_Diag = Inh_Diag {recurs_Inh_Diag :: Recursive}
data Syn_Diag = Syn_Diag {pos_Syn_Diag :: SourcePos,ty_Syn_Diag :: Ty,tyErrs_Syn_Diag :: ([TyErr]),tycons_Syn_Diag :: TyCons}
wrap_Diag :: T_Diag ->
             Inh_Diag ->
             Syn_Diag
wrap_Diag sem (Inh_Diag _lhsIrecurs) =
    (let ( _lhsOpos,_lhsOty,_lhsOtyErrs,_lhsOtycons) = sem _lhsIrecurs
     in  (Syn_Diag _lhsOpos _lhsOty _lhsOtyErrs _lhsOtycons))
sem_Diag_Diag :: SourcePos ->
                 T_Diag_ ->
                 T_Diag
sem_Diag_Diag pos_ d_ =
    (\ _lhsIrecurs ->
         (let _lhsOpos :: SourcePos
              _dOpos :: SourcePos
              _lhsOtyErrs :: ([TyErr])
              _lhsOty :: Ty
              _lhsOtycons :: TyCons
              _dOrecurs :: Recursive
              _dIty :: Ty
              _dItyErrs :: ([TyErr])
              _dItycons :: TyCons
              _lhsOpos =
                  ({-# LINE 14 "AG\\Pos.ag" #-}
                   pos_
                   {-# LINE 280 "Ag.hs" #-}
                   )
              _dOpos =
                  ({-# LINE 20 "AG\\Pos.ag" #-}
                   pos_
                   {-# LINE 285 "Ag.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 88 "AG\\Typing.ag" #-}
                   _dItyErrs
                   {-# LINE 290 "Ag.hs" #-}
                   )
              _lhsOty =
                  ({-# LINE 87 "AG\\Typing.ag" #-}
                   _dIty
                   {-# LINE 295 "Ag.hs" #-}
                   )
              _lhsOtycons =
                  ({-# LINE 85 "AG\\Typing.ag" #-}
                   _dItycons
                   {-# LINE 300 "Ag.hs" #-}
                   )
              _dOrecurs =
                  ({-# LINE 86 "AG\\Typing.ag" #-}
                   _lhsIrecurs
                   {-# LINE 305 "Ag.hs" #-}
                   )
              ( _dIty,_dItyErrs,_dItycons) =
                  d_ _dOpos _dOrecurs
          in  ( _lhsOpos,_lhsOty,_lhsOtyErrs,_lhsOtycons)))
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
               Recursive ->
               ( Ty,([TyErr]),TyCons)
data Inh_Diag_ = Inh_Diag_ {pos_Inh_Diag_ :: SourcePos,recurs_Inh_Diag_ :: Recursive}
data Syn_Diag_ = Syn_Diag_ {ty_Syn_Diag_ :: Ty,tyErrs_Syn_Diag_ :: ([TyErr]),tycons_Syn_Diag_ :: TyCons}
wrap_Diag_ :: T_Diag_ ->
              Inh_Diag_ ->
              Syn_Diag_
wrap_Diag_ sem (Inh_Diag_ _lhsIpos _lhsIrecurs) =
    (let ( _lhsOty,_lhsOtyErrs,_lhsOtycons) = sem _lhsIpos _lhsIrecurs
     in  (Syn_Diag_ _lhsOty _lhsOtyErrs _lhsOtycons))
sem_Diag__Program :: Ident ->
                     Ident ->
                     T_Diag_
sem_Diag__Program p_ l_ =
    (\ _lhsIpos
       _lhsIrecurs ->
         (let _lhsOty :: Ty
              _lhsOtycons :: TyCons
              _lhsOtyErrs :: ([TyErr])
              _lhsOty =
                  ({-# LINE 91 "AG\\Typing.ag" #-}
                   Ty Prog (Just l_) Nothing Nothing
                   {-# LINE 356 "Ag.hs" #-}
                   )
              _lhsOtycons =
                  ({-# LINE 92 "AG\\Typing.ag" #-}
                   Prog
                   {-# LINE 361 "Ag.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 88 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 366 "Ag.hs" #-}
                   )
          in  ( _lhsOty,_lhsOtyErrs,_lhsOtycons)))
sem_Diag__Platform :: Ident ->
                      T_Diag_
sem_Diag__Platform m_ =
    (\ _lhsIpos
       _lhsIrecurs ->
         (let _lhsOty :: Ty
              _lhsOtycons :: TyCons
              _lhsOtyErrs :: ([TyErr])
              _lhsOty =
                  ({-# LINE 97 "AG\\Typing.ag" #-}
                   Ty PlatF Nothing Nothing (Just m_)
                   {-# LINE 380 "Ag.hs" #-}
                   )
              _lhsOtycons =
                  ({-# LINE 98 "AG\\Typing.ag" #-}
                   PlatF
                   {-# LINE 385 "Ag.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 88 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 390 "Ag.hs" #-}
                   )
          in  ( _lhsOty,_lhsOtyErrs,_lhsOtycons)))
sem_Diag__Interpreter :: Ident ->
                         Ident ->
                         Ident ->
                         T_Diag_
sem_Diag__Interpreter i_ l_ m_ =
    (\ _lhsIpos
       _lhsIrecurs ->
         (let _lhsOty :: Ty
              _lhsOtycons :: TyCons
              _lhsOtyErrs :: ([TyErr])
              _lhsOty =
                  ({-# LINE 93 "AG\\Typing.ag" #-}
                   Ty Interp (Just l_) Nothing (Just m_)
                   {-# LINE 406 "Ag.hs" #-}
                   )
              _lhsOtycons =
                  ({-# LINE 94 "AG\\Typing.ag" #-}
                   Interp
                   {-# LINE 411 "Ag.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 88 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 416 "Ag.hs" #-}
                   )
          in  ( _lhsOty,_lhsOtyErrs,_lhsOtycons)))
sem_Diag__Compiler :: Ident ->
                      Ident ->
                      Ident ->
                      Ident ->
                      T_Diag_
sem_Diag__Compiler c_ l1_ l2_ m_ =
    (\ _lhsIpos
       _lhsIrecurs ->
         (let _lhsOty :: Ty
              _lhsOtycons :: TyCons
              _lhsOtyErrs :: ([TyErr])
              _lhsOty =
                  ({-# LINE 95 "AG\\Typing.ag" #-}
                   Ty Comp (Just l1_) (Just l2_) (Just m_)
                   {-# LINE 433 "Ag.hs" #-}
                   )
              _lhsOtycons =
                  ({-# LINE 96 "AG\\Typing.ag" #-}
                   Comp
                   {-# LINE 438 "Ag.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 88 "AG\\Typing.ag" #-}
                   []
                   {-# LINE 443 "Ag.hs" #-}
                   )
          in  ( _lhsOty,_lhsOtyErrs,_lhsOtycons)))
sem_Diag__Execute :: T_Diag ->
                     T_Diag ->
                     T_Diag_
sem_Diag__Execute d1_ d2_ =
    (\ _lhsIpos
       _lhsIrecurs ->
         (let _lhsOty :: Ty
              _lhsOtycons :: TyCons
              _d1Orecurs :: Recursive
              _d2Orecurs :: Recursive
              _lhsOtyErrs :: ([TyErr])
              _d1Ipos :: SourcePos
              _d1Ity :: Ty
              _d1ItyErrs :: ([TyErr])
              _d1Itycons :: TyCons
              _d2Ipos :: SourcePos
              _d2Ity :: Ty
              _d2ItyErrs :: ([TyErr])
              _d2Itycons :: TyCons
              _lhsOty =
                  ({-# LINE 99 "AG\\Typing.ag" #-}
                   _d2Ity
                   {-# LINE 468 "Ag.hs" #-}
                   )
              _lhsOtycons =
                  ({-# LINE 100 "AG\\Typing.ag" #-}
                   Executed
                   {-# LINE 473 "Ag.hs" #-}
                   )
              _d1Orecurs =
                  ({-# LINE 101 "AG\\Typing.ag" #-}
                   Not_recursive
                   {-# LINE 478 "Ag.hs" #-}
                   )
              _d2Orecurs =
                  ({-# LINE 102 "AG\\Typing.ag" #-}
                   Not_recursive
                   {-# LINE 483 "Ag.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 112 "AG\\Typing.ag" #-}
                   _d1ItyErrs ++ _d2ItyErrs ++
                   checkRunnable _d1Ipos (cons _d1Ity) ++
                   checkFramework _d2Ipos (cons _d2Ity) ++
                   checkIfMatches _d1Ipos _d1Ity _d2Ity ++
                   checkExeOrCompile _d2Ipos _d2Itycons
                   {-# LINE 492 "Ag.hs" #-}
                   )
              ( _d1Ipos,_d1Ity,_d1ItyErrs,_d1Itycons) =
                  d1_ _d1Orecurs
              ( _d2Ipos,_d2Ity,_d2ItyErrs,_d2Itycons) =
                  d2_ _d2Orecurs
          in  ( _lhsOty,_lhsOtyErrs,_lhsOtycons)))
sem_Diag__Compile :: T_Diag ->
                     T_Diag ->
                     T_Diag_
sem_Diag__Compile d1_ d2_ =
    (\ _lhsIpos
       _lhsIrecurs ->
         (let _lhsOty :: Ty
              _lhsOtycons :: TyCons
              _d1Orecurs :: Recursive
              _d2Orecurs :: Recursive
              _lhsOtyErrs :: ([TyErr])
              _d1Ipos :: SourcePos
              _d1Ity :: Ty
              _d1ItyErrs :: ([TyErr])
              _d1Itycons :: TyCons
              _d2Ipos :: SourcePos
              _d2Ity :: Ty
              _d2ItyErrs :: ([TyErr])
              _d2Itycons :: TyCons
              _lhsOty =
                  ({-# LINE 103 "AG\\Typing.ag" #-}
                   translate _d1Ity _d2Ity
                   {-# LINE 521 "Ag.hs" #-}
                   )
              _lhsOtycons =
                  ({-# LINE 104 "AG\\Typing.ag" #-}
                   Compiled
                   {-# LINE 526 "Ag.hs" #-}
                   )
              _d1r =
                  ({-# LINE 105 "AG\\Typing.ag" #-}
                   rightRecursion _d1Itycons
                   {-# LINE 531 "Ag.hs" #-}
                   )
              _d2r =
                  ({-# LINE 106 "AG\\Typing.ag" #-}
                   leftRecursion _d2Itycons
                   {-# LINE 536 "Ag.hs" #-}
                   )
              _d1Orecurs =
                  ({-# LINE 107 "AG\\Typing.ag" #-}
                   _d1r
                   {-# LINE 541 "Ag.hs" #-}
                   )
              _d2Orecurs =
                  ({-# LINE 108 "AG\\Typing.ag" #-}
                   _d2r
                   {-# LINE 546 "Ag.hs" #-}
                   )
              _lhsOtyErrs =
                  ({-# LINE 140 "AG\\Typing.ag" #-}
                   _d1ItyErrs  ++ _d2ItyErrs ++
                   checkRunnable  _d1Ipos (cons _d1Ity) ++
                   checkComp      _d2Ipos (cons _d2Ity) ++
                   checkIfMatches _d1Ipos _d1Ity _d2Ity ++
                   checkExeInCompile _d1Ipos _d1Itycons ++
                   checkExeInCompile _d2Ipos _d2Itycons ++
                   checkLandRrecurs _lhsIpos _d1r     _d2r     ++
                   checkLeftRightRecurs _d1Ipos _d1r     _lhsIrecurs ++
                   checkLeftRightRecurs _d2Ipos _d2r     _lhsIrecurs
                   {-# LINE 559 "Ag.hs" #-}
                   )
              ( _d1Ipos,_d1Ity,_d1ItyErrs,_d1Itycons) =
                  d1_ _d1Orecurs
              ( _d2Ipos,_d2Ity,_d2ItyErrs,_d2Itycons) =
                  d2_ _d2Orecurs
          in  ( _lhsOty,_lhsOtyErrs,_lhsOtycons)))