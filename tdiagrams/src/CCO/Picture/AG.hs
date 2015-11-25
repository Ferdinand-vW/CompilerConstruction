

-- UUAGC 0.9.52.1 (Ag.ag)
module CCO.Picture.AG where

{-# LINE 2 "AG\\Printing.ag" #-}

import CCO.Printing
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

{-# LINE 2 "AG\\Base.ag" #-}

import CCO.Tree             (ATerm (App), Tree (fromTree, toTree))
import CCO.Tree.Parser      (parseTree, app, arg)
import Control.Applicative  (Applicative ((<*>)), (<$>))
{-# LINE 28 "Ag.hs" #-}
{-# LINE 34 "AG\\Printing.ag" #-}

ppCall :: Show a => String -> (a, a) -> Doc -> Doc
ppCall cmd args body = singleLine >//< multiLine
  where
    prefix     = text ('\\' : cmd) >|< ppPair args
    singleLine = prefix >|<  braces body
    multiLine  = prefix >|< lbrace >|< text "%" >-< indent 2 body >|< rbrace

ppPair :: Show a => (a, a) -> Doc
ppPair (i, j) = parens (showable i >|< comma >|< showable j)
{-# LINE 40 "Ag.hs" #-}

{-# LINE 9 "AG\\Translate.ag" #-}

type Pos = (Double,Double)
type Ejoint = (Double, Double)
type Cjoint = (Double, Double)
type Height = Double
type Length = Double
type TotalLength = Double
type BlockLength = Double
type CompLength = Double

data DiagInfo = D {cons :: DiagCons, h :: Height, len :: Length, clen :: CompLength,
                   ejoint :: Ejoint, cjoint :: Cjoint}

{-# LINE 56 "Ag.hs" #-}

{-# LINE 109 "AG\\Translate.ag" #-}



compilationPos :: Diag -> Pos -> TotalLength -> CompLength -> Pos
compilationPos (Diag _ (Compiler _ _ _ _)) (x,y) _ _= (x,y)
compilationPos (Diag _ (Compile _ _)) (x,y) tlen clen = (x + tlen - 150 - clen,y + 20)

compileWithPos :: Diag -> Pos -> CompLength -> BlockLength -> Pos
compileWithPos (Diag _ (Compiler _ _ _ _)) (x,y) clen blen = cJoint Comp (x,y) blen
compileWithPos (Diag _ (Compile _ _)) (x,y) _ _ = (x,y)

mapTuple :: (a -> b) -> (a,a) -> (b,b)
mapTuple f (a1,a2) = (f a1, f a2)

sumTuple :: Num a => (a,a) -> (a,a) -> (a,a)
sumTuple (a1,a2) (b1,b2) = (a1 + b1, a2 + b2)

diagInfo :: DiagCons -> BlockLength -> DiagInfo
diagInfo Plat   blen = D Plat   30 50  50  (0   ,0  ) (       0      ,   0)
diagInfo Interp blen = D Interp 30 50  50  (0   ,-30) (    blen      , -20)
diagInfo Comp   blen = D Comp   30 150 100 (blen,-30) (2 * blen      , -20)
diagInfo prog   blen = D Prog   30 50  50  (7.5 ,-30) (    blen + 7.5, -20)

translatePos :: DiagInfo -> Pos
translatePos (D Comp _ _ _ _ _) = (0,0)
translatePos (D cons _ _ _ _ _) = (0,0)

translateDiagInfo :: DiagInfo -> Pos -> DiagInfo
translateDiagInfo (D c h l cl (ex,ey) (cx,cy)) (x,y) = 
    D c h l cl (ex + (x), ey + (y )) (cx + (x), cy + (y))

type Position = (Double,Double)

compLen :: Diag_ -> Double -> Double
compLen (Compiler _ _ _ _) len = len - 50
compLen _ len = len

wordLength :: String -> Double
wordLength s = fromIntegral (length s) * 5

maxLength :: Diag_ -> Double
maxLength (Platform m) = wordLength m
maxLength (Program p l) = max (wordLength p) (wordLength l)
maxLength (Interpreter i l m) = maximum [wordLength i, wordLength l, wordLength m]
maxLength (Compiler c l1 l2 m) = maximum [wordLength c, wordLength l1, wordLength l2, wordLength m]

dimension :: Diag_ -> Double -> (Double, Double)
dimension (Platform _) len = (len, 30)
dimension (Program _ _) len = (len + 15, 30)
dimension (Interpreter _ _ _) len = (len, 30)
dimension (Compiler _ _ _ _) len = (len * 3, 30)
dimension _ _ = (0,0)

joinDimensions :: Diag_  -> (Double, Double) -> (Double, Double) -> (Double, Double)
joinDimensions (Execute _ _) (x1,y1) (x2,y2) = (max x1 x2, y1 + y2)
joinDimensions (Compile _ _) (x1,y1) (x2,y2) = (x1 + x2, y1 + y2 - 10)

--These are necessary, because we need to determine the ejoint and cjoint of the compiled
--diag, but as we then do not know what diag it is we have to do pattern matching.
eJoint :: DiagCons -> Pos -> BlockLength -> Ejoint
eJoint Prog   (x,y) _    = (x + 7.5 , y - 30)
eJoint Interp (x,y) _    = (x       , y - 30)
eJoint Comp   (x,y) blen = (x + blen, y - 30)

cJoint :: DiagCons -> Pos -> BlockLength -> Cjoint
cJoint Prog   (x,y) blen = (x +      blen + 7.5, y - 20)
cJoint Interp (x,y) blen = (x +      blen      , y - 20)
cJoint Comp   (x,y) blen = (x + (2 * blen)     , y - 20)

rightPos :: Diag_ -> Position -> Double -> Position
rightPos (Program _ _) (x, y) len = (x + (len * 4), y)
rightPos (Interpreter _ _ _) (x, y) len = (x + (len * 4), y)
rightPos (Compiler _ _ _ _) (x, y) len = (x + (len * 4), y)

dtd_ :: Diag -> Diag_
dtd_ (Diag p d) = d

--First we still have to do some translation
compile :: Diag_ -> Diag_ -> Diag_
compile dg (Compile d1 d2) = compile dg (compile (dtd_ d1) (dtd_ d2))
compile (Program p _) (Compiler _ _ t _) = Program p t
compile (Interpreter i l _) (Compiler _ _ t _) = Interpreter i l t
compile (Compiler c l1 l2 _) (Compiler _ _ t _) = Compiler c l1 l2 t
compile (Execute d1 d2) dg = compile (dtd_ d2) dg
compile (Compile d1 d2) dg = compile (compile (dtd_ d1) (dtd_ d2)) dg
compile d1 _ = d1

--Given the translated diag and a position, convert it to a list of commands
compiled :: Diag_ -> (Double, Double) -> Double -> [Command]
compiled (Program p l) pos len = program pos len p l
compiled (Interpreter i l m) pos len = interpreter pos len i l m
compiled (Compiler c l1 l2 t) pos len = compiler pos len c l1 l2 t
compiled _ _ _= []

platform :: (Double, Double) -> Double -> String -> [Command]
platform (x,y) len m = [
            Put (x, y - 15) $ Line (5, -3) 25,
            Put (x + len / 2,y - 30) $ Line (5, 3) 25,
            Put (x, y - 15) $ Line (0, 1) 15,
            Put (x, y)  $ Line (1, 0) 50,
            Put (x + len, y) $ Line (0,-1) 15,
            Put (x, y - 15) $ Makebox (50, 15) m
        ]

program ::  (Double, Double) -> Double -> String -> String -> [Command]
program (x,y) len p l = [
                Put (x + 7.5, y - 30) $ Line (1,0) len,
                Put (x + 7.5, y - 30) $ Line (0,1) 15,
                Put (x + 7.5, y - 15) $ Line (-1, 2) 7.5,
                Put (x + len + 7.5, y - 15) $ Line (1, 2) 7.5,
                Put (x + len + 7.5, y - 30) $ Line (0,1) 15,
                Put (x, y) $ Line (1,0) (len + 15),
                Put (x + 7.5, y - 15) $ Makebox (len, 15) p,
                Put (x + 7.5, y - 30) $ Makebox (len, 15) l
            ]


interpreter :: (Double, Double) -> Double -> String -> String -> String -> [Command]
interpreter (x,y) len i l m = [ 
                        Put (x, y - 30) $ Framebox (len, 30) "", 
                        Put (x, y - 10) $ Makebox (len, 10) i,
                        Put (x, y - 20) $ Makebox (len, 10) l,
                        Put (x, y - 30) $ Makebox (len,10) m
                    ]

compiler :: (Double, Double) -> Double -> String -> String -> String -> String -> [Command] 
compiler (x,y) len c l1 l2 m = [
                        Put (x + len, y - 30) $ Line (0, 1) 20,
                        Put (x + len, y - 10) $ Line (-1, 0) len,
                        Put (x, y - 10) $ Line (0, 1) 10,
                        Put (x, y) $ Line (1, 0) (3 * len),
                        Put (x + (3 * len), y) $ Line (0,-1) 10,
                        Put (x + (3 * len), y - 10) $ Line (-1,0) len,
                        Put (x + (2 * len), y - 10) $ Line (0,-1) 20,
                        Put (x + (2 * len), y - 30) $ Line (-1, 0) len,
                        Put (x, y - 10) $ Makebox (len,10) l1,
                        Put (x + len, y - 10) $ Makebox (len,10) "$\\longrightarrow$",
                        Put (x + (2 * len), y - 10) $ Makebox (len,10) l2,
                        Put (x + len, y - 20) $ Makebox (len,10) c,
                        Put (x + len, y - 30) $ Makebox (len,10) m
                    ]


{-# LINE 202 "Ag.hs" #-}

{-# LINE 16 "..\\Diag.ag" #-}

type Ident = String
{-# LINE 207 "Ag.hs" #-}

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
{-# LINE 233 "Ag.hs" #-}

{-# LINE 29 "AG\\Base.ag" #-}

instance Tree Object where
  fromTree (Line s l)     = App "Line"     [fromTree s, fromTree l]
  fromTree (Makebox d b)  = App "Makebox"  [fromTree d, fromTree b]
  fromTree (Framebox d b) = App "Framebox" [fromTree d, fromTree b]

  toTree = parseTree [ app "Line"     (Line     <$> arg <*> arg)
                     , app "Makebox"  (Makebox  <$> arg <*> arg)
                     , app "Framebox" (Framebox <$> arg <*> arg)
                     ]

instance Tree Command where
  fromTree (Put p o) = App "Put" [fromTree p, fromTree o]
  toTree = parseTree [app "Put" (Put <$> arg <*> arg)]

instance Tree Picture where
  fromTree (Picture d cs) = App "Picture" [fromTree d, fromTree cs]
  toTree = parseTree [app "Picture" (Picture <$> arg <*> arg)]
{-# LINE 254 "Ag.hs" #-}
-- Command -----------------------------------------------------
data Command = Put (((Double, Double))) (Object)
-- cata
sem_Command :: Command ->
               T_Command
sem_Command (Put _pos _obj) =
    (sem_Command_Put _pos (sem_Object _obj))
-- semantic domain
type T_Command = ( Doc)
data Inh_Command = Inh_Command {}
data Syn_Command = Syn_Command {pp_Syn_Command :: Doc}
wrap_Command :: T_Command ->
                Inh_Command ->
                Syn_Command
wrap_Command sem (Inh_Command) =
    (let ( _lhsOpp) = sem
     in  (Syn_Command _lhsOpp))
sem_Command_Put :: ((Double, Double)) ->
                   T_Object ->
                   T_Command
sem_Command_Put pos_ obj_ =
    (let _lhsOpp :: Doc
         _objIpp :: Doc
         _lhsOpp =
             ({-# LINE 19 "AG\\Printing.ag" #-}
              ppCall "put" pos_ _objIpp
              {-# LINE 281 "Ag.hs" #-}
              )
         ( _objIpp) =
             obj_
     in  ( _lhsOpp))
-- Commands ----------------------------------------------------
type Commands = [Command]
-- cata
sem_Commands :: Commands ->
                T_Commands
sem_Commands list =
    (Prelude.foldr sem_Commands_Cons sem_Commands_Nil (Prelude.map sem_Command list))
-- semantic domain
type T_Commands = ( Doc)
data Inh_Commands = Inh_Commands {}
data Syn_Commands = Syn_Commands {pp_Syn_Commands :: Doc}
wrap_Commands :: T_Commands ->
                 Inh_Commands ->
                 Syn_Commands
wrap_Commands sem (Inh_Commands) =
    (let ( _lhsOpp) = sem
     in  (Syn_Commands _lhsOpp))
sem_Commands_Cons :: T_Command ->
                     T_Commands ->
                     T_Commands
sem_Commands_Cons hd_ tl_ =
    (let _lhsOpp :: Doc
         _hdIpp :: Doc
         _tlIpp :: Doc
         _lhsOpp =
             ({-# LINE 23 "AG\\Printing.ag" #-}
              _hdIpp >-< _tlIpp
              {-# LINE 313 "Ag.hs" #-}
              )
         ( _hdIpp) =
             hd_
         ( _tlIpp) =
             tl_
     in  ( _lhsOpp))
sem_Commands_Nil :: T_Commands
sem_Commands_Nil =
    (let _lhsOpp :: Doc
         _lhsOpp =
             ({-# LINE 22 "AG\\Printing.ag" #-}
              empty
              {-# LINE 326 "Ag.hs" #-}
              )
     in  ( _lhsOpp))
-- Diag --------------------------------------------------------
data Diag = Diag (SourcePos) (Diag_)
-- cata
sem_Diag :: Diag ->
            T_Diag
sem_Diag (Diag _pos _d) =
    (sem_Diag_Diag _pos (sem_Diag_ _d))
-- semantic domain
type T_Diag = ((Double)) ->
              ((Double,Double)) ->
              ( ([Command]),Diag,((Diag_)),((Double,Double)),((DiagInfo)),Double,((Double)),((Double)))
data Inh_Diag = Inh_Diag {blen_Inh_Diag :: ((Double)),pos_Inh_Diag :: ((Double,Double))}
data Syn_Diag = Syn_Diag {cmd_Syn_Diag :: ([Command]),diag_Syn_Diag :: Diag,diag'_Syn_Diag :: ((Diag_)),dim_Syn_Diag :: ((Double,Double)),dinfo_Syn_Diag :: ((DiagInfo)),h_Syn_Diag :: Double,mlen_Syn_Diag :: ((Double)),tlen_Syn_Diag :: ((Double))}
wrap_Diag :: T_Diag ->
             Inh_Diag ->
             Syn_Diag
wrap_Diag sem (Inh_Diag _lhsIblen _lhsIpos) =
    (let ( _lhsOcmd,_lhsOdiag,_lhsOdiag',_lhsOdim,_lhsOdinfo,_lhsOh,_lhsOmlen,_lhsOtlen) = sem _lhsIblen _lhsIpos
     in  (Syn_Diag _lhsOcmd _lhsOdiag _lhsOdiag' _lhsOdim _lhsOdinfo _lhsOh _lhsOmlen _lhsOtlen))
sem_Diag_Diag :: SourcePos ->
                 T_Diag_ ->
                 T_Diag
sem_Diag_Diag pos_ d_ =
    (\ _lhsIblen
       _lhsIpos ->
         (let _lhsOcmd :: ([Command])
              _lhsOdiag :: Diag
              _lhsOdiag' :: ((Diag_))
              _lhsOdim :: ((Double,Double))
              _lhsOdinfo :: ((DiagInfo))
              _lhsOh :: Double
              _lhsOmlen :: ((Double))
              _lhsOtlen :: ((Double))
              _dOblen :: ((Double))
              _dOpos :: ((Double,Double))
              _dIcmd :: ([Command])
              _dIdiag :: Diag_
              _dIdiag' :: ((Diag_))
              _dIdim :: ((Double,Double))
              _dIdinfo :: ((DiagInfo))
              _dIh :: Double
              _dImlen :: ((Double))
              _dItlen :: ((Double))
              _lhsOcmd =
                  ({-# LINE 41 "AG\\Translate.ag" #-}
                   _dIcmd
                   {-# LINE 375 "Ag.hs" #-}
                   )
              _diag =
                  ({-# LINE 39 "AG\\Translate.ag" #-}
                   Diag pos_ _dIdiag
                   {-# LINE 380 "Ag.hs" #-}
                   )
              _lhsOdiag =
                  ({-# LINE 39 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 385 "Ag.hs" #-}
                   )
              _lhsOdiag' =
                  ({-# LINE 40 "AG\\Translate.ag" #-}
                   _dIdiag'
                   {-# LINE 390 "Ag.hs" #-}
                   )
              _lhsOdim =
                  ({-# LINE 42 "AG\\Translate.ag" #-}
                   _dIdim
                   {-# LINE 395 "Ag.hs" #-}
                   )
              _lhsOdinfo =
                  ({-# LINE 43 "AG\\Translate.ag" #-}
                   _dIdinfo
                   {-# LINE 400 "Ag.hs" #-}
                   )
              _lhsOh =
                  ({-# LINE 44 "AG\\Translate.ag" #-}
                   _dIh
                   {-# LINE 405 "Ag.hs" #-}
                   )
              _lhsOmlen =
                  ({-# LINE 46 "AG\\Translate.ag" #-}
                   _dImlen
                   {-# LINE 410 "Ag.hs" #-}
                   )
              _lhsOtlen =
                  ({-# LINE 45 "AG\\Translate.ag" #-}
                   _dItlen
                   {-# LINE 415 "Ag.hs" #-}
                   )
              _dOblen =
                  ({-# LINE 47 "AG\\Translate.ag" #-}
                   _lhsIblen
                   {-# LINE 420 "Ag.hs" #-}
                   )
              _dOpos =
                  ({-# LINE 48 "AG\\Translate.ag" #-}
                   _lhsIpos
                   {-# LINE 425 "Ag.hs" #-}
                   )
              ( _dIcmd,_dIdiag,_dIdiag',_dIdim,_dIdinfo,_dIh,_dImlen,_dItlen) =
                  d_ _dOblen _dOpos
          in  ( _lhsOcmd,_lhsOdiag,_lhsOdiag',_lhsOdim,_lhsOdinfo,_lhsOh,_lhsOmlen,_lhsOtlen)))
-- DiagCons ----------------------------------------------------
data DiagCons = Plat
              | Interp
              | Comp
              | Prog
-- cata
sem_DiagCons :: DiagCons ->
                T_DiagCons
sem_DiagCons (Plat) =
    (sem_DiagCons_Plat)
sem_DiagCons (Interp) =
    (sem_DiagCons_Interp)
sem_DiagCons (Comp) =
    (sem_DiagCons_Comp)
sem_DiagCons (Prog) =
    (sem_DiagCons_Prog)
-- semantic domain
type T_DiagCons = ( )
data Inh_DiagCons = Inh_DiagCons {}
data Syn_DiagCons = Syn_DiagCons {}
wrap_DiagCons :: T_DiagCons ->
                 Inh_DiagCons ->
                 Syn_DiagCons
wrap_DiagCons sem (Inh_DiagCons) =
    (let ( ) = sem
     in  (Syn_DiagCons))
sem_DiagCons_Plat :: T_DiagCons
sem_DiagCons_Plat =
    (let
     in  ( ))
sem_DiagCons_Interp :: T_DiagCons
sem_DiagCons_Interp =
    (let
     in  ( ))
sem_DiagCons_Comp :: T_DiagCons
sem_DiagCons_Comp =
    (let
     in  ( ))
sem_DiagCons_Prog :: T_DiagCons
sem_DiagCons_Prog =
    (let
     in  ( ))
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
type T_Diag_ = ((Double)) ->
               ((Double,Double)) ->
               ( ([Command]),Diag_,((Diag_)),((Double,Double)),((DiagInfo)),Double,((Double)),((Double)))
data Inh_Diag_ = Inh_Diag_ {blen_Inh_Diag_ :: ((Double)),pos_Inh_Diag_ :: ((Double,Double))}
data Syn_Diag_ = Syn_Diag_ {cmd_Syn_Diag_ :: ([Command]),diag_Syn_Diag_ :: Diag_,diag'_Syn_Diag_ :: ((Diag_)),dim_Syn_Diag_ :: ((Double,Double)),dinfo_Syn_Diag_ :: ((DiagInfo)),h_Syn_Diag_ :: Double,mlen_Syn_Diag_ :: ((Double)),tlen_Syn_Diag_ :: ((Double))}
wrap_Diag_ :: T_Diag_ ->
              Inh_Diag_ ->
              Syn_Diag_
wrap_Diag_ sem (Inh_Diag_ _lhsIblen _lhsIpos) =
    (let ( _lhsOcmd,_lhsOdiag,_lhsOdiag',_lhsOdim,_lhsOdinfo,_lhsOh,_lhsOmlen,_lhsOtlen) = sem _lhsIblen _lhsIpos
     in  (Syn_Diag_ _lhsOcmd _lhsOdiag _lhsOdiag' _lhsOdim _lhsOdinfo _lhsOh _lhsOmlen _lhsOtlen))
sem_Diag__Program :: Ident ->
                     Ident ->
                     T_Diag_
sem_Diag__Program p_ l_ =
    (\ _lhsIblen
       _lhsIpos ->
         (let _lhsOcmd :: ([Command])
              _lhsOdiag' :: ((Diag_))
              _lhsOmlen :: ((Double))
              _lhsOh :: Double
              _lhsOtlen :: ((Double))
              _lhsOdim :: ((Double,Double))
              _lhsOdinfo :: ((DiagInfo))
              _lhsOdiag :: Diag_
              _lhsOcmd =
                  ({-# LINE 59 "AG\\Translate.ag" #-}
                   program _lhsIpos _lhsIblen p_ l_
                   {-# LINE 523 "Ag.hs" #-}
                   )
              _lhsOdiag' =
                  ({-# LINE 60 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 528 "Ag.hs" #-}
                   )
              _lhsOmlen =
                  ({-# LINE 61 "AG\\Translate.ag" #-}
                   maxLength _diag
                   {-# LINE 533 "Ag.hs" #-}
                   )
              _lhsOh =
                  ({-# LINE 62 "AG\\Translate.ag" #-}
                   30
                   {-# LINE 538 "Ag.hs" #-}
                   )
              _lhsOtlen =
                  ({-# LINE 63 "AG\\Translate.ag" #-}
                   50
                   {-# LINE 543 "Ag.hs" #-}
                   )
              _lhsOdim =
                  ({-# LINE 64 "AG\\Translate.ag" #-}
                   dimension _diag _lhsIblen
                   {-# LINE 548 "Ag.hs" #-}
                   )
              _lhsOdinfo =
                  ({-# LINE 65 "AG\\Translate.ag" #-}
                   diagInfo Prog _lhsIblen
                   {-# LINE 553 "Ag.hs" #-}
                   )
              _diag =
                  ({-# LINE 39 "AG\\Translate.ag" #-}
                   Program p_ l_
                   {-# LINE 558 "Ag.hs" #-}
                   )
              _lhsOdiag =
                  ({-# LINE 39 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 563 "Ag.hs" #-}
                   )
          in  ( _lhsOcmd,_lhsOdiag,_lhsOdiag',_lhsOdim,_lhsOdinfo,_lhsOh,_lhsOmlen,_lhsOtlen)))
sem_Diag__Platform :: Ident ->
                      T_Diag_
sem_Diag__Platform m_ =
    (\ _lhsIblen
       _lhsIpos ->
         (let _lhsOcmd :: ([Command])
              _lhsOdiag' :: ((Diag_))
              _lhsOmlen :: ((Double))
              _lhsOh :: Double
              _lhsOtlen :: ((Double))
              _lhsOdim :: ((Double,Double))
              _lhsOdinfo :: ((DiagInfo))
              _lhsOdiag :: Diag_
              _lhsOcmd =
                  ({-# LINE 52 "AG\\Translate.ag" #-}
                   platform _lhsIpos _lhsIblen m_
                   {-# LINE 582 "Ag.hs" #-}
                   )
              _lhsOdiag' =
                  ({-# LINE 53 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 587 "Ag.hs" #-}
                   )
              _lhsOmlen =
                  ({-# LINE 54 "AG\\Translate.ag" #-}
                   maxLength _diag
                   {-# LINE 592 "Ag.hs" #-}
                   )
              _lhsOh =
                  ({-# LINE 55 "AG\\Translate.ag" #-}
                   30
                   {-# LINE 597 "Ag.hs" #-}
                   )
              _lhsOtlen =
                  ({-# LINE 56 "AG\\Translate.ag" #-}
                   50
                   {-# LINE 602 "Ag.hs" #-}
                   )
              _lhsOdim =
                  ({-# LINE 57 "AG\\Translate.ag" #-}
                   dimension _diag _lhsIblen
                   {-# LINE 607 "Ag.hs" #-}
                   )
              _lhsOdinfo =
                  ({-# LINE 58 "AG\\Translate.ag" #-}
                   diagInfo Plat _lhsIblen
                   {-# LINE 612 "Ag.hs" #-}
                   )
              _diag =
                  ({-# LINE 39 "AG\\Translate.ag" #-}
                   Platform m_
                   {-# LINE 617 "Ag.hs" #-}
                   )
              _lhsOdiag =
                  ({-# LINE 39 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 622 "Ag.hs" #-}
                   )
          in  ( _lhsOcmd,_lhsOdiag,_lhsOdiag',_lhsOdim,_lhsOdinfo,_lhsOh,_lhsOmlen,_lhsOtlen)))
sem_Diag__Interpreter :: Ident ->
                         Ident ->
                         Ident ->
                         T_Diag_
sem_Diag__Interpreter i_ l_ m_ =
    (\ _lhsIblen
       _lhsIpos ->
         (let _lhsOcmd :: ([Command])
              _lhsOdiag' :: ((Diag_))
              _lhsOmlen :: ((Double))
              _lhsOh :: Double
              _lhsOtlen :: ((Double))
              _lhsOdim :: ((Double,Double))
              _lhsOdinfo :: ((DiagInfo))
              _lhsOdiag :: Diag_
              _lhsOcmd =
                  ({-# LINE 66 "AG\\Translate.ag" #-}
                   interpreter _lhsIpos _lhsIblen i_ l_ m_
                   {-# LINE 643 "Ag.hs" #-}
                   )
              _lhsOdiag' =
                  ({-# LINE 67 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 648 "Ag.hs" #-}
                   )
              _lhsOmlen =
                  ({-# LINE 68 "AG\\Translate.ag" #-}
                   maxLength _diag
                   {-# LINE 653 "Ag.hs" #-}
                   )
              _lhsOh =
                  ({-# LINE 69 "AG\\Translate.ag" #-}
                   30
                   {-# LINE 658 "Ag.hs" #-}
                   )
              _lhsOtlen =
                  ({-# LINE 70 "AG\\Translate.ag" #-}
                   50
                   {-# LINE 663 "Ag.hs" #-}
                   )
              _lhsOdim =
                  ({-# LINE 71 "AG\\Translate.ag" #-}
                   dimension _diag _lhsIblen
                   {-# LINE 668 "Ag.hs" #-}
                   )
              _lhsOdinfo =
                  ({-# LINE 72 "AG\\Translate.ag" #-}
                   diagInfo Interp _lhsIblen
                   {-# LINE 673 "Ag.hs" #-}
                   )
              _diag =
                  ({-# LINE 39 "AG\\Translate.ag" #-}
                   Interpreter i_ l_ m_
                   {-# LINE 678 "Ag.hs" #-}
                   )
              _lhsOdiag =
                  ({-# LINE 39 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 683 "Ag.hs" #-}
                   )
          in  ( _lhsOcmd,_lhsOdiag,_lhsOdiag',_lhsOdim,_lhsOdinfo,_lhsOh,_lhsOmlen,_lhsOtlen)))
sem_Diag__Compiler :: Ident ->
                      Ident ->
                      Ident ->
                      Ident ->
                      T_Diag_
sem_Diag__Compiler c_ l1_ l2_ m_ =
    (\ _lhsIblen
       _lhsIpos ->
         (let _lhsOcmd :: ([Command])
              _lhsOdiag' :: ((Diag_))
              _lhsOmlen :: ((Double))
              _lhsOh :: Double
              _lhsOtlen :: ((Double))
              _lhsOdim :: ((Double,Double))
              _lhsOdinfo :: ((DiagInfo))
              _lhsOdiag :: Diag_
              _lhsOcmd =
                  ({-# LINE 73 "AG\\Translate.ag" #-}
                   compiler _lhsIpos _lhsIblen c_ l1_ l2_ m_
                   {-# LINE 705 "Ag.hs" #-}
                   )
              _lhsOdiag' =
                  ({-# LINE 74 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 710 "Ag.hs" #-}
                   )
              _lhsOmlen =
                  ({-# LINE 75 "AG\\Translate.ag" #-}
                   maxLength _diag
                   {-# LINE 715 "Ag.hs" #-}
                   )
              _lhsOh =
                  ({-# LINE 76 "AG\\Translate.ag" #-}
                   30
                   {-# LINE 720 "Ag.hs" #-}
                   )
              _lhsOtlen =
                  ({-# LINE 77 "AG\\Translate.ag" #-}
                   150
                   {-# LINE 725 "Ag.hs" #-}
                   )
              _lhsOdim =
                  ({-# LINE 78 "AG\\Translate.ag" #-}
                   dimension _diag _lhsIblen
                   {-# LINE 730 "Ag.hs" #-}
                   )
              _lhsOdinfo =
                  ({-# LINE 79 "AG\\Translate.ag" #-}
                   diagInfo Comp _lhsIblen
                   {-# LINE 735 "Ag.hs" #-}
                   )
              _diag =
                  ({-# LINE 39 "AG\\Translate.ag" #-}
                   Compiler c_ l1_ l2_ m_
                   {-# LINE 740 "Ag.hs" #-}
                   )
              _lhsOdiag =
                  ({-# LINE 39 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 745 "Ag.hs" #-}
                   )
          in  ( _lhsOcmd,_lhsOdiag,_lhsOdiag',_lhsOdim,_lhsOdinfo,_lhsOh,_lhsOmlen,_lhsOtlen)))
sem_Diag__Execute :: T_Diag ->
                     T_Diag ->
                     T_Diag_
sem_Diag__Execute d1_ d2_ =
    (\ _lhsIblen
       _lhsIpos ->
         (let _lhsOcmd :: ([Command])
              _lhsOdiag' :: ((Diag_))
              _d1Opos :: ((Double,Double))
              _d2Opos :: ((Double,Double))
              _lhsOmlen :: ((Double))
              _d1Oblen :: ((Double))
              _d2Oblen :: ((Double))
              _lhsOh :: Double
              _lhsOtlen :: ((Double))
              _lhsOdim :: ((Double,Double))
              _lhsOdinfo :: ((DiagInfo))
              _lhsOdiag :: Diag_
              _d1Icmd :: ([Command])
              _d1Idiag :: Diag
              _d1Idiag' :: ((Diag_))
              _d1Idim :: ((Double,Double))
              _d1Idinfo :: ((DiagInfo))
              _d1Ih :: Double
              _d1Imlen :: ((Double))
              _d1Itlen :: ((Double))
              _d2Icmd :: ([Command])
              _d2Idiag :: Diag
              _d2Idiag' :: ((Diag_))
              _d2Idim :: ((Double,Double))
              _d2Idinfo :: ((DiagInfo))
              _d2Ih :: Double
              _d2Imlen :: ((Double))
              _d2Itlen :: ((Double))
              _lhsOcmd =
                  ({-# LINE 80 "AG\\Translate.ag" #-}
                   _d1Icmd ++ _d2Icmd
                   {-# LINE 785 "Ag.hs" #-}
                   )
              _lhsOdiag' =
                  ({-# LINE 81 "AG\\Translate.ag" #-}
                   _d2Idiag'
                   {-# LINE 790 "Ag.hs" #-}
                   )
              _d1Opos =
                  ({-# LINE 82 "AG\\Translate.ag" #-}
                   _lhsIpos
                   {-# LINE 795 "Ag.hs" #-}
                   )
              _d2Opos =
                  ({-# LINE 83 "AG\\Translate.ag" #-}
                   sumTuple _lhsIpos (ejoint _d1Idinfo)
                   {-# LINE 800 "Ag.hs" #-}
                   )
              _lhsOmlen =
                  ({-# LINE 84 "AG\\Translate.ag" #-}
                   max _d1Imlen _d2Imlen
                   {-# LINE 805 "Ag.hs" #-}
                   )
              _d1Oblen =
                  ({-# LINE 85 "AG\\Translate.ag" #-}
                   _lhsIblen
                   {-# LINE 810 "Ag.hs" #-}
                   )
              _d2Oblen =
                  ({-# LINE 86 "AG\\Translate.ag" #-}
                   _lhsIblen
                   {-# LINE 815 "Ag.hs" #-}
                   )
              _lhsOh =
                  ({-# LINE 87 "AG\\Translate.ag" #-}
                   _d1Ih + _d2Ih
                   {-# LINE 820 "Ag.hs" #-}
                   )
              _lhsOtlen =
                  ({-# LINE 88 "AG\\Translate.ag" #-}
                   _d1Itlen + _d2Itlen
                   {-# LINE 825 "Ag.hs" #-}
                   )
              _lhsOdim =
                  ({-# LINE 89 "AG\\Translate.ag" #-}
                   joinDimensions _diag _d1Idim _d2Idim
                   {-# LINE 830 "Ag.hs" #-}
                   )
              _lhsOdinfo =
                  ({-# LINE 90 "AG\\Translate.ag" #-}
                   _d2Idinfo
                   {-# LINE 835 "Ag.hs" #-}
                   )
              _diag =
                  ({-# LINE 39 "AG\\Translate.ag" #-}
                   Execute _d1Idiag _d2Idiag
                   {-# LINE 840 "Ag.hs" #-}
                   )
              _lhsOdiag =
                  ({-# LINE 39 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 845 "Ag.hs" #-}
                   )
              ( _d1Icmd,_d1Idiag,_d1Idiag',_d1Idim,_d1Idinfo,_d1Ih,_d1Imlen,_d1Itlen) =
                  d1_ _d1Oblen _d1Opos
              ( _d2Icmd,_d2Idiag,_d2Idiag',_d2Idim,_d2Idinfo,_d2Ih,_d2Imlen,_d2Itlen) =
                  d2_ _d2Oblen _d2Opos
          in  ( _lhsOcmd,_lhsOdiag,_lhsOdiag',_lhsOdim,_lhsOdinfo,_lhsOh,_lhsOmlen,_lhsOtlen)))
sem_Diag__Compile :: T_Diag ->
                     T_Diag ->
                     T_Diag_
sem_Diag__Compile d1_ d2_ =
    (\ _lhsIblen
       _lhsIpos ->
         (let _lhsOcmd :: ([Command])
              _lhsOdiag' :: ((Diag_))
              _d1Opos :: ((Double,Double))
              _d2Opos :: ((Double,Double))
              _lhsOmlen :: ((Double))
              _d1Oblen :: ((Double))
              _d2Oblen :: ((Double))
              _lhsOh :: Double
              _lhsOtlen :: ((Double))
              _lhsOdim :: ((Double,Double))
              _lhsOdinfo :: ((DiagInfo))
              _lhsOdiag :: Diag_
              _d1Icmd :: ([Command])
              _d1Idiag :: Diag
              _d1Idiag' :: ((Diag_))
              _d1Idim :: ((Double,Double))
              _d1Idinfo :: ((DiagInfo))
              _d1Ih :: Double
              _d1Imlen :: ((Double))
              _d1Itlen :: ((Double))
              _d2Icmd :: ([Command])
              _d2Idiag :: Diag
              _d2Idiag' :: ((Diag_))
              _d2Idim :: ((Double,Double))
              _d2Idinfo :: ((DiagInfo))
              _d2Ih :: Double
              _d2Imlen :: ((Double))
              _d2Itlen :: ((Double))
              _cpos =
                  ({-# LINE 91 "AG\\Translate.ag" #-}
                   rightPos _cmpl     _d1pos     _lhsIblen
                   {-# LINE 889 "Ag.hs" #-}
                   )
              _cmpl =
                  ({-# LINE 92 "AG\\Translate.ag" #-}
                   compile (dtd_ _d1Idiag) (dtd_ _d2Idiag)
                   {-# LINE 894 "Ag.hs" #-}
                   )
              _lhsOcmd =
                  ({-# LINE 93 "AG\\Translate.ag" #-}
                   _d1Icmd ++ _d2Icmd ++ compiled _cmpl     _cpos     _lhsIblen
                   {-# LINE 899 "Ag.hs" #-}
                   )
              _lhsOdiag' =
                  ({-# LINE 94 "AG\\Translate.ag" #-}
                   _d1Idiag'
                   {-# LINE 904 "Ag.hs" #-}
                   )
              _d1pos =
                  ({-# LINE 95 "AG\\Translate.ag" #-}
                   compilationPos _d2Idiag _lhsIpos _d2Itlen (clen _d1Idinfo)
                   {-# LINE 909 "Ag.hs" #-}
                   )
              _d1Opos =
                  ({-# LINE 96 "AG\\Translate.ag" #-}
                   _d1pos
                   {-# LINE 914 "Ag.hs" #-}
                   )
              _d2Opos =
                  ({-# LINE 97 "AG\\Translate.ag" #-}
                   compileWithPos _d2Idiag _lhsIpos (clen _d1Idinfo) _lhsIblen
                   {-# LINE 919 "Ag.hs" #-}
                   )
              _lhsOmlen =
                  ({-# LINE 98 "AG\\Translate.ag" #-}
                   max _d1Imlen _d2Imlen
                   {-# LINE 924 "Ag.hs" #-}
                   )
              _d1Oblen =
                  ({-# LINE 99 "AG\\Translate.ag" #-}
                   _lhsIblen
                   {-# LINE 929 "Ag.hs" #-}
                   )
              _d2Oblen =
                  ({-# LINE 100 "AG\\Translate.ag" #-}
                   _lhsIblen
                   {-# LINE 934 "Ag.hs" #-}
                   )
              _h =
                  ({-# LINE 101 "AG\\Translate.ag" #-}
                   _d1Ih + _d2Ih - 10
                   {-# LINE 939 "Ag.hs" #-}
                   )
              _lhsOh =
                  ({-# LINE 102 "AG\\Translate.ag" #-}
                   _d1Ih
                   {-# LINE 944 "Ag.hs" #-}
                   )
              _lhsOtlen =
                  ({-# LINE 103 "AG\\Translate.ag" #-}
                   (2 * clen _d1Idinfo) + _d2Itlen
                   {-# LINE 949 "Ag.hs" #-}
                   )
              _dim =
                  ({-# LINE 104 "AG\\Translate.ag" #-}
                   (joinDimensions _diag (fst _d1Idim, _d1Ih) _d2Idim)
                   {-# LINE 954 "Ag.hs" #-}
                   )
              _lhsOdim =
                  ({-# LINE 105 "AG\\Translate.ag" #-}
                   (fst $ joinDimensions _diag (dimension _cmpl     _lhsIblen) _dim    , snd _dim    )
                   {-# LINE 959 "Ag.hs" #-}
                   )
              _lhsOdinfo =
                  ({-# LINE 106 "AG\\Translate.ag" #-}
                   translateDiagInfo _d1Idinfo (translatePos _d1Idinfo)
                   {-# LINE 964 "Ag.hs" #-}
                   )
              _diag =
                  ({-# LINE 39 "AG\\Translate.ag" #-}
                   Compile _d1Idiag _d2Idiag
                   {-# LINE 969 "Ag.hs" #-}
                   )
              _lhsOdiag =
                  ({-# LINE 39 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 974 "Ag.hs" #-}
                   )
              ( _d1Icmd,_d1Idiag,_d1Idiag',_d1Idim,_d1Idinfo,_d1Ih,_d1Imlen,_d1Itlen) =
                  d1_ _d1Oblen _d1Opos
              ( _d2Icmd,_d2Idiag,_d2Idiag',_d2Idim,_d2Idinfo,_d2Ih,_d2Imlen,_d2Itlen) =
                  d2_ _d2Oblen _d2Opos
          in  ( _lhsOcmd,_lhsOdiag,_lhsOdiag',_lhsOdim,_lhsOdinfo,_lhsOh,_lhsOmlen,_lhsOtlen)))
-- Object ------------------------------------------------------
data Object = Line (((Int, Int))) (Double)
            | Makebox (((Double, Double))) (String)
            | Framebox (((Double, Double))) (String)
-- cata
sem_Object :: Object ->
              T_Object
sem_Object (Line _slope _len) =
    (sem_Object_Line _slope _len)
sem_Object (Makebox _dim _body) =
    (sem_Object_Makebox _dim _body)
sem_Object (Framebox _dim _body) =
    (sem_Object_Framebox _dim _body)
-- semantic domain
type T_Object = ( Doc)
data Inh_Object = Inh_Object {}
data Syn_Object = Syn_Object {pp_Syn_Object :: Doc}
wrap_Object :: T_Object ->
               Inh_Object ->
               Syn_Object
wrap_Object sem (Inh_Object) =
    (let ( _lhsOpp) = sem
     in  (Syn_Object _lhsOpp))
sem_Object_Line :: ((Int, Int)) ->
                   Double ->
                   T_Object
sem_Object_Line slope_ len_ =
    (let _lhsOpp :: Doc
         _lhsOpp =
             ({-# LINE 14 "AG\\Printing.ag" #-}
              ppCall "line"     slope_ (showable len_)
              {-# LINE 1012 "Ag.hs" #-}
              )
     in  ( _lhsOpp))
sem_Object_Makebox :: ((Double, Double)) ->
                      String ->
                      T_Object
sem_Object_Makebox dim_ body_ =
    (let _lhsOpp :: Doc
         _lhsOpp =
             ({-# LINE 15 "AG\\Printing.ag" #-}
              ppCall "makebox"  dim_   (text body_)
              {-# LINE 1023 "Ag.hs" #-}
              )
     in  ( _lhsOpp))
sem_Object_Framebox :: ((Double, Double)) ->
                       String ->
                       T_Object
sem_Object_Framebox dim_ body_ =
    (let _lhsOpp :: Doc
         _lhsOpp =
             ({-# LINE 16 "AG\\Printing.ag" #-}
              ppCall "framebox" dim_   (text body_)
              {-# LINE 1034 "Ag.hs" #-}
              )
     in  ( _lhsOpp))
-- Pic ---------------------------------------------------------
data Pic = Pic (Diag)
-- cata
sem_Pic :: Pic ->
           T_Pic
sem_Pic (Pic _d) =
    (sem_Pic_Pic (sem_Diag _d))
-- semantic domain
type T_Pic = ( Picture)
data Inh_Pic = Inh_Pic {}
data Syn_Pic = Syn_Pic {pic_Syn_Pic :: Picture}
wrap_Pic :: T_Pic ->
            Inh_Pic ->
            Syn_Pic
wrap_Pic sem (Inh_Pic) =
    (let ( _lhsOpic) = sem
     in  (Syn_Pic _lhsOpic))
sem_Pic_Pic :: T_Diag ->
               T_Pic
sem_Pic_Pic d_ =
    (let _lhsOpic :: Picture
         _dOpos :: ((Double,Double))
         _dOblen :: ((Double))
         _dIcmd :: ([Command])
         _dIdiag :: Diag
         _dIdiag' :: ((Diag_))
         _dIdim :: ((Double,Double))
         _dIdinfo :: ((DiagInfo))
         _dIh :: Double
         _dImlen :: ((Double))
         _dItlen :: ((Double))
         _lhsOpic =
             ({-# LINE 33 "AG\\Translate.ag" #-}
              Picture _dIdim _dIcmd
              {-# LINE 1071 "Ag.hs" #-}
              )
         _dOpos =
             ({-# LINE 34 "AG\\Translate.ag" #-}
              (-100,snd _dIdim)
              {-# LINE 1076 "Ag.hs" #-}
              )
         _dOblen =
             ({-# LINE 35 "AG\\Translate.ag" #-}
              max _dImlen 50
              {-# LINE 1081 "Ag.hs" #-}
              )
         ( _dIcmd,_dIdiag,_dIdiag',_dIdim,_dIdinfo,_dIh,_dImlen,_dItlen) =
             d_ _dOblen _dOpos
     in  ( _lhsOpic))
-- Picture -----------------------------------------------------
data Picture = Picture (((Double, Double))) (Commands)
-- cata
sem_Picture :: Picture ->
               T_Picture
sem_Picture (Picture _dim _cmds) =
    (sem_Picture_Picture _dim (sem_Commands _cmds))
-- semantic domain
type T_Picture = ( Doc)
data Inh_Picture = Inh_Picture {}
data Syn_Picture = Syn_Picture {pp_Syn_Picture :: Doc}
wrap_Picture :: T_Picture ->
                Inh_Picture ->
                Syn_Picture
wrap_Picture sem (Inh_Picture) =
    (let ( _lhsOpp) = sem
     in  (Syn_Picture _lhsOpp))
sem_Picture_Picture :: ((Double, Double)) ->
                       T_Commands ->
                       T_Picture
sem_Picture_Picture dim_ cmds_ =
    (let _lhsOpp :: Doc
         _cmdsIpp :: Doc
         _lhsOpp =
             ({-# LINE 26 "AG\\Printing.ag" #-}
              text "\\begin{picture}" >|< ppPair dim_ >-<
              indent 2 _cmdsIpp >-<
              text "\\end{picture}"
              {-# LINE 1114 "Ag.hs" #-}
              )
         ( _cmdsIpp) =
             cmds_
     in  ( _lhsOpp))