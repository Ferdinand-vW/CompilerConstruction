

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
type Depth = Double
type Width = Double
{-# LINE 54 "Ag.hs" #-}

{-# LINE 33 "AG\\Translate.ag" #-}

--We draw a frame around our diagram to show, that everything neatly
--fits in the dimensions. Note the use of '-100' is for the same reason as above
frame :: (Double,Double) -> [Command]
frame (dx,dy) = [
  Put (-100,0) $ Line (1, 0) dx,
  Put (-100,0) $ Line (0, 1) dy,
  Put (dx-100,dy) $ Line (-1,0) dx,
  Put (dx-100,dy) $ Line (0,-1) dy
  ]
{-# LINE 67 "Ag.hs" #-}

{-# LINE 128 "AG\\Translate.ag" #-}


--We have to know whether there is a compile inside the execute
--and if so we have to remove some heigth
executeHeight :: Diag -> Height -> Height -> Height
executeHeight (Diag _ (Compile _ _)) h1 h2 = h1 - 20 + h2
executeHeight _ h1 h2 = h1 + h2

--For each compiling we have to remove the overlap
--between what is being compiled and the compiler
compileHeight :: Depth -> Height -> Height -> Height
compileHeight cdepth d1h d2h
  | cdepth > 0 = d1h
  | otherwise = d1h + d2h - 10

--Every level of left recursion in a compilation tree adds the compLength of a compiler
--to the width. The top level adds it twice. 
compileWidth :: Depth -> DiagCons -> BlockLength -> Width -> Width -> Width
compileWidth wdepth d1c blen d1w d2w
  | wdepth > 0 = d2w + (d1w - actualLength d1c blen + compLength d1c blen)
  | otherwise = d2w + (d1w - actualLength d1c blen + 2 * compLength d1c blen) 

--Determines where diagram d1 should be positioned. We have two cases,
--if we are doing a right recursive compilation, then d1 is the first diagram.
--Otherwise it is not and we have to move it a certain amount to the right
d1Pos :: Depth -> Depth -> Pos -> BlockLength -> DiagCons -> Pos
d1Pos cdepth wdepth (x,y) blen cons
  | cdepth >= wdepth = (x,y)
  | otherwise = (x + (3 * blen) + (wdepth - 1) * (2 * blen) - fittToDiag cons blen,y)

--Determines where diagram d2 should be positioned. We have two cases,
--if we are doing a left recursive compilation, then d2 is the first diagram,
--but also the lowest. Otherwise it should simply be connected to d1 using d1's cjoint
d2Pos :: Depth -> Depth -> Pos -> BlockLength -> Cjoint -> Pos
d2Pos cdepth wdepth (x,y) blen (cx,cy)
  | wdepth > cdepth = (x,(wdepth * 20) + 30)
  | otherwise = (cx,cy)

--We have to move diagrams to the left sometimes
fittToDiag :: DiagCons -> BlockLength -> Double
fittToDiag Prog _ = 7.5
fittToDiag Interp _ = 0
fittToDiag Plat _ = 0
fittToDiag Comp blen = blen

--In compilation the complete length of a diagram is possibly not used
compLength :: DiagCons -> BlockLength -> Double
compLength Prog blen = blen + 7.5
compLength Interp blen = blen
compLength Comp blen = blen * 2
compLength Plat blen = blen

actualLength :: DiagCons -> BlockLength -> Double
actualLength Prog blen = blen + 15
actualLength Interp blen = blen
actualLength Comp blen = blen * 3
actualLength Plat blen = blen 

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

--Simply moves diagrams to the right of a compiler
rightPos :: DiagCons -> Pos -> BlockLength -> Pos
rightPos Prog (x,y) blen = (x + (blen * 3), y)
rightPos Interp (x,y) blen = (x + (blen * 3), y)
rightPos Comp (x,y) blen = (x + (blen * 2), y)

dtd_ :: Diag -> Diag_
dtd_ (Diag _ d) = d

--We have to compile d1, but because we are using the actual diag of d1
--it can still contain compiles and executes. So we'll have to recurse through
--them and determine what will eventually be the returning diagram
compile :: Diag_ -> Diag_ -> Diag_
compile dg (Compile d1 d2) = compile dg (compile (dtd_ d1) (dtd_ d2))
compile (Program p _) (Compiler _ _ t _) = Program p t
compile (Interpreter i l _) (Compiler _ _ t _) = Interpreter i l t
compile (Compiler c l1 l2 _) (Compiler _ _ t _) = Compiler c l1 l2 t
compile (Execute d1 d2) dg = compile (dtd_ d2) dg
compile (Compile d1 d2) dg = compile (compile (dtd_ d1) (dtd_ d2)) dg
compile d1 _ = d1

--Get the list of commands for the compiled diagram
compiled :: Diag_ -> Pos -> BlockLength -> [Command]
compiled (Program p l) pos blen = program pos blen p l
compiled (Interpreter i l m) pos blen = interpreter pos blen i l m
compiled (Compiler c l1 l2 t) pos blen = compiler pos blen c l1 l2 t
compiled _ _ _= []


--Next 4 functions define the templates for the basic constructs
platform :: Pos -> BlockLength -> String -> [Command]
platform (x,y) blen m = [
            Put (x, y - 15) $ Line (5, -3) 25,
            Put (x + blen / 2,y - 30) $ Line (5, 3) 25,
            Put (x, y - 15) $ Line (0, 1) 15,
            Put (x, y)  $ Line (1, 0) 50,
            Put (x + blen, y) $ Line (0,-1) 15,
            Put (x, y - 15) $ Makebox (50, 15) m
        ]

program :: Pos -> BlockLength -> String -> String -> [Command]
program (x,y) blen p l = [
                Put (x + 7.5, y - 30) $ Line (1,0) blen,
                Put (x + 7.5, y - 30) $ Line (0,1) 15,
                Put (x + 7.5, y - 15) $ Line (-1, 2) 7.5,
                Put (x + blen + 7.5, y - 15) $ Line (1, 2) 7.5,
                Put (x + blen + 7.5, y - 30) $ Line (0,1) 15,
                Put (x, y) $ Line (1,0) (blen + 15),
                Put (x + 7.5, y - 15) $ Makebox (blen, 15) p,
                Put (x + 7.5, y - 30) $ Makebox (blen, 15) l
            ]


interpreter :: Pos -> BlockLength -> String -> String -> String -> [Command]
interpreter (x,y) blen i l m = [ 
                        Put (x, y - 30) $ Framebox (blen, 30) "",
                        Put (x, y - 10) $ Makebox (blen, 10) l,
                        Put (x, y - 20) $ Makebox (blen, 10) i,
                        Put (x, y - 30) $ Makebox (blen,10) m
                    ]

compiler :: Pos -> BlockLength -> String -> String -> String -> String -> [Command] 
compiler (x,y) blen c l1 l2 m = [
                        Put (x + blen, y - 30) $ Line (0, 1) 20,
                        Put (x + blen, y - 10) $ Line (-1, 0) blen,
                        Put (x, y - 10) $ Line (0, 1) 10,
                        Put (x, y) $ Line (1, 0) (3 * blen),
                        Put (x + (3 * blen), y) $ Line (0,-1) 10,
                        Put (x + (3 * blen), y - 10) $ Line (-1,0) blen,
                        Put (x + (2 * blen), y - 10) $ Line (0,-1) 20,
                        Put (x + (2 * blen), y - 30) $ Line (-1, 0) blen,
                        Put (x, y - 10) $ Makebox (blen,10) l1,
                        Put (x + blen, y - 10) $ Makebox (blen,10) "$\\longrightarrow$",
                        Put (x + (2 * blen), y - 10) $ Makebox (blen,10) l2,
                        Put (x + blen, y - 20) $ Makebox (blen,10) c,
                        Put (x + blen, y - 30) $ Makebox (blen,10) m
                    ]
{-# LINE 217 "Ag.hs" #-}

{-# LINE 16 "..\\Diag.ag" #-}

type Ident = String
{-# LINE 222 "Ag.hs" #-}

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
{-# LINE 248 "Ag.hs" #-}

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
{-# LINE 269 "Ag.hs" #-}
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
              {-# LINE 296 "Ag.hs" #-}
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
              {-# LINE 328 "Ag.hs" #-}
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
              {-# LINE 341 "Ag.hs" #-}
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
type T_Diag = BlockLength ->
              Pos ->
              ( Depth,Cjoint,([Command]),DiagCons,Diag,Ejoint,Height,TotalLength,Width,Depth)
data Inh_Diag = Inh_Diag {blen_Inh_Diag :: BlockLength,pos_Inh_Diag :: Pos}
data Syn_Diag = Syn_Diag {cdepth_Syn_Diag :: Depth,cjoint_Syn_Diag :: Cjoint,cmd_Syn_Diag :: ([Command]),dcons_Syn_Diag :: DiagCons,diag_Syn_Diag :: Diag,ejoint_Syn_Diag :: Ejoint,h_Syn_Diag :: Height,tlen_Syn_Diag :: TotalLength,w_Syn_Diag :: Width,wdepth_Syn_Diag :: Depth}
wrap_Diag :: T_Diag ->
             Inh_Diag ->
             Syn_Diag
wrap_Diag sem (Inh_Diag _lhsIblen _lhsIpos) =
    (let ( _lhsOcdepth,_lhsOcjoint,_lhsOcmd,_lhsOdcons,_lhsOdiag,_lhsOejoint,_lhsOh,_lhsOtlen,_lhsOw,_lhsOwdepth) = sem _lhsIblen _lhsIpos
     in  (Syn_Diag _lhsOcdepth _lhsOcjoint _lhsOcmd _lhsOdcons _lhsOdiag _lhsOejoint _lhsOh _lhsOtlen _lhsOw _lhsOwdepth))
sem_Diag_Diag :: SourcePos ->
                 T_Diag_ ->
                 T_Diag
sem_Diag_Diag pos_ d_ =
    (\ _lhsIblen
       _lhsIpos ->
         (let _lhsOcmd :: ([Command])
              _lhsOdiag :: Diag
              _lhsOcdepth :: Depth
              _lhsOcjoint :: Cjoint
              _lhsOdcons :: DiagCons
              _lhsOejoint :: Ejoint
              _lhsOh :: Height
              _lhsOtlen :: TotalLength
              _lhsOw :: Width
              _lhsOwdepth :: Depth
              _dOblen :: BlockLength
              _dOpos :: Pos
              _dIcdepth :: Depth
              _dIcjoint :: Cjoint
              _dIcmd :: ([Command])
              _dIdcons :: DiagCons
              _dIdiag :: Diag_
              _dIejoint :: Ejoint
              _dIh :: Height
              _dItlen :: TotalLength
              _dIw :: Width
              _dIwdepth :: Depth
              _lhsOcmd =
                  ({-# LINE 48 "AG\\Translate.ag" #-}
                   _dIcmd
                   {-# LINE 394 "Ag.hs" #-}
                   )
              _diag =
                  ({-# LINE 49 "AG\\Translate.ag" #-}
                   Diag pos_ _dIdiag
                   {-# LINE 399 "Ag.hs" #-}
                   )
              _lhsOdiag =
                  ({-# LINE 49 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 404 "Ag.hs" #-}
                   )
              _lhsOcdepth =
                  ({-# LINE 57 "AG\\Translate.ag" #-}
                   _dIcdepth
                   {-# LINE 409 "Ag.hs" #-}
                   )
              _lhsOcjoint =
                  ({-# LINE 54 "AG\\Translate.ag" #-}
                   _dIcjoint
                   {-# LINE 414 "Ag.hs" #-}
                   )
              _lhsOdcons =
                  ({-# LINE 50 "AG\\Translate.ag" #-}
                   _dIdcons
                   {-# LINE 419 "Ag.hs" #-}
                   )
              _lhsOejoint =
                  ({-# LINE 55 "AG\\Translate.ag" #-}
                   _dIejoint
                   {-# LINE 424 "Ag.hs" #-}
                   )
              _lhsOh =
                  ({-# LINE 52 "AG\\Translate.ag" #-}
                   _dIh
                   {-# LINE 429 "Ag.hs" #-}
                   )
              _lhsOtlen =
                  ({-# LINE 53 "AG\\Translate.ag" #-}
                   _dItlen
                   {-# LINE 434 "Ag.hs" #-}
                   )
              _lhsOw =
                  ({-# LINE 51 "AG\\Translate.ag" #-}
                   _dIw
                   {-# LINE 439 "Ag.hs" #-}
                   )
              _lhsOwdepth =
                  ({-# LINE 56 "AG\\Translate.ag" #-}
                   _dIwdepth
                   {-# LINE 444 "Ag.hs" #-}
                   )
              _dOblen =
                  ({-# LINE 46 "AG\\Translate.ag" #-}
                   _lhsIblen
                   {-# LINE 449 "Ag.hs" #-}
                   )
              _dOpos =
                  ({-# LINE 47 "AG\\Translate.ag" #-}
                   _lhsIpos
                   {-# LINE 454 "Ag.hs" #-}
                   )
              ( _dIcdepth,_dIcjoint,_dIcmd,_dIdcons,_dIdiag,_dIejoint,_dIh,_dItlen,_dIw,_dIwdepth) =
                  d_ _dOblen _dOpos
          in  ( _lhsOcdepth,_lhsOcjoint,_lhsOcmd,_lhsOdcons,_lhsOdiag,_lhsOejoint,_lhsOh,_lhsOtlen,_lhsOw,_lhsOwdepth)))
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
type T_Diag_ = BlockLength ->
               Pos ->
               ( Depth,Cjoint,([Command]),DiagCons,Diag_,Ejoint,Height,TotalLength,Width,Depth)
data Inh_Diag_ = Inh_Diag_ {blen_Inh_Diag_ :: BlockLength,pos_Inh_Diag_ :: Pos}
data Syn_Diag_ = Syn_Diag_ {cdepth_Syn_Diag_ :: Depth,cjoint_Syn_Diag_ :: Cjoint,cmd_Syn_Diag_ :: ([Command]),dcons_Syn_Diag_ :: DiagCons,diag_Syn_Diag_ :: Diag_,ejoint_Syn_Diag_ :: Ejoint,h_Syn_Diag_ :: Height,tlen_Syn_Diag_ :: TotalLength,w_Syn_Diag_ :: Width,wdepth_Syn_Diag_ :: Depth}
wrap_Diag_ :: T_Diag_ ->
              Inh_Diag_ ->
              Syn_Diag_
wrap_Diag_ sem (Inh_Diag_ _lhsIblen _lhsIpos) =
    (let ( _lhsOcdepth,_lhsOcjoint,_lhsOcmd,_lhsOdcons,_lhsOdiag,_lhsOejoint,_lhsOh,_lhsOtlen,_lhsOw,_lhsOwdepth) = sem _lhsIblen _lhsIpos
     in  (Syn_Diag_ _lhsOcdepth _lhsOcjoint _lhsOcmd _lhsOdcons _lhsOdiag _lhsOejoint _lhsOh _lhsOtlen _lhsOw _lhsOwdepth))
sem_Diag__Program :: Ident ->
                     Ident ->
                     T_Diag_
sem_Diag__Program p_ l_ =
    (\ _lhsIblen
       _lhsIpos ->
         (let _lhsOcmd :: ([Command])
              _lhsOdcons :: DiagCons
              _lhsOtlen :: TotalLength
              _lhsOh :: Height
              _lhsOw :: Width
              _lhsOcjoint :: Cjoint
              _lhsOejoint :: Ejoint
              _lhsOwdepth :: Depth
              _lhsOcdepth :: Depth
              _lhsOdiag :: Diag_
              _lhsOcmd =
                  ({-# LINE 69 "AG\\Translate.ag" #-}
                   program _lhsIpos _lhsIblen p_ l_
                   {-# LINE 554 "Ag.hs" #-}
                   )
              _lhsOdcons =
                  ({-# LINE 70 "AG\\Translate.ag" #-}
                   Prog
                   {-# LINE 559 "Ag.hs" #-}
                   )
              _lhsOtlen =
                  ({-# LINE 71 "AG\\Translate.ag" #-}
                   _lhsIblen
                   {-# LINE 564 "Ag.hs" #-}
                   )
              _lhsOh =
                  ({-# LINE 72 "AG\\Translate.ag" #-}
                   30
                   {-# LINE 569 "Ag.hs" #-}
                   )
              _lhsOw =
                  ({-# LINE 73 "AG\\Translate.ag" #-}
                   _lhsIblen + 15
                   {-# LINE 574 "Ag.hs" #-}
                   )
              _lhsOcjoint =
                  ({-# LINE 74 "AG\\Translate.ag" #-}
                   cJoint Prog _lhsIpos _lhsIblen
                   {-# LINE 579 "Ag.hs" #-}
                   )
              _lhsOejoint =
                  ({-# LINE 75 "AG\\Translate.ag" #-}
                   eJoint Prog _lhsIpos _lhsIblen
                   {-# LINE 584 "Ag.hs" #-}
                   )
              _lhsOwdepth =
                  ({-# LINE 76 "AG\\Translate.ag" #-}
                   0
                   {-# LINE 589 "Ag.hs" #-}
                   )
              _lhsOcdepth =
                  ({-# LINE 77 "AG\\Translate.ag" #-}
                   0
                   {-# LINE 594 "Ag.hs" #-}
                   )
              _diag =
                  ({-# LINE 49 "AG\\Translate.ag" #-}
                   Program p_ l_
                   {-# LINE 599 "Ag.hs" #-}
                   )
              _lhsOdiag =
                  ({-# LINE 49 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 604 "Ag.hs" #-}
                   )
          in  ( _lhsOcdepth,_lhsOcjoint,_lhsOcmd,_lhsOdcons,_lhsOdiag,_lhsOejoint,_lhsOh,_lhsOtlen,_lhsOw,_lhsOwdepth)))
sem_Diag__Platform :: Ident ->
                      T_Diag_
sem_Diag__Platform m_ =
    (\ _lhsIblen
       _lhsIpos ->
         (let _lhsOcmd :: ([Command])
              _lhsOdcons :: DiagCons
              _lhsOtlen :: TotalLength
              _lhsOh :: Height
              _lhsOw :: Width
              _lhsOcjoint :: Cjoint
              _lhsOejoint :: Ejoint
              _lhsOwdepth :: Depth
              _lhsOcdepth :: Depth
              _lhsOdiag :: Diag_
              _lhsOcmd =
                  ({-# LINE 60 "AG\\Translate.ag" #-}
                   platform _lhsIpos _lhsIblen m_
                   {-# LINE 625 "Ag.hs" #-}
                   )
              _lhsOdcons =
                  ({-# LINE 61 "AG\\Translate.ag" #-}
                   Plat
                   {-# LINE 630 "Ag.hs" #-}
                   )
              _lhsOtlen =
                  ({-# LINE 62 "AG\\Translate.ag" #-}
                   _lhsIblen - 7.5
                   {-# LINE 635 "Ag.hs" #-}
                   )
              _lhsOh =
                  ({-# LINE 63 "AG\\Translate.ag" #-}
                   30
                   {-# LINE 640 "Ag.hs" #-}
                   )
              _lhsOw =
                  ({-# LINE 64 "AG\\Translate.ag" #-}
                   _lhsIblen
                   {-# LINE 645 "Ag.hs" #-}
                   )
              _lhsOcjoint =
                  ({-# LINE 65 "AG\\Translate.ag" #-}
                   cJoint Plat _lhsIpos _lhsIblen
                   {-# LINE 650 "Ag.hs" #-}
                   )
              _lhsOejoint =
                  ({-# LINE 66 "AG\\Translate.ag" #-}
                   eJoint Plat _lhsIpos _lhsIblen
                   {-# LINE 655 "Ag.hs" #-}
                   )
              _lhsOwdepth =
                  ({-# LINE 67 "AG\\Translate.ag" #-}
                   0
                   {-# LINE 660 "Ag.hs" #-}
                   )
              _lhsOcdepth =
                  ({-# LINE 68 "AG\\Translate.ag" #-}
                   0
                   {-# LINE 665 "Ag.hs" #-}
                   )
              _diag =
                  ({-# LINE 49 "AG\\Translate.ag" #-}
                   Platform m_
                   {-# LINE 670 "Ag.hs" #-}
                   )
              _lhsOdiag =
                  ({-# LINE 49 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 675 "Ag.hs" #-}
                   )
          in  ( _lhsOcdepth,_lhsOcjoint,_lhsOcmd,_lhsOdcons,_lhsOdiag,_lhsOejoint,_lhsOh,_lhsOtlen,_lhsOw,_lhsOwdepth)))
sem_Diag__Interpreter :: Ident ->
                         Ident ->
                         Ident ->
                         T_Diag_
sem_Diag__Interpreter i_ l_ m_ =
    (\ _lhsIblen
       _lhsIpos ->
         (let _lhsOcmd :: ([Command])
              _lhsOdcons :: DiagCons
              _lhsOh :: Height
              _lhsOw :: Width
              _lhsOtlen :: TotalLength
              _lhsOcjoint :: Cjoint
              _lhsOejoint :: Ejoint
              _lhsOwdepth :: Depth
              _lhsOcdepth :: Depth
              _lhsOdiag :: Diag_
              _lhsOcmd =
                  ({-# LINE 78 "AG\\Translate.ag" #-}
                   interpreter _lhsIpos _lhsIblen i_ l_ m_
                   {-# LINE 698 "Ag.hs" #-}
                   )
              _lhsOdcons =
                  ({-# LINE 79 "AG\\Translate.ag" #-}
                   Interp
                   {-# LINE 703 "Ag.hs" #-}
                   )
              _lhsOh =
                  ({-# LINE 80 "AG\\Translate.ag" #-}
                   30
                   {-# LINE 708 "Ag.hs" #-}
                   )
              _lhsOw =
                  ({-# LINE 81 "AG\\Translate.ag" #-}
                   _lhsIblen
                   {-# LINE 713 "Ag.hs" #-}
                   )
              _lhsOtlen =
                  ({-# LINE 82 "AG\\Translate.ag" #-}
                   _lhsIblen
                   {-# LINE 718 "Ag.hs" #-}
                   )
              _lhsOcjoint =
                  ({-# LINE 83 "AG\\Translate.ag" #-}
                   cJoint Interp _lhsIpos _lhsIblen
                   {-# LINE 723 "Ag.hs" #-}
                   )
              _lhsOejoint =
                  ({-# LINE 84 "AG\\Translate.ag" #-}
                   eJoint Interp _lhsIpos _lhsIblen
                   {-# LINE 728 "Ag.hs" #-}
                   )
              _lhsOwdepth =
                  ({-# LINE 85 "AG\\Translate.ag" #-}
                   0
                   {-# LINE 733 "Ag.hs" #-}
                   )
              _lhsOcdepth =
                  ({-# LINE 86 "AG\\Translate.ag" #-}
                   0
                   {-# LINE 738 "Ag.hs" #-}
                   )
              _diag =
                  ({-# LINE 49 "AG\\Translate.ag" #-}
                   Interpreter i_ l_ m_
                   {-# LINE 743 "Ag.hs" #-}
                   )
              _lhsOdiag =
                  ({-# LINE 49 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 748 "Ag.hs" #-}
                   )
          in  ( _lhsOcdepth,_lhsOcjoint,_lhsOcmd,_lhsOdcons,_lhsOdiag,_lhsOejoint,_lhsOh,_lhsOtlen,_lhsOw,_lhsOwdepth)))
sem_Diag__Compiler :: Ident ->
                      Ident ->
                      Ident ->
                      Ident ->
                      T_Diag_
sem_Diag__Compiler c_ l1_ l2_ m_ =
    (\ _lhsIblen
       _lhsIpos ->
         (let _lhsOcmd :: ([Command])
              _lhsOdcons :: DiagCons
              _lhsOh :: Height
              _lhsOw :: Width
              _lhsOtlen :: TotalLength
              _lhsOcjoint :: Cjoint
              _lhsOejoint :: Ejoint
              _lhsOwdepth :: Depth
              _lhsOcdepth :: Depth
              _lhsOdiag :: Diag_
              _lhsOcmd =
                  ({-# LINE 87 "AG\\Translate.ag" #-}
                   compiler _lhsIpos _lhsIblen c_ l1_ l2_ m_
                   {-# LINE 772 "Ag.hs" #-}
                   )
              _lhsOdcons =
                  ({-# LINE 88 "AG\\Translate.ag" #-}
                   Comp
                   {-# LINE 777 "Ag.hs" #-}
                   )
              _lhsOh =
                  ({-# LINE 89 "AG\\Translate.ag" #-}
                   30
                   {-# LINE 782 "Ag.hs" #-}
                   )
              _lhsOw =
                  ({-# LINE 90 "AG\\Translate.ag" #-}
                   3 * _lhsIblen
                   {-# LINE 787 "Ag.hs" #-}
                   )
              _lhsOtlen =
                  ({-# LINE 91 "AG\\Translate.ag" #-}
                   2 * _lhsIblen
                   {-# LINE 792 "Ag.hs" #-}
                   )
              _lhsOcjoint =
                  ({-# LINE 92 "AG\\Translate.ag" #-}
                   cJoint Comp _lhsIpos _lhsIblen
                   {-# LINE 797 "Ag.hs" #-}
                   )
              _lhsOejoint =
                  ({-# LINE 93 "AG\\Translate.ag" #-}
                   eJoint Comp _lhsIpos _lhsIblen
                   {-# LINE 802 "Ag.hs" #-}
                   )
              _lhsOwdepth =
                  ({-# LINE 94 "AG\\Translate.ag" #-}
                   0
                   {-# LINE 807 "Ag.hs" #-}
                   )
              _lhsOcdepth =
                  ({-# LINE 95 "AG\\Translate.ag" #-}
                   0
                   {-# LINE 812 "Ag.hs" #-}
                   )
              _diag =
                  ({-# LINE 49 "AG\\Translate.ag" #-}
                   Compiler c_ l1_ l2_ m_
                   {-# LINE 817 "Ag.hs" #-}
                   )
              _lhsOdiag =
                  ({-# LINE 49 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 822 "Ag.hs" #-}
                   )
          in  ( _lhsOcdepth,_lhsOcjoint,_lhsOcmd,_lhsOdcons,_lhsOdiag,_lhsOejoint,_lhsOh,_lhsOtlen,_lhsOw,_lhsOwdepth)))
sem_Diag__Execute :: T_Diag ->
                     T_Diag ->
                     T_Diag_
sem_Diag__Execute d1_ d2_ =
    (\ _lhsIblen
       _lhsIpos ->
         (let _lhsOcmd :: ([Command])
              _lhsOdcons :: DiagCons
              _d1Opos :: Pos
              _d2Opos :: Pos
              _d1Oblen :: BlockLength
              _d2Oblen :: BlockLength
              _lhsOh :: Height
              _lhsOw :: Width
              _lhsOtlen :: TotalLength
              _lhsOcjoint :: Cjoint
              _lhsOejoint :: Ejoint
              _lhsOwdepth :: Depth
              _lhsOcdepth :: Depth
              _lhsOdiag :: Diag_
              _d1Icdepth :: Depth
              _d1Icjoint :: Cjoint
              _d1Icmd :: ([Command])
              _d1Idcons :: DiagCons
              _d1Idiag :: Diag
              _d1Iejoint :: Ejoint
              _d1Ih :: Height
              _d1Itlen :: TotalLength
              _d1Iw :: Width
              _d1Iwdepth :: Depth
              _d2Icdepth :: Depth
              _d2Icjoint :: Cjoint
              _d2Icmd :: ([Command])
              _d2Idcons :: DiagCons
              _d2Idiag :: Diag
              _d2Iejoint :: Ejoint
              _d2Ih :: Height
              _d2Itlen :: TotalLength
              _d2Iw :: Width
              _d2Iwdepth :: Depth
              _lhsOcmd =
                  ({-# LINE 96 "AG\\Translate.ag" #-}
                   _d1Icmd ++ _d2Icmd
                   {-# LINE 868 "Ag.hs" #-}
                   )
              _lhsOdcons =
                  ({-# LINE 97 "AG\\Translate.ag" #-}
                   _d2Idcons
                   {-# LINE 873 "Ag.hs" #-}
                   )
              _d1Opos =
                  ({-# LINE 98 "AG\\Translate.ag" #-}
                   _lhsIpos
                   {-# LINE 878 "Ag.hs" #-}
                   )
              _d2Opos =
                  ({-# LINE 99 "AG\\Translate.ag" #-}
                   _d1Iejoint
                   {-# LINE 883 "Ag.hs" #-}
                   )
              _d1Oblen =
                  ({-# LINE 100 "AG\\Translate.ag" #-}
                   _lhsIblen
                   {-# LINE 888 "Ag.hs" #-}
                   )
              _d2Oblen =
                  ({-# LINE 101 "AG\\Translate.ag" #-}
                   _lhsIblen
                   {-# LINE 893 "Ag.hs" #-}
                   )
              _lhsOh =
                  ({-# LINE 102 "AG\\Translate.ag" #-}
                   executeHeight _d1Idiag _d1Ih _d2Ih
                   {-# LINE 898 "Ag.hs" #-}
                   )
              _lhsOw =
                  ({-# LINE 103 "AG\\Translate.ag" #-}
                   max _d1Iw _d2Iw
                   {-# LINE 903 "Ag.hs" #-}
                   )
              _lhsOtlen =
                  ({-# LINE 104 "AG\\Translate.ag" #-}
                   _d1Itlen + _d2Itlen
                   {-# LINE 908 "Ag.hs" #-}
                   )
              _lhsOcjoint =
                  ({-# LINE 105 "AG\\Translate.ag" #-}
                   _d2Icjoint
                   {-# LINE 913 "Ag.hs" #-}
                   )
              _lhsOejoint =
                  ({-# LINE 106 "AG\\Translate.ag" #-}
                   _d2Iejoint
                   {-# LINE 918 "Ag.hs" #-}
                   )
              _lhsOwdepth =
                  ({-# LINE 107 "AG\\Translate.ag" #-}
                   0
                   {-# LINE 923 "Ag.hs" #-}
                   )
              _lhsOcdepth =
                  ({-# LINE 108 "AG\\Translate.ag" #-}
                   0
                   {-# LINE 928 "Ag.hs" #-}
                   )
              _diag =
                  ({-# LINE 49 "AG\\Translate.ag" #-}
                   Execute _d1Idiag _d2Idiag
                   {-# LINE 933 "Ag.hs" #-}
                   )
              _lhsOdiag =
                  ({-# LINE 49 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 938 "Ag.hs" #-}
                   )
              ( _d1Icdepth,_d1Icjoint,_d1Icmd,_d1Idcons,_d1Idiag,_d1Iejoint,_d1Ih,_d1Itlen,_d1Iw,_d1Iwdepth) =
                  d1_ _d1Oblen _d1Opos
              ( _d2Icdepth,_d2Icjoint,_d2Icmd,_d2Idcons,_d2Idiag,_d2Iejoint,_d2Ih,_d2Itlen,_d2Iw,_d2Iwdepth) =
                  d2_ _d2Oblen _d2Opos
          in  ( _lhsOcdepth,_lhsOcjoint,_lhsOcmd,_lhsOdcons,_lhsOdiag,_lhsOejoint,_lhsOh,_lhsOtlen,_lhsOw,_lhsOwdepth)))
sem_Diag__Compile :: T_Diag ->
                     T_Diag ->
                     T_Diag_
sem_Diag__Compile d1_ d2_ =
    (\ _lhsIblen
       _lhsIpos ->
         (let _lhsOcmd :: ([Command])
              _lhsOdcons :: DiagCons
              _d1Opos :: Pos
              _d2Opos :: Pos
              _d1Oblen :: BlockLength
              _d2Oblen :: BlockLength
              _lhsOh :: Height
              _lhsOw :: Width
              _lhsOtlen :: TotalLength
              _lhsOcjoint :: Cjoint
              _lhsOejoint :: Ejoint
              _lhsOwdepth :: Depth
              _lhsOcdepth :: Depth
              _lhsOdiag :: Diag_
              _d1Icdepth :: Depth
              _d1Icjoint :: Cjoint
              _d1Icmd :: ([Command])
              _d1Idcons :: DiagCons
              _d1Idiag :: Diag
              _d1Iejoint :: Ejoint
              _d1Ih :: Height
              _d1Itlen :: TotalLength
              _d1Iw :: Width
              _d1Iwdepth :: Depth
              _d2Icdepth :: Depth
              _d2Icjoint :: Cjoint
              _d2Icmd :: ([Command])
              _d2Idcons :: DiagCons
              _d2Idiag :: Diag
              _d2Iejoint :: Ejoint
              _d2Ih :: Height
              _d2Itlen :: TotalLength
              _d2Iw :: Width
              _d2Iwdepth :: Depth
              _cpos =
                  ({-# LINE 109 "AG\\Translate.ag" #-}
                   rightPos _d1Idcons (fst _d1pos     + _d1Itlen, snd _d1pos    ) _lhsIblen
                   {-# LINE 988 "Ag.hs" #-}
                   )
              _cmpl =
                  ({-# LINE 110 "AG\\Translate.ag" #-}
                   compile (dtd_ _d1Idiag) (dtd_ _d2Idiag)
                   {-# LINE 993 "Ag.hs" #-}
                   )
              _lhsOcmd =
                  ({-# LINE 111 "AG\\Translate.ag" #-}
                   _d1Icmd ++ _d2Icmd ++ compiled _cmpl     _cpos     _lhsIblen
                   {-# LINE 998 "Ag.hs" #-}
                   )
              _lhsOdcons =
                  ({-# LINE 112 "AG\\Translate.ag" #-}
                   _d1Idcons
                   {-# LINE 1003 "Ag.hs" #-}
                   )
              _d1pos =
                  ({-# LINE 113 "AG\\Translate.ag" #-}
                   d1Pos _d1Icdepth _d2Iwdepth _lhsIpos _lhsIblen _d1Idcons
                   {-# LINE 1008 "Ag.hs" #-}
                   )
              _d2pos =
                  ({-# LINE 114 "AG\\Translate.ag" #-}
                   d2Pos _d1Icdepth _d2Iwdepth _lhsIpos _lhsIblen _d1Icjoint
                   {-# LINE 1013 "Ag.hs" #-}
                   )
              _d1Opos =
                  ({-# LINE 115 "AG\\Translate.ag" #-}
                   _d1pos
                   {-# LINE 1018 "Ag.hs" #-}
                   )
              _d2Opos =
                  ({-# LINE 116 "AG\\Translate.ag" #-}
                   _d2pos
                   {-# LINE 1023 "Ag.hs" #-}
                   )
              _d1Oblen =
                  ({-# LINE 117 "AG\\Translate.ag" #-}
                   _lhsIblen
                   {-# LINE 1028 "Ag.hs" #-}
                   )
              _d2Oblen =
                  ({-# LINE 118 "AG\\Translate.ag" #-}
                   _lhsIblen
                   {-# LINE 1033 "Ag.hs" #-}
                   )
              _lhsOh =
                  ({-# LINE 119 "AG\\Translate.ag" #-}
                   compileHeight _d1Icdepth _d1Ih _d2Ih
                   {-# LINE 1038 "Ag.hs" #-}
                   )
              _lhsOw =
                  ({-# LINE 120 "AG\\Translate.ag" #-}
                   compileWidth _d2Iwdepth _d1Idcons _lhsIblen _d1Iw _d2Iw
                   {-# LINE 1043 "Ag.hs" #-}
                   )
              _lhsOtlen =
                  ({-# LINE 121 "AG\\Translate.ag" #-}
                   _d1Itlen + (3 * _lhsIblen) + _lhsIblen
                   {-# LINE 1048 "Ag.hs" #-}
                   )
              _lhsOcjoint =
                  ({-# LINE 122 "AG\\Translate.ag" #-}
                   cJoint _d1Idcons _cpos     _lhsIblen
                   {-# LINE 1053 "Ag.hs" #-}
                   )
              _lhsOejoint =
                  ({-# LINE 123 "AG\\Translate.ag" #-}
                   eJoint _d1Idcons _cpos     _lhsIblen
                   {-# LINE 1058 "Ag.hs" #-}
                   )
              _lhsOwdepth =
                  ({-# LINE 124 "AG\\Translate.ag" #-}
                   1 + _d2Iwdepth
                   {-# LINE 1063 "Ag.hs" #-}
                   )
              _lhsOcdepth =
                  ({-# LINE 125 "AG\\Translate.ag" #-}
                   1 + _d1Icdepth
                   {-# LINE 1068 "Ag.hs" #-}
                   )
              _diag =
                  ({-# LINE 49 "AG\\Translate.ag" #-}
                   Compile _d1Idiag _d2Idiag
                   {-# LINE 1073 "Ag.hs" #-}
                   )
              _lhsOdiag =
                  ({-# LINE 49 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 1078 "Ag.hs" #-}
                   )
              ( _d1Icdepth,_d1Icjoint,_d1Icmd,_d1Idcons,_d1Idiag,_d1Iejoint,_d1Ih,_d1Itlen,_d1Iw,_d1Iwdepth) =
                  d1_ _d1Oblen _d1Opos
              ( _d2Icdepth,_d2Icjoint,_d2Icmd,_d2Idcons,_d2Idiag,_d2Iejoint,_d2Ih,_d2Itlen,_d2Iw,_d2Iwdepth) =
                  d2_ _d2Oblen _d2Opos
          in  ( _lhsOcdepth,_lhsOcjoint,_lhsOcmd,_lhsOdcons,_lhsOdiag,_lhsOejoint,_lhsOh,_lhsOtlen,_lhsOw,_lhsOwdepth)))
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
              {-# LINE 1116 "Ag.hs" #-}
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
              {-# LINE 1127 "Ag.hs" #-}
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
              {-# LINE 1138 "Ag.hs" #-}
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
         _dOpos :: Pos
         _dOblen :: BlockLength
         _dIcdepth :: Depth
         _dIcjoint :: Cjoint
         _dIcmd :: ([Command])
         _dIdcons :: DiagCons
         _dIdiag :: Diag
         _dIejoint :: Ejoint
         _dIh :: Height
         _dItlen :: TotalLength
         _dIw :: Width
         _dIwdepth :: Depth
         _lhsOpic =
             ({-# LINE 28 "AG\\Translate.ag" #-}
              Picture (_dIw,_dIh) (_dIcmd ++ frame (_dIw,_dIh))
              {-# LINE 1177 "Ag.hs" #-}
              )
         _dOpos =
             ({-# LINE 29 "AG\\Translate.ag" #-}
              (-100,_dIh)
              {-# LINE 1182 "Ag.hs" #-}
              )
         _dOblen =
             ({-# LINE 30 "AG\\Translate.ag" #-}
              50
              {-# LINE 1187 "Ag.hs" #-}
              )
         ( _dIcdepth,_dIcjoint,_dIcmd,_dIdcons,_dIdiag,_dIejoint,_dIh,_dItlen,_dIw,_dIwdepth) =
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
              {-# LINE 1220 "Ag.hs" #-}
              )
         ( _cmdsIpp) =
             cmds_
     in  ( _lhsOpp))