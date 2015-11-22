

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

{-# LINE 62 "AG\\Translate.ag" #-}

type Position = (Double,Double)

arrowLength :: Double
arrowLength = 20

wordLength :: String -> Double
wordLength s = fromIntegral (length s) * 5

maxLength :: Diag_ -> Double
maxLength (Platform m) = wordLength m
maxLength (Program p l) = max (wordLength p) (wordLength l)
maxLength (Interpreter i l m) = maximum [wordLength i, wordLength l, wordLength m]
maxLength (Compiler c l1 l2 m) = maximum [wordLength c, wordLength l1, wordLength l2, wordLength m]

--These are necessary, because we need to determine the ejoint and cjoint of the compiled
--diag, but as we then do not know what diag it is we have to do pattern matching.
eJoint :: Diag_ -> (Double,Double) -> Double -> (Double,Double)
eJoint (Program _ _) (x,y) _= (x + 7.5, y - 30)
eJoint (Interpreter _ _ _) (x, y) _= (x, y - 30)
eJoint (Compiler _ _ _ _) (x, y) len = (x + len, y - 30)

cJoint :: Diag_ -> (Double,Double) -> Double -> (Double,Double)
cJoint (Program _ _) (x, y) len = (x + len + 7.5, y - 20)
cJoint (Interpreter _ _ _) (x, y) len = (x + len, y - 20)
cJoint (Compiler _ _ _ _) (x, y) len = (x + len, y - 20)

rightPos :: Diag_ -> Position -> Double -> Position
rightPos (Program _ _) (x, y) len = (y + len + 7.5, y - 20)
rightPos (Interpreter _ _ _) (x, y) len = (x + len, y - 20)
rightPos (Compiler _ _ _ _) (x, y) len = (x + len, y - 20)

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
            Put (x, y + 15) $ Line (5, -3) 25,
            Put (x + len / 2,y) $ Line (5, 3) 25,
            Put (x, y + 15) $ Line (0, 1) 15,
            Put (x, y + 30) $ Line (1, 0) 50,
            Put (x + len, y + 30) $ Line (0,-1) 15,
            Put (x, y + 15) $ Makebox (len, 15) m
        ]

program ::  (Double, Double) -> Double -> String -> String -> [Command]
program (x,y) len p l = [
                Put (x + 7.5, y) $ Line (1,0) len,
                Put (x + 7.5, y) $ Line (0,1) 15,
                Put (x + 7.5, y + 15) $ Line (-1, 2) 7.5,
                Put (x + len + 7.5, y + 15) $ Line (1, 2) 7.5,
                Put (x + len + 7.5, y) $ Line (0,1) 15,
                Put (x, y + 30) $ Line (1,0) (len + 15),
                Put (x + 7.5, y + 15) $ Makebox (len, 15) p,
                Put (x + 7.5, y) $ Makebox (len, 15) l
            ]


interpreter :: (Double, Double) -> Double -> String -> String -> String -> [Command]
interpreter (x,y) len i l m = [ 
                        Put (x, y) $ Framebox (len, 30) "", 
                        Put (x, y + 20) $ Makebox (len, 10) i,
                        Put (x, y + 10) $ Makebox (len, 10) l,
                        Put (x, y) $ Makebox (len,10) m
                    ]

compiler :: (Double, Double) -> Double -> String -> String -> String -> String -> [Command] 
compiler (x,y) len c l1 l2 m = [
                        Put (x + len, y) $ Line (0, 1) 20,
                        Put (x + len, y + 20) $ Line (-1, 0) len,
                        Put (x, y + 20) $ Line (0, 1) 10,
                        Put (x, y + 30) $ Line (1, 0) (3 * len),
                        Put (x + (3 * len), y + 30) $ Line (0,-1) 10,
                        Put (x + (3 * len), y + 20) $ Line (-1,0) len,
                        Put (x + (2 * len), y + 20) $ Line (0,-1) 20,
                        Put (x + (2 * len), y) $ Line (-1, 0) len,
                        Put (x, y + 20) $ Makebox (len,10) l1,
                        Put (x + len, y + 20) $ Makebox (len,10) "$\\longrightarrow$",
                        Put (x + (2 * len), y + 20) $ Makebox (len,10) l2,
                        Put (x + len, y + 10) $ Makebox (len,10) c,
                        Put (x + len, y) $ Makebox (len,10) m
                    ]


{-# LINE 144 "Ag.hs" #-}

{-# LINE 16 "..\\Diag.ag" #-}

type Ident = String
{-# LINE 149 "Ag.hs" #-}

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
{-# LINE 175 "Ag.hs" #-}

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
{-# LINE 196 "Ag.hs" #-}
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
              {-# LINE 223 "Ag.hs" #-}
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
              {-# LINE 255 "Ag.hs" #-}
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
              {-# LINE 268 "Ag.hs" #-}
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
              ( ((Double,Double)),([Command]),Diag,((Double,Double)),((Double)))
data Inh_Diag = Inh_Diag {len_Inh_Diag :: ((Double)),pos_Inh_Diag :: ((Double,Double))}
data Syn_Diag = Syn_Diag {cjoint_Syn_Diag :: ((Double,Double)),cmd_Syn_Diag :: ([Command]),diag_Syn_Diag :: Diag,ejoint_Syn_Diag :: ((Double,Double)),mlen_Syn_Diag :: ((Double))}
wrap_Diag :: T_Diag ->
             Inh_Diag ->
             Syn_Diag
wrap_Diag sem (Inh_Diag _lhsIlen _lhsIpos) =
    (let ( _lhsOcjoint,_lhsOcmd,_lhsOdiag,_lhsOejoint,_lhsOmlen) = sem _lhsIlen _lhsIpos
     in  (Syn_Diag _lhsOcjoint _lhsOcmd _lhsOdiag _lhsOejoint _lhsOmlen))
sem_Diag_Diag :: SourcePos ->
                 T_Diag_ ->
                 T_Diag
sem_Diag_Diag pos_ d_ =
    (\ _lhsIlen
       _lhsIpos ->
         (let _lhsOcmd :: ([Command])
              _lhsOdiag :: Diag
              _lhsOcjoint :: ((Double,Double))
              _lhsOejoint :: ((Double,Double))
              _lhsOmlen :: ((Double))
              _dOlen :: ((Double))
              _dOpos :: ((Double,Double))
              _dIcjoint :: ((Double,Double))
              _dIcmd :: ([Command])
              _dIdiag :: Diag_
              _dIejoint :: ((Double,Double))
              _dImlen :: ((Double))
              _lhsOcmd =
                  ({-# LINE 17 "AG\\Translate.ag" #-}
                   _dIcmd
                   {-# LINE 311 "Ag.hs" #-}
                   )
              _diag =
                  ({-# LINE 16 "AG\\Translate.ag" #-}
                   Diag pos_ _dIdiag
                   {-# LINE 316 "Ag.hs" #-}
                   )
              _lhsOdiag =
                  ({-# LINE 16 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 321 "Ag.hs" #-}
                   )
              _lhsOcjoint =
                  ({-# LINE 18 "AG\\Translate.ag" #-}
                   _dIcjoint
                   {-# LINE 326 "Ag.hs" #-}
                   )
              _lhsOejoint =
                  ({-# LINE 19 "AG\\Translate.ag" #-}
                   _dIejoint
                   {-# LINE 331 "Ag.hs" #-}
                   )
              _lhsOmlen =
                  ({-# LINE 20 "AG\\Translate.ag" #-}
                   _dImlen
                   {-# LINE 336 "Ag.hs" #-}
                   )
              _dOlen =
                  ({-# LINE 21 "AG\\Translate.ag" #-}
                   _lhsIlen
                   {-# LINE 341 "Ag.hs" #-}
                   )
              _dOpos =
                  ({-# LINE 22 "AG\\Translate.ag" #-}
                   _lhsIpos
                   {-# LINE 346 "Ag.hs" #-}
                   )
              ( _dIcjoint,_dIcmd,_dIdiag,_dIejoint,_dImlen) =
                  d_ _dOlen _dOpos
          in  ( _lhsOcjoint,_lhsOcmd,_lhsOdiag,_lhsOejoint,_lhsOmlen)))
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
               ( ((Double,Double)),([Command]),Diag_,((Double,Double)),((Double)))
data Inh_Diag_ = Inh_Diag_ {len_Inh_Diag_ :: ((Double)),pos_Inh_Diag_ :: ((Double,Double))}
data Syn_Diag_ = Syn_Diag_ {cjoint_Syn_Diag_ :: ((Double,Double)),cmd_Syn_Diag_ :: ([Command]),diag_Syn_Diag_ :: Diag_,ejoint_Syn_Diag_ :: ((Double,Double)),mlen_Syn_Diag_ :: ((Double))}
wrap_Diag_ :: T_Diag_ ->
              Inh_Diag_ ->
              Syn_Diag_
wrap_Diag_ sem (Inh_Diag_ _lhsIlen _lhsIpos) =
    (let ( _lhsOcjoint,_lhsOcmd,_lhsOdiag,_lhsOejoint,_lhsOmlen) = sem _lhsIlen _lhsIpos
     in  (Syn_Diag_ _lhsOcjoint _lhsOcmd _lhsOdiag _lhsOejoint _lhsOmlen))
sem_Diag__Program :: Ident ->
                     Ident ->
                     T_Diag_
sem_Diag__Program p_ l_ =
    (\ _lhsIlen
       _lhsIpos ->
         (let _lhsOcmd :: ([Command])
              _lhsOejoint :: ((Double,Double))
              _lhsOcjoint :: ((Double,Double))
              _lhsOmlen :: ((Double))
              _lhsOdiag :: Diag_
              _lhsOcmd =
                  ({-# LINE 30 "AG\\Translate.ag" #-}
                   program _lhsIpos _lhsIlen p_ l_
                   {-# LINE 399 "Ag.hs" #-}
                   )
              _lhsOejoint =
                  ({-# LINE 31 "AG\\Translate.ag" #-}
                   eJoint _diag _lhsIpos _lhsIlen
                   {-# LINE 404 "Ag.hs" #-}
                   )
              _lhsOcjoint =
                  ({-# LINE 32 "AG\\Translate.ag" #-}
                   cJoint _diag _lhsIpos _lhsIlen
                   {-# LINE 409 "Ag.hs" #-}
                   )
              _lhsOmlen =
                  ({-# LINE 33 "AG\\Translate.ag" #-}
                   maxLength _diag
                   {-# LINE 414 "Ag.hs" #-}
                   )
              _diag =
                  ({-# LINE 16 "AG\\Translate.ag" #-}
                   Program p_ l_
                   {-# LINE 419 "Ag.hs" #-}
                   )
              _lhsOdiag =
                  ({-# LINE 16 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 424 "Ag.hs" #-}
                   )
          in  ( _lhsOcjoint,_lhsOcmd,_lhsOdiag,_lhsOejoint,_lhsOmlen)))
sem_Diag__Platform :: Ident ->
                      T_Diag_
sem_Diag__Platform m_ =
    (\ _lhsIlen
       _lhsIpos ->
         (let _lhsOcmd :: ([Command])
              _lhsOejoint :: ((Double,Double))
              _lhsOcjoint :: ((Double,Double))
              _lhsOmlen :: ((Double))
              _lhsOdiag :: Diag_
              _lhsOcmd =
                  ({-# LINE 26 "AG\\Translate.ag" #-}
                   platform _lhsIpos _lhsIlen m_
                   {-# LINE 440 "Ag.hs" #-}
                   )
              _lhsOejoint =
                  ({-# LINE 27 "AG\\Translate.ag" #-}
                   _lhsIpos
                   {-# LINE 445 "Ag.hs" #-}
                   )
              _lhsOcjoint =
                  ({-# LINE 28 "AG\\Translate.ag" #-}
                   _lhsIpos
                   {-# LINE 450 "Ag.hs" #-}
                   )
              _lhsOmlen =
                  ({-# LINE 29 "AG\\Translate.ag" #-}
                   maxLength _diag
                   {-# LINE 455 "Ag.hs" #-}
                   )
              _diag =
                  ({-# LINE 16 "AG\\Translate.ag" #-}
                   Platform m_
                   {-# LINE 460 "Ag.hs" #-}
                   )
              _lhsOdiag =
                  ({-# LINE 16 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 465 "Ag.hs" #-}
                   )
          in  ( _lhsOcjoint,_lhsOcmd,_lhsOdiag,_lhsOejoint,_lhsOmlen)))
sem_Diag__Interpreter :: Ident ->
                         Ident ->
                         Ident ->
                         T_Diag_
sem_Diag__Interpreter i_ l_ m_ =
    (\ _lhsIlen
       _lhsIpos ->
         (let _lhsOcmd :: ([Command])
              _lhsOejoint :: ((Double,Double))
              _lhsOcjoint :: ((Double,Double))
              _lhsOmlen :: ((Double))
              _lhsOdiag :: Diag_
              _lhsOcmd =
                  ({-# LINE 34 "AG\\Translate.ag" #-}
                   interpreter _lhsIpos _lhsIlen i_ l_ m_
                   {-# LINE 483 "Ag.hs" #-}
                   )
              _lhsOejoint =
                  ({-# LINE 35 "AG\\Translate.ag" #-}
                   eJoint _diag _lhsIpos _lhsIlen
                   {-# LINE 488 "Ag.hs" #-}
                   )
              _lhsOcjoint =
                  ({-# LINE 36 "AG\\Translate.ag" #-}
                   cJoint _diag _lhsIpos _lhsIlen
                   {-# LINE 493 "Ag.hs" #-}
                   )
              _lhsOmlen =
                  ({-# LINE 37 "AG\\Translate.ag" #-}
                   maxLength _diag
                   {-# LINE 498 "Ag.hs" #-}
                   )
              _diag =
                  ({-# LINE 16 "AG\\Translate.ag" #-}
                   Interpreter i_ l_ m_
                   {-# LINE 503 "Ag.hs" #-}
                   )
              _lhsOdiag =
                  ({-# LINE 16 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 508 "Ag.hs" #-}
                   )
          in  ( _lhsOcjoint,_lhsOcmd,_lhsOdiag,_lhsOejoint,_lhsOmlen)))
sem_Diag__Compiler :: Ident ->
                      Ident ->
                      Ident ->
                      Ident ->
                      T_Diag_
sem_Diag__Compiler c_ l1_ l2_ m_ =
    (\ _lhsIlen
       _lhsIpos ->
         (let _lhsOcmd :: ([Command])
              _lhsOejoint :: ((Double,Double))
              _lhsOcjoint :: ((Double,Double))
              _lhsOmlen :: ((Double))
              _lhsOdiag :: Diag_
              _lhsOcmd =
                  ({-# LINE 38 "AG\\Translate.ag" #-}
                   compiler _lhsIpos _lhsIlen c_ l1_ l2_ m_
                   {-# LINE 527 "Ag.hs" #-}
                   )
              _lhsOejoint =
                  ({-# LINE 39 "AG\\Translate.ag" #-}
                   eJoint _diag _lhsIpos _lhsIlen
                   {-# LINE 532 "Ag.hs" #-}
                   )
              _lhsOcjoint =
                  ({-# LINE 40 "AG\\Translate.ag" #-}
                   cJoint _diag _lhsIpos _lhsIlen
                   {-# LINE 537 "Ag.hs" #-}
                   )
              _lhsOmlen =
                  ({-# LINE 41 "AG\\Translate.ag" #-}
                   maxLength _diag
                   {-# LINE 542 "Ag.hs" #-}
                   )
              _diag =
                  ({-# LINE 16 "AG\\Translate.ag" #-}
                   Compiler c_ l1_ l2_ m_
                   {-# LINE 547 "Ag.hs" #-}
                   )
              _lhsOdiag =
                  ({-# LINE 16 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 552 "Ag.hs" #-}
                   )
          in  ( _lhsOcjoint,_lhsOcmd,_lhsOdiag,_lhsOejoint,_lhsOmlen)))
sem_Diag__Execute :: T_Diag ->
                     T_Diag ->
                     T_Diag_
sem_Diag__Execute d1_ d2_ =
    (\ _lhsIlen
       _lhsIpos ->
         (let _lhsOcmd :: ([Command])
              _lhsOcjoint :: ((Double,Double))
              _lhsOejoint :: ((Double,Double))
              _d1Opos :: ((Double,Double))
              _d2Opos :: ((Double,Double))
              _lhsOmlen :: ((Double))
              _d1Olen :: ((Double))
              _d2Olen :: ((Double))
              _lhsOdiag :: Diag_
              _d1Icjoint :: ((Double,Double))
              _d1Icmd :: ([Command])
              _d1Idiag :: Diag
              _d1Iejoint :: ((Double,Double))
              _d1Imlen :: ((Double))
              _d2Icjoint :: ((Double,Double))
              _d2Icmd :: ([Command])
              _d2Idiag :: Diag
              _d2Iejoint :: ((Double,Double))
              _d2Imlen :: ((Double))
              _lhsOcmd =
                  ({-# LINE 42 "AG\\Translate.ag" #-}
                   _d1Icmd ++ _d2Icmd
                   {-# LINE 583 "Ag.hs" #-}
                   )
              _lhsOcjoint =
                  ({-# LINE 43 "AG\\Translate.ag" #-}
                   _d2Icjoint
                   {-# LINE 588 "Ag.hs" #-}
                   )
              _lhsOejoint =
                  ({-# LINE 44 "AG\\Translate.ag" #-}
                   _d2Iejoint
                   {-# LINE 593 "Ag.hs" #-}
                   )
              _d1Opos =
                  ({-# LINE 45 "AG\\Translate.ag" #-}
                   _lhsIpos
                   {-# LINE 598 "Ag.hs" #-}
                   )
              _d2Opos =
                  ({-# LINE 46 "AG\\Translate.ag" #-}
                   _d1Iejoint
                   {-# LINE 603 "Ag.hs" #-}
                   )
              _lhsOmlen =
                  ({-# LINE 47 "AG\\Translate.ag" #-}
                   max _d1Imlen _d2Imlen
                   {-# LINE 608 "Ag.hs" #-}
                   )
              _d1Olen =
                  ({-# LINE 48 "AG\\Translate.ag" #-}
                   _lhsIlen
                   {-# LINE 613 "Ag.hs" #-}
                   )
              _d2Olen =
                  ({-# LINE 49 "AG\\Translate.ag" #-}
                   _lhsIlen
                   {-# LINE 618 "Ag.hs" #-}
                   )
              _diag =
                  ({-# LINE 16 "AG\\Translate.ag" #-}
                   Execute _d1Idiag _d2Idiag
                   {-# LINE 623 "Ag.hs" #-}
                   )
              _lhsOdiag =
                  ({-# LINE 16 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 628 "Ag.hs" #-}
                   )
              ( _d1Icjoint,_d1Icmd,_d1Idiag,_d1Iejoint,_d1Imlen) =
                  d1_ _d1Olen _d1Opos
              ( _d2Icjoint,_d2Icmd,_d2Idiag,_d2Iejoint,_d2Imlen) =
                  d2_ _d2Olen _d2Opos
          in  ( _lhsOcjoint,_lhsOcmd,_lhsOdiag,_lhsOejoint,_lhsOmlen)))
sem_Diag__Compile :: T_Diag ->
                     T_Diag ->
                     T_Diag_
sem_Diag__Compile d1_ d2_ =
    (\ _lhsIlen
       _lhsIpos ->
         (let _lhsOcmd :: ([Command])
              _lhsOcjoint :: ((Double,Double))
              _lhsOejoint :: ((Double,Double))
              _d1Opos :: ((Double,Double))
              _d2Opos :: ((Double,Double))
              _lhsOmlen :: ((Double))
              _d1Olen :: ((Double))
              _d2Olen :: ((Double))
              _lhsOdiag :: Diag_
              _d1Icjoint :: ((Double,Double))
              _d1Icmd :: ([Command])
              _d1Idiag :: Diag
              _d1Iejoint :: ((Double,Double))
              _d1Imlen :: ((Double))
              _d2Icjoint :: ((Double,Double))
              _d2Icmd :: ([Command])
              _d2Idiag :: Diag
              _d2Iejoint :: ((Double,Double))
              _d2Imlen :: ((Double))
              _cpos =
                  ({-# LINE 50 "AG\\Translate.ag" #-}
                   (fst _d1Icjoint + (3 * _lhsIlen), snd _d1Icjoint)
                   {-# LINE 663 "Ag.hs" #-}
                   )
              _cmpl =
                  ({-# LINE 51 "AG\\Translate.ag" #-}
                   compile (dtd_ _d1Idiag) (dtd_ _d2Idiag)
                   {-# LINE 668 "Ag.hs" #-}
                   )
              _lhsOcmd =
                  ({-# LINE 52 "AG\\Translate.ag" #-}
                   _d1Icmd ++ _d2Icmd ++ compiled _cmpl     _cpos     _lhsIlen
                   {-# LINE 673 "Ag.hs" #-}
                   )
              _lhsOcjoint =
                  ({-# LINE 53 "AG\\Translate.ag" #-}
                   cJoint _cmpl     _cpos     _lhsIlen
                   {-# LINE 678 "Ag.hs" #-}
                   )
              _lhsOejoint =
                  ({-# LINE 54 "AG\\Translate.ag" #-}
                   eJoint _cmpl     _cpos     _lhsIlen
                   {-# LINE 683 "Ag.hs" #-}
                   )
              _d1Opos =
                  ({-# LINE 55 "AG\\Translate.ag" #-}
                   _lhsIpos
                   {-# LINE 688 "Ag.hs" #-}
                   )
              _d2Opos =
                  ({-# LINE 56 "AG\\Translate.ag" #-}
                   _d1Icjoint
                   {-# LINE 693 "Ag.hs" #-}
                   )
              _lhsOmlen =
                  ({-# LINE 57 "AG\\Translate.ag" #-}
                   maximum [_d1Imlen, _d2Imlen, arrowLength]
                   {-# LINE 698 "Ag.hs" #-}
                   )
              _d1Olen =
                  ({-# LINE 58 "AG\\Translate.ag" #-}
                   _lhsIlen
                   {-# LINE 703 "Ag.hs" #-}
                   )
              _d2Olen =
                  ({-# LINE 59 "AG\\Translate.ag" #-}
                   _lhsIlen
                   {-# LINE 708 "Ag.hs" #-}
                   )
              _diag =
                  ({-# LINE 16 "AG\\Translate.ag" #-}
                   Compile _d1Idiag _d2Idiag
                   {-# LINE 713 "Ag.hs" #-}
                   )
              _lhsOdiag =
                  ({-# LINE 16 "AG\\Translate.ag" #-}
                   _diag
                   {-# LINE 718 "Ag.hs" #-}
                   )
              ( _d1Icjoint,_d1Icmd,_d1Idiag,_d1Iejoint,_d1Imlen) =
                  d1_ _d1Olen _d1Opos
              ( _d2Icjoint,_d2Icmd,_d2Idiag,_d2Iejoint,_d2Imlen) =
                  d2_ _d2Olen _d2Opos
          in  ( _lhsOcjoint,_lhsOcmd,_lhsOdiag,_lhsOejoint,_lhsOmlen)))
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
              {-# LINE 756 "Ag.hs" #-}
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
              {-# LINE 767 "Ag.hs" #-}
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
              {-# LINE 778 "Ag.hs" #-}
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
         _dOlen :: ((Double))
         _dIcjoint :: ((Double,Double))
         _dIcmd :: ([Command])
         _dIdiag :: Diag
         _dIejoint :: ((Double,Double))
         _dImlen :: ((Double))
         _lhsOpic =
             ({-# LINE 10 "AG\\Translate.ag" #-}
              Picture (500,500) _dIcmd
              {-# LINE 812 "Ag.hs" #-}
              )
         _dOpos =
             ({-# LINE 11 "AG\\Translate.ag" #-}
              (-200,500)
              {-# LINE 817 "Ag.hs" #-}
              )
         _dOlen =
             ({-# LINE 12 "AG\\Translate.ag" #-}
              _dImlen
              {-# LINE 822 "Ag.hs" #-}
              )
         ( _dIcjoint,_dIcmd,_dIdiag,_dIejoint,_dImlen) =
             d_ _dOlen _dOpos
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
              {-# LINE 855 "Ag.hs" #-}
              )
         ( _cmdsIpp) =
             cmds_
     in  ( _lhsOpp))