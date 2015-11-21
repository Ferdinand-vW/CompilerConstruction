

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

{-# LINE 34 "AG\\Translate.ag" #-}



platform :: (Double, Double) -> String -> [Command]
platform (x,y) m = [
            Put (x, y + 15) $ Line (5, -3) 25,
            Put (x + 25,y) $ Line (5, 3) 25,
            Put (x, y + 15) $ Line (0, 1) 15,
            Put (x, y + 30) $ Line (1, 0) 50,
            Put (x + 50, y + 30) $ Line (0,-1) 15,
            Put (x, y + 15) $ Makebox (50, 15) m
        ]

program ::  (Double, Double) ->  String -> String -> [Command]
program (x,y) p l = [
                Put (x + 7.5, y) $ Line (1,0) 50,
                Put (x + 7.5, y) $ Line (0,-1) 15,
                Put (x + 7.5, y + 15) $ Line (-1, 2) 7.5,
                Put (x + 57.5, y + 15) $ Line (1, 2) 7.5,
                Put (x + 57.5, y) $ Line (0,1) 15,
                Put (x, y + 30) $ Line (1,0) 65,
                Put (x + 7.5, y + 15) $ Makebox (50, 15) p,
                Put (x + 7.5, y) $ Makebox (50, 15) l
            ]


interpreter :: (Double, Double) -> String -> String -> String -> [Command]
interpreter (x,y) i l m = [ 
                        Put (x, y) $ Framebox (50, 30) "", 
                        Put (x, y + 20) $ Makebox (50, 10) i,
                        Put (x, y + 10) $ Makebox (50, 10) l,
                        Put (x, y) $ Makebox (50,10) m
                    ]

compiler :: (Double, Double) -> String -> String -> String -> String -> [Command] 
compiler (x,y) c l1 l2 m = [
                        Put (x + 50, y) $ Line (0, 1) 20,
                        Put (x + 50, y + 20) $ Line (-1, 0) 50,
                        Put (x, y + 20) $ Line (0, 1) 10,
                        Put (x, y + 30) $ Line (1, 0) 150,
                        Put (x + 150, y + 30) $ Line (0,-1) 10,
                        Put (x + 150, y + 20) $ Line (-1,0) 50,
                        Put (x + 100, y + 20) $ Line (0,-1) 20,
                        Put (x + 100, y) $ Line (-1, 0) 50,
                        Put (x, y + 20) $ Makebox (50,10) l1,
                        Put (x + 50, y + 20) $ Makebox (50,10) "$\\longrightarrow$",
                        Put (x + 100, y + 20) $ Makebox (50,10) l2,
                        Put (x + 55, y + 10) $ Makebox (50,10) c,
                        Put (x + 50, y) $ Makebox (50,10) m
                    ]


{-# LINE 95 "Ag.hs" #-}

{-# LINE 16 "..\\Diag.ag" #-}

type Ident = String
{-# LINE 100 "Ag.hs" #-}

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
{-# LINE 126 "Ag.hs" #-}

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
{-# LINE 147 "Ag.hs" #-}
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
              {-# LINE 174 "Ag.hs" #-}
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
              {-# LINE 206 "Ag.hs" #-}
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
              {-# LINE 219 "Ag.hs" #-}
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
type T_Diag = ((Double,Double)) ->
              ( ((Double,Double)),((Double,Double)),([Command]))
data Inh_Diag = Inh_Diag {pos_Inh_Diag :: ((Double,Double))}
data Syn_Diag = Syn_Diag {cjoint_Syn_Diag :: ((Double,Double)),ejoint_Syn_Diag :: ((Double,Double)),pic_Syn_Diag :: ([Command])}
wrap_Diag :: T_Diag ->
             Inh_Diag ->
             Syn_Diag
wrap_Diag sem (Inh_Diag _lhsIpos) =
    (let ( _lhsOcjoint,_lhsOejoint,_lhsOpic) = sem _lhsIpos
     in  (Syn_Diag _lhsOcjoint _lhsOejoint _lhsOpic))
sem_Diag_Diag :: SourcePos ->
                 T_Diag_ ->
                 T_Diag
sem_Diag_Diag pos_ d_ =
    (\ _lhsIpos ->
         (let _lhsOpic :: ([Command])
              _lhsOcjoint :: ((Double,Double))
              _lhsOejoint :: ((Double,Double))
              _dOpos :: ((Double,Double))
              _dIcjoint :: ((Double,Double))
              _dIejoint :: ((Double,Double))
              _dIpic :: ([Command])
              _lhsOpic =
                  ({-# LINE 4 "AG\\Translate.ag" #-}
                   _dIpic
                   {-# LINE 255 "Ag.hs" #-}
                   )
              _lhsOcjoint =
                  ({-# LINE 5 "AG\\Translate.ag" #-}
                   _dIcjoint
                   {-# LINE 260 "Ag.hs" #-}
                   )
              _lhsOejoint =
                  ({-# LINE 6 "AG\\Translate.ag" #-}
                   _dIejoint
                   {-# LINE 265 "Ag.hs" #-}
                   )
              _dOpos =
                  ({-# LINE 7 "AG\\Translate.ag" #-}
                   _lhsIpos
                   {-# LINE 270 "Ag.hs" #-}
                   )
              ( _dIcjoint,_dIejoint,_dIpic) =
                  d_ _dOpos
          in  ( _lhsOcjoint,_lhsOejoint,_lhsOpic)))
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
type T_Diag_ = ((Double,Double)) ->
               ( ((Double,Double)),((Double,Double)),([Command]))
data Inh_Diag_ = Inh_Diag_ {pos_Inh_Diag_ :: ((Double,Double))}
data Syn_Diag_ = Syn_Diag_ {cjoint_Syn_Diag_ :: ((Double,Double)),ejoint_Syn_Diag_ :: ((Double,Double)),pic_Syn_Diag_ :: ([Command])}
wrap_Diag_ :: T_Diag_ ->
              Inh_Diag_ ->
              Syn_Diag_
wrap_Diag_ sem (Inh_Diag_ _lhsIpos) =
    (let ( _lhsOcjoint,_lhsOejoint,_lhsOpic) = sem _lhsIpos
     in  (Syn_Diag_ _lhsOcjoint _lhsOejoint _lhsOpic))
sem_Diag__Program :: Ident ->
                     Ident ->
                     T_Diag_
sem_Diag__Program p_ l_ =
    (\ _lhsIpos ->
         (let _lhsOpic :: ([Command])
              _lhsOejoint :: ((Double,Double))
              _lhsOcjoint :: ((Double,Double))
              _lhsOpic =
                  ({-# LINE 14 "AG\\Translate.ag" #-}
                   program _lhsIpos p_ l_
                   {-# LINE 319 "Ag.hs" #-}
                   )
              _lhsOejoint =
                  ({-# LINE 15 "AG\\Translate.ag" #-}
                   ((fst _lhsIpos) +  7.5, (snd _lhsIpos) - 30)
                   {-# LINE 324 "Ag.hs" #-}
                   )
              _lhsOcjoint =
                  ({-# LINE 16 "AG\\Translate.ag" #-}
                   ((fst _lhsIpos) + 57.5, (snd _lhsIpos) - 15)
                   {-# LINE 329 "Ag.hs" #-}
                   )
          in  ( _lhsOcjoint,_lhsOejoint,_lhsOpic)))
sem_Diag__Platform :: Ident ->
                      T_Diag_
sem_Diag__Platform m_ =
    (\ _lhsIpos ->
         (let _lhsOpic :: ([Command])
              _lhsOcjoint :: ((Double,Double))
              _lhsOejoint :: ((Double,Double))
              _lhsOpic =
                  ({-# LINE 11 "AG\\Translate.ag" #-}
                   platform _lhsIpos m_
                   {-# LINE 342 "Ag.hs" #-}
                   )
              _lhsOcjoint =
                  ({-# LINE 12 "AG\\Translate.ag" #-}
                   (fst _lhsIpos + 50, snd _lhsIpos - 25)
                   {-# LINE 347 "Ag.hs" #-}
                   )
              _lhsOejoint =
                  ({-# LINE 13 "AG\\Translate.ag" #-}
                   (fst _lhsIpos, snd _lhsIpos - 25)
                   {-# LINE 352 "Ag.hs" #-}
                   )
          in  ( _lhsOcjoint,_lhsOejoint,_lhsOpic)))
sem_Diag__Interpreter :: Ident ->
                         Ident ->
                         Ident ->
                         T_Diag_
sem_Diag__Interpreter i_ l_ m_ =
    (\ _lhsIpos ->
         (let _lhsOpic :: ([Command])
              _lhsOejoint :: ((Double,Double))
              _lhsOcjoint :: ((Double,Double))
              _lhsOpic =
                  ({-# LINE 17 "AG\\Translate.ag" #-}
                   interpreter _lhsIpos i_ l_ m_
                   {-# LINE 367 "Ag.hs" #-}
                   )
              _lhsOejoint =
                  ({-# LINE 18 "AG\\Translate.ag" #-}
                   (fst _lhsIpos, snd _lhsIpos - 30)
                   {-# LINE 372 "Ag.hs" #-}
                   )
              _lhsOcjoint =
                  ({-# LINE 19 "AG\\Translate.ag" #-}
                   (fst _lhsIpos + 50, snd _lhsIpos - 20)
                   {-# LINE 377 "Ag.hs" #-}
                   )
          in  ( _lhsOcjoint,_lhsOejoint,_lhsOpic)))
sem_Diag__Compiler :: Ident ->
                      Ident ->
                      Ident ->
                      Ident ->
                      T_Diag_
sem_Diag__Compiler c_ l1_ l2_ m_ =
    (\ _lhsIpos ->
         (let _lhsOpic :: ([Command])
              _lhsOejoint :: ((Double,Double))
              _lhsOcjoint :: ((Double,Double))
              _lhsOpic =
                  ({-# LINE 20 "AG\\Translate.ag" #-}
                   compiler _lhsIpos c_ l1_ l2_ m_
                   {-# LINE 393 "Ag.hs" #-}
                   )
              _lhsOejoint =
                  ({-# LINE 21 "AG\\Translate.ag" #-}
                   (fst _lhsIpos + 50, snd _lhsIpos - 30)
                   {-# LINE 398 "Ag.hs" #-}
                   )
              _lhsOcjoint =
                  ({-# LINE 22 "AG\\Translate.ag" #-}
                   (fst _lhsIpos + 50, snd _lhsIpos - 20)
                   {-# LINE 403 "Ag.hs" #-}
                   )
          in  ( _lhsOcjoint,_lhsOejoint,_lhsOpic)))
sem_Diag__Execute :: T_Diag ->
                     T_Diag ->
                     T_Diag_
sem_Diag__Execute d1_ d2_ =
    (\ _lhsIpos ->
         (let _lhsOpic :: ([Command])
              _lhsOcjoint :: ((Double,Double))
              _lhsOejoint :: ((Double,Double))
              _d1Opos :: ((Double,Double))
              _d2Opos :: ((Double,Double))
              _d1Icjoint :: ((Double,Double))
              _d1Iejoint :: ((Double,Double))
              _d1Ipic :: ([Command])
              _d2Icjoint :: ((Double,Double))
              _d2Iejoint :: ((Double,Double))
              _d2Ipic :: ([Command])
              _lhsOpic =
                  ({-# LINE 23 "AG\\Translate.ag" #-}
                   _d1Ipic ++ _d2Ipic
                   {-# LINE 425 "Ag.hs" #-}
                   )
              _lhsOcjoint =
                  ({-# LINE 24 "AG\\Translate.ag" #-}
                   _d2Icjoint
                   {-# LINE 430 "Ag.hs" #-}
                   )
              _lhsOejoint =
                  ({-# LINE 25 "AG\\Translate.ag" #-}
                   _d2Iejoint
                   {-# LINE 435 "Ag.hs" #-}
                   )
              _d1Opos =
                  ({-# LINE 26 "AG\\Translate.ag" #-}
                   _lhsIpos
                   {-# LINE 440 "Ag.hs" #-}
                   )
              _d2Opos =
                  ({-# LINE 27 "AG\\Translate.ag" #-}
                   _d1Iejoint
                   {-# LINE 445 "Ag.hs" #-}
                   )
              ( _d1Icjoint,_d1Iejoint,_d1Ipic) =
                  d1_ _d1Opos
              ( _d2Icjoint,_d2Iejoint,_d2Ipic) =
                  d2_ _d2Opos
          in  ( _lhsOcjoint,_lhsOejoint,_lhsOpic)))
sem_Diag__Compile :: T_Diag ->
                     T_Diag ->
                     T_Diag_
sem_Diag__Compile d1_ d2_ =
    (\ _lhsIpos ->
         (let _lhsOcjoint :: ((Double,Double))
              _lhsOejoint :: ((Double,Double))
              _d1Opos :: ((Double,Double))
              _d2Opos :: ((Double,Double))
              _lhsOpic :: ([Command])
              _d1Icjoint :: ((Double,Double))
              _d1Iejoint :: ((Double,Double))
              _d1Ipic :: ([Command])
              _d2Icjoint :: ((Double,Double))
              _d2Iejoint :: ((Double,Double))
              _d2Ipic :: ([Command])
              _lhsOcjoint =
                  ({-# LINE 28 "AG\\Translate.ag" #-}
                   (fst _d1Icjoint + (3 * 50), snd _d1Icjoint)
                   {-# LINE 471 "Ag.hs" #-}
                   )
              _lhsOejoint =
                  ({-# LINE 29 "AG\\Translate.ag" #-}
                   _d2Iejoint
                   {-# LINE 476 "Ag.hs" #-}
                   )
              _d1Opos =
                  ({-# LINE 30 "AG\\Translate.ag" #-}
                   _lhsIpos
                   {-# LINE 481 "Ag.hs" #-}
                   )
              _d2Opos =
                  ({-# LINE 31 "AG\\Translate.ag" #-}
                   _d1Icjoint
                   {-# LINE 486 "Ag.hs" #-}
                   )
              _lhsOpic =
                  ({-# LINE 4 "AG\\Translate.ag" #-}
                   _d1Ipic ++ _d2Ipic
                   {-# LINE 491 "Ag.hs" #-}
                   )
              ( _d1Icjoint,_d1Iejoint,_d1Ipic) =
                  d1_ _d1Opos
              ( _d2Icjoint,_d2Iejoint,_d2Ipic) =
                  d2_ _d2Opos
          in  ( _lhsOcjoint,_lhsOejoint,_lhsOpic)))
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
              {-# LINE 529 "Ag.hs" #-}
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
              {-# LINE 540 "Ag.hs" #-}
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
              {-# LINE 551 "Ag.hs" #-}
              )
     in  ( _lhsOpp))
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
              {-# LINE 582 "Ag.hs" #-}
              )
         ( _cmdsIpp) =
             cmds_
     in  ( _lhsOpp))