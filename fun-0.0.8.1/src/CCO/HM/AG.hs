

-- UUAGC 0.9.52.1 (CCO/HM/AG.ag)
module CCO.HM.AG where

{-# LINE 2 "CCO\\HM\\..\\AG\\HM.ag" #-}

import CCO.SourcePos
import CCO.Tree                   (Tree (fromTree, toTree))
import qualified CCO.Tree as T    (ATerm (App))
import CCO.Tree.Parser            (parseTree, app, arg)
import Control.Applicative        (Applicative ((<*>)), (<$>))
{-# LINE 14 "CCO/HM/AG.hs" #-}
{-# LINE 20 "CCO\\HM\\..\\Core\\AG\\ToANormal.ag" #-}


functie :: Tm_ -> Tm -> Tm
functie x y = y

translate :: Tm_ -> Tm_ -> Tm_
translate (HVar x) (HVar y) | x==y = HVar "Matthew"
                          | otherwise = (HVar x)
translate x y = x


--Ik krijg het niet correct werkend met uuagc-.-, dus zal wel de stappen opschrijven wat ik van plan was. Ik ben ook nog wel op skype denk ik.
--Controleer HApp of @t2 een HApp is
--Als dit zo is; controleer of @t1 en bij de eerste tm van @t2, dus @t2.t1, allebei een var zijn en gelijk aan elkaar zijn.
-- Als dit zo is voeg een HLet  toe met de naam @t1 ++ @t2.t2 (Als dit een var is). En doe de HApp als waarde
-- Vervolgens in de in van de let @t1 ++ @t2.t2 doe je HApp met @t1 (@t1 ++ @t2.t2)
-- In exaples/haakjes.out en examples/correct.out zie je het verschil van hoe het moet worden en hoe het moet worden


-- 
{-# LINE 36 "CCO/HM/AG.hs" #-}

{-# LINE 11 "CCO\\HM\\..\\AG\\HM.ag" #-}

instance Tree Tm where
  fromTree (Tm pos t) = T.App "Tm" [fromTree pos, fromTree t]
  toTree = parseTree [app "Tm" (Tm <$> arg <*> arg)]

instance Tree Tm_ where
  fromTree (HNat x)       = T.App "HNat" [fromTree x]
  fromTree (HVar x)       = T.App "HVar" [fromTree x]
  fromTree (HLam x t1)    = T.App "HLam" [fromTree x, fromTree t1]
  fromTree (HApp t1 t2)   = T.App "HApp" [fromTree t1, fromTree t2]
  fromTree (HLet x t1 t2) = T.App "HLet" [fromTree x, fromTree t1, fromTree t2]

  toTree = parseTree [ app "HNat" (HNat <$> arg                )
                     , app "HVar" (HVar <$> arg                )
                     , app "HLam" (HLam <$> arg <*> arg        )
                     , app "HApp" (HApp <$> arg <*> arg        )
                     , app "HLet" (HLet <$> arg <*> arg <*> arg)
                     ]

{-# LINE 58 "CCO/HM/AG.hs" #-}

{-# LINE 36 "CCO\\HM\\..\\AG\\HM.ag" #-}

type Var = String    -- ^ Type of variables.
{-# LINE 63 "CCO/HM/AG.hs" #-}
-- Tm ----------------------------------------------------------
data Tm = Tm (SourcePos) (Tm_)
-- cata
sem_Tm :: Tm ->
          T_Tm
sem_Tm (Tm _pos _t) =
    (sem_Tm_Tm _pos (sem_Tm_ _t))
-- semantic domain
type T_Tm = Tm_ ->
            ( Tm,Tm_)
data Inh_Tm = Inh_Tm {previousTm_Inh_Tm :: Tm_}
data Syn_Tm = Syn_Tm {tm_Syn_Tm :: Tm,tm__Syn_Tm :: Tm_}
wrap_Tm :: T_Tm ->
           Inh_Tm ->
           Syn_Tm
wrap_Tm sem (Inh_Tm _lhsIpreviousTm) =
    (let ( _lhsOtm,_lhsOtm_) = sem _lhsIpreviousTm
     in  (Syn_Tm _lhsOtm _lhsOtm_))
sem_Tm_Tm :: SourcePos ->
             T_Tm_ ->
             T_Tm
sem_Tm_Tm pos_ t_ =
    (\ _lhsIpreviousTm ->
         (let _lhsOtm :: Tm
              _lhsOtm_ :: Tm_
              _tOpreviousTm :: Tm_
              _tItm_ :: Tm_
              _lhsOtm =
                  ({-# LINE 9 "CCO\\HM\\..\\Core\\AG\\ToANormal.ag" #-}
                   Tm pos_ _tItm_
                   {-# LINE 94 "CCO/HM/AG.hs" #-}
                   )
              _lhsOtm_ =
                  ({-# LINE 2 "CCO\\HM\\..\\Core\\AG\\ToANormal.ag" #-}
                   _tItm_
                   {-# LINE 99 "CCO/HM/AG.hs" #-}
                   )
              _tOpreviousTm =
                  ({-# LINE 3 "CCO\\HM\\..\\Core\\AG\\ToANormal.ag" #-}
                   _lhsIpreviousTm
                   {-# LINE 104 "CCO/HM/AG.hs" #-}
                   )
              ( _tItm_) =
                  t_ _tOpreviousTm
          in  ( _lhsOtm,_lhsOtm_)))
-- Tm_ ---------------------------------------------------------
data Tm_ = HNat (Int)
         | HVar (Var)
         | HLam (Var) (Tm)
         | HApp (Tm) (Tm)
         | HLet (Var) (Tm) (Tm)
-- cata
sem_Tm_ :: Tm_ ->
           T_Tm_
sem_Tm_ (HNat _i) =
    (sem_Tm__HNat _i)
sem_Tm_ (HVar _x) =
    (sem_Tm__HVar _x)
sem_Tm_ (HLam _x _t1) =
    (sem_Tm__HLam _x (sem_Tm _t1))
sem_Tm_ (HApp _t1 _t2) =
    (sem_Tm__HApp (sem_Tm _t1) (sem_Tm _t2))
sem_Tm_ (HLet _x _t1 _t2) =
    (sem_Tm__HLet _x (sem_Tm _t1) (sem_Tm _t2))
-- semantic domain
type T_Tm_ = Tm_ ->
             ( Tm_)
data Inh_Tm_ = Inh_Tm_ {previousTm_Inh_Tm_ :: Tm_}
data Syn_Tm_ = Syn_Tm_ {tm__Syn_Tm_ :: Tm_}
wrap_Tm_ :: T_Tm_ ->
            Inh_Tm_ ->
            Syn_Tm_
wrap_Tm_ sem (Inh_Tm_ _lhsIpreviousTm) =
    (let ( _lhsOtm_) = sem _lhsIpreviousTm
     in  (Syn_Tm_ _lhsOtm_))
sem_Tm__HNat :: Int ->
                T_Tm_
sem_Tm__HNat i_ =
    (\ _lhsIpreviousTm ->
         (let _lhsOtm_ :: Tm_
              _lhsOtm_ =
                  ({-# LINE 12 "CCO\\HM\\..\\Core\\AG\\ToANormal.ag" #-}
                   HNat i_
                   {-# LINE 147 "CCO/HM/AG.hs" #-}
                   )
          in  ( _lhsOtm_)))
sem_Tm__HVar :: Var ->
                T_Tm_
sem_Tm__HVar x_ =
    (\ _lhsIpreviousTm ->
         (let _lhsOtm_ :: Tm_
              _lhsOtm_ =
                  ({-# LINE 13 "CCO\\HM\\..\\Core\\AG\\ToANormal.ag" #-}
                   HVar x_
                   {-# LINE 158 "CCO/HM/AG.hs" #-}
                   )
          in  ( _lhsOtm_)))
sem_Tm__HLam :: Var ->
                T_Tm ->
                T_Tm_
sem_Tm__HLam x_ t1_ =
    (\ _lhsIpreviousTm ->
         (let _lhsOtm_ :: Tm_
              _t1OpreviousTm :: Tm_
              _t1Itm :: Tm
              _t1Itm_ :: Tm_
              _lhsOtm_ =
                  ({-# LINE 14 "CCO\\HM\\..\\Core\\AG\\ToANormal.ag" #-}
                   HLam x_ _t1Itm
                   {-# LINE 173 "CCO/HM/AG.hs" #-}
                   )
              _t1OpreviousTm =
                  ({-# LINE 3 "CCO\\HM\\..\\Core\\AG\\ToANormal.ag" #-}
                   _lhsIpreviousTm
                   {-# LINE 178 "CCO/HM/AG.hs" #-}
                   )
              ( _t1Itm,_t1Itm_) =
                  t1_ _t1OpreviousTm
          in  ( _lhsOtm_)))
sem_Tm__HApp :: T_Tm ->
                T_Tm ->
                T_Tm_
sem_Tm__HApp t1_ t2_ =
    (\ _lhsIpreviousTm ->
         (let _lhsOtm_ :: Tm_
              _t2OpreviousTm :: Tm_
              _t1OpreviousTm :: Tm_
              _t1Itm :: Tm
              _t1Itm_ :: Tm_
              _t2Itm :: Tm
              _t2Itm_ :: Tm_
              _lhsOtm_ =
                  ({-# LINE 15 "CCO\\HM\\..\\Core\\AG\\ToANormal.ag" #-}
                   HApp (functie _t1Itm_ _t1Itm) _t2Itm
                   {-# LINE 198 "CCO/HM/AG.hs" #-}
                   )
              _t2OpreviousTm =
                  ({-# LINE 16 "CCO\\HM\\..\\Core\\AG\\ToANormal.ag" #-}
                   _t1Itm_
                   {-# LINE 203 "CCO/HM/AG.hs" #-}
                   )
              _t1OpreviousTm =
                  ({-# LINE 3 "CCO\\HM\\..\\Core\\AG\\ToANormal.ag" #-}
                   _lhsIpreviousTm
                   {-# LINE 208 "CCO/HM/AG.hs" #-}
                   )
              ( _t1Itm,_t1Itm_) =
                  t1_ _t1OpreviousTm
              ( _t2Itm,_t2Itm_) =
                  t2_ _t2OpreviousTm
          in  ( _lhsOtm_)))
sem_Tm__HLet :: Var ->
                T_Tm ->
                T_Tm ->
                T_Tm_
sem_Tm__HLet x_ t1_ t2_ =
    (\ _lhsIpreviousTm ->
         (let _lhsOtm_ :: Tm_
              _t1OpreviousTm :: Tm_
              _t2OpreviousTm :: Tm_
              _t1Itm :: Tm
              _t1Itm_ :: Tm_
              _t2Itm :: Tm
              _t2Itm_ :: Tm_
              _lhsOtm_ =
                  ({-# LINE 17 "CCO\\HM\\..\\Core\\AG\\ToANormal.ag" #-}
                   HLet x_ _t1Itm _t2Itm
                   {-# LINE 231 "CCO/HM/AG.hs" #-}
                   )
              _t1OpreviousTm =
                  ({-# LINE 3 "CCO\\HM\\..\\Core\\AG\\ToANormal.ag" #-}
                   _lhsIpreviousTm
                   {-# LINE 236 "CCO/HM/AG.hs" #-}
                   )
              _t2OpreviousTm =
                  ({-# LINE 3 "CCO\\HM\\..\\Core\\AG\\ToANormal.ag" #-}
                   _lhsIpreviousTm
                   {-# LINE 241 "CCO/HM/AG.hs" #-}
                   )
              ( _t1Itm,_t1Itm_) =
                  t1_ _t1OpreviousTm
              ( _t2Itm,_t2Itm_) =
                  t2_ _t2OpreviousTm
          in  ( _lhsOtm_)))