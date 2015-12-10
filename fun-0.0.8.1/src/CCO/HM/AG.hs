

-- UUAGC 0.9.52.1 (CCO/HM/AG.ag)
module CCO.HM.AG where

{-# LINE 2 "CCO\\HM\\..\\AG\\HM.ag" #-}

import CCO.SourcePos
import CCO.Tree                   (Tree (fromTree, toTree))
import qualified CCO.Tree as T    (ATerm (App))
import CCO.Tree.Parser            (parseTree, app, arg)
import Control.Applicative        (Applicative ((<*>)), (<$>))
{-# LINE 14 "CCO/HM/AG.hs" #-}
{-# LINE 20 "CCO\\HM\\AG\\ToANormal.ag" #-}


transform :: Tm_ -> Tm_
transform (HApp (Tm lpos (HLet x lt1 lt2)) (Tm rpos (HLet y rt1 rt2))) =
    HLet x lt1 $ Tm lpos $
        HLet y rt1 $ Tm rpos $
            transform $ HApp lt2 rt2
transform (HApp (Tm pos (HLet x lt1 lt2)) t2) = HLet x lt1 (Tm pos $ transform (HApp lt2 t2))
transform (HApp t1 (Tm pos (HLet x lt1 lt2))) = HLet x lt1 (Tm pos $ transform (HApp t1 lt2))
transform (HApp (Tm lpos (HApp lt1 lt2)) (Tm rpos (HApp rt1 rt2))) =
    HLet (letName lt1 lt2) (Tm lpos $ HApp lt1 lt2) $ Tm lpos $
        HLet (letName rt1 rt2) (Tm rpos $ HApp rt1 rt2) $ Tm rpos $
            HApp (Tm lpos $ HVar $ letName lt1 lt2) (Tm rpos $ HVar $ letName rt1 rt2)
transform (HApp (Tm lpos (HApp lt1 lt2)) t2) = HLet (letName lt1 lt2) (Tm lpos $ HApp lt1 lt2) (Tm lpos $ HApp (Tm lpos $ HVar $ letName lt1 lt2) t2)
transform (HApp t1 (Tm rpos (HApp rt1 rt2))) = HLet (letName rt1 rt2) (Tm rpos $ HApp rt1 rt2) (Tm rpos $ HApp t1 (Tm rpos $ HVar $ letName rt1 rt2))
transform tm = tm

letName :: Tm -> Tm -> String
letName tm1 tm2 = (getName $ tTm_ tm1) ++ (getName $ tTm_ tm2)

tTm_ :: Tm -> Tm_
tTm_ (Tm _ t) = t

getName :: Tm_ -> String
getName (HNat i) = show i
getName (HVar x) = x
getName (HLam x _) = x
getName (HLet x _ _) = x
getname _ = ""

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
{-# LINE 64 "CCO/HM/AG.hs" #-}

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

{-# LINE 86 "CCO/HM/AG.hs" #-}

{-# LINE 36 "CCO\\HM\\..\\AG\\HM.ag" #-}

type Var = String    -- ^ Type of variables.
{-# LINE 91 "CCO/HM/AG.hs" #-}
-- Tm ----------------------------------------------------------
data Tm = Tm (SourcePos) (Tm_)
-- cata
sem_Tm :: Tm ->
          T_Tm
sem_Tm (Tm _pos _t) =
    (sem_Tm_Tm _pos (sem_Tm_ _t))
-- semantic domain
type T_Tm = ( Tm,Tm_)
data Inh_Tm = Inh_Tm {}
data Syn_Tm = Syn_Tm {tm_Syn_Tm :: Tm,tm__Syn_Tm :: Tm_}
wrap_Tm :: T_Tm ->
           Inh_Tm ->
           Syn_Tm
wrap_Tm sem (Inh_Tm) =
    (let ( _lhsOtm,_lhsOtm_) = sem
     in  (Syn_Tm _lhsOtm _lhsOtm_))
sem_Tm_Tm :: SourcePos ->
             T_Tm_ ->
             T_Tm
sem_Tm_Tm pos_ t_ =
    (let _lhsOtm :: Tm
         _lhsOtm_ :: Tm_
         _tItm_ :: Tm_
         _lhsOtm =
             ({-# LINE 10 "CCO\\HM\\AG\\ToANormal.ag" #-}
              Tm pos_ _tItm_
              {-# LINE 119 "CCO/HM/AG.hs" #-}
              )
         _lhsOtm_ =
             ({-# LINE 4 "CCO\\HM\\AG\\ToANormal.ag" #-}
              _tItm_
              {-# LINE 124 "CCO/HM/AG.hs" #-}
              )
         ( _tItm_) =
             t_
     in  ( _lhsOtm,_lhsOtm_))
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
type T_Tm_ = ( Tm_)
data Inh_Tm_ = Inh_Tm_ {}
data Syn_Tm_ = Syn_Tm_ {tm__Syn_Tm_ :: Tm_}
wrap_Tm_ :: T_Tm_ ->
            Inh_Tm_ ->
            Syn_Tm_
wrap_Tm_ sem (Inh_Tm_) =
    (let ( _lhsOtm_) = sem
     in  (Syn_Tm_ _lhsOtm_))
sem_Tm__HNat :: Int ->
                T_Tm_
sem_Tm__HNat i_ =
    (let _lhsOtm_ :: Tm_
         _lhsOtm_ =
             ({-# LINE 13 "CCO\\HM\\AG\\ToANormal.ag" #-}
              HNat i_
              {-# LINE 165 "CCO/HM/AG.hs" #-}
              )
     in  ( _lhsOtm_))
sem_Tm__HVar :: Var ->
                T_Tm_
sem_Tm__HVar x_ =
    (let _lhsOtm_ :: Tm_
         _lhsOtm_ =
             ({-# LINE 14 "CCO\\HM\\AG\\ToANormal.ag" #-}
              HVar x_
              {-# LINE 175 "CCO/HM/AG.hs" #-}
              )
     in  ( _lhsOtm_))
sem_Tm__HLam :: Var ->
                T_Tm ->
                T_Tm_
sem_Tm__HLam x_ t1_ =
    (let _lhsOtm_ :: Tm_
         _t1Itm :: Tm
         _t1Itm_ :: Tm_
         _lhsOtm_ =
             ({-# LINE 15 "CCO\\HM\\AG\\ToANormal.ag" #-}
              HLam x_ _t1Itm
              {-# LINE 188 "CCO/HM/AG.hs" #-}
              )
         ( _t1Itm,_t1Itm_) =
             t1_
     in  ( _lhsOtm_))
sem_Tm__HApp :: T_Tm ->
                T_Tm ->
                T_Tm_
sem_Tm__HApp t1_ t2_ =
    (let _lhsOtm_ :: Tm_
         _t1Itm :: Tm
         _t1Itm_ :: Tm_
         _t2Itm :: Tm
         _t2Itm_ :: Tm_
         _lhsOtm_ =
             ({-# LINE 16 "CCO\\HM\\AG\\ToANormal.ag" #-}
              transform (HApp _t1Itm _t2Itm)
              {-# LINE 205 "CCO/HM/AG.hs" #-}
              )
         ( _t1Itm,_t1Itm_) =
             t1_
         ( _t2Itm,_t2Itm_) =
             t2_
     in  ( _lhsOtm_))
sem_Tm__HLet :: Var ->
                T_Tm ->
                T_Tm ->
                T_Tm_
sem_Tm__HLet x_ t1_ t2_ =
    (let _lhsOtm_ :: Tm_
         _t1Itm :: Tm
         _t1Itm_ :: Tm_
         _t2Itm :: Tm
         _t2Itm_ :: Tm_
         _lhsOtm_ =
             ({-# LINE 17 "CCO\\HM\\AG\\ToANormal.ag" #-}
              HLet x_ _t1Itm _t2Itm
              {-# LINE 225 "CCO/HM/AG.hs" #-}
              )
         ( _t1Itm,_t1Itm_) =
             t1_
         ( _t2Itm,_t2Itm_) =
             t2_
     in  ( _lhsOtm_))