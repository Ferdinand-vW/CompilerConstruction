

-- UUAGC 0.9.52.1 (CCO/HM/AG.ag)
module CCO.HM.AG where

{-# LINE 2 "CCO\\HM\\..\\AG\\AHM.ag" #-}

import CCO.Tree (Tree (fromTree, toTree))
import qualified CCO.Tree as T (ATerm (App))
import CCO.Tree.Parser (parseTree, app, arg)
{-# LINE 12 "CCO/HM/AG.hs" #-}

{-# LINE 2 "CCO\\HM\\..\\AG\\HM.ag" #-}

import CCO.SourcePos
import CCO.Tree                   (Tree (fromTree, toTree))
import qualified CCO.Tree as T    (ATerm (App))
import CCO.Tree.Parser            (parseTree, app, arg)
import Control.Applicative        (Applicative ((<*>)), (<$>))
{-# LINE 21 "CCO/HM/AG.hs" #-}
{-# LINE 19 "CCO\\HM\\AG\\ToANormal.ag" #-}


pullOutLets :: ATm -> ATm
pullOutLets (AIf (ALet x lt1 lt2) t2 t3) = ALet x lt1 (pullOutLets $ AIf lt2 t2 t3)
pullOutLets (AIf (AApp at1 at2) t2 t3) = ALet (letName at1 at2) (AApp at1 at2) (AIf (AVar $ letName at1 at2) t2 t3)
pullOutLets atm = atm

removeDup :: [String] -> ATm -> ATm
removeDup env (ALet x t1 t2)
    | x `elem` env = removeDup env t2
    | otherwise    = ALet x t1 $ removeDup (x:env) t2
removeDup _ tm = tm

eitherAApp :: ATm -> ATm -> Bool
eitherAApp (AApp _ _) _ = True
eitherAApp _ (AApp _ _) = True
eitherAApp _ _ = False

transform :: ATm -> ATm
transform (AApp (ALet x lt1 lt2) t2) = ALet x lt1 $ transform $ AApp lt2 t2
transform (AApp t1 (ALet x rt1 rt2)) = ALet x rt1 $ transform $ AApp t1 rt2
transform (AApp t1 (ACons x l)) = ALet (getName (ACons x l)) (ACons x l) (transform $ AApp t1 (AVar $ getName (ACons x l)))
transform tm = newLets tm

newLets :: ATm -> ATm
newLets (AApp (AApp lt1 lt2) t2) = 
        ALet (letName lt1 lt2) (AApp lt1 lt2) $ newLets $
            AApp (AVar $ letName lt1 lt2) t2
newLets (AApp t1 (AApp rt1 rt2)) =
         ALet (letName rt1 rt2) (AApp rt1 rt2) $ newLets $
            AApp t1 $ AVar $ letName rt1 rt2
newLets tm = tm

getName :: ATm -> String
getName (ANat i) = show i
getName (ACons x l) = getName x ++ getName l
getName (ANil) = "nil"
getName (APrim x _ _) = x
getName (AVar x) = x
getName (ALam x _) = x
getName (ALet x _ _) = x
getname _ = ""

letName :: ATm -> ATm -> String
letName tm1 tm2 = (getName tm1) ++ (getName tm2)

{-# LINE 69 "CCO/HM/AG.hs" #-}

{-# LINE 9 "CCO\\HM\\..\\AG\\AHM.ag" #-}

instance Tree ATm where
  fromTree (ANat x)        = T.App "ANat" [fromTree x]
  fromTree (AVar x)        = T.App "AVar" [fromTree x]
  fromTree (ANil)          = T.App "ANil" [] 
  fromTree (ACons t1 t2)   = T.App "ACons" [fromTree t1, fromTree t2]
  fromTree (APrim f t1 t2) = T.App "APrim" [fromTree f, fromTree t1, fromTree t2]
  fromTree (ALam x t1)     = T.App "ALam" [fromTree x, fromTree t1]
  fromTree (AApp t1 t2)    = T.App "AApp" [fromTree t1, fromTree t2]
  fromTree (ALet x t1 t2)  = T.App "ALet" [fromTree x, fromTree t1, fromTree t2]
  fromTree (AIf exp t1 t2) = T.App "AIf" [fromTree exp, fromTree t1, fromTree t2]

  toTree = parseTree [ app "ANat" (ANat <$> arg                )
                     , app "AVar" (AVar <$> arg                )
                     , app "ANil" (pure ANil                   )
                     , app "ACons" (ACons <$> arg <*> arg      )
                     , app "APrim" (APrim <$> arg <*> arg <*> arg)
                     , app "ALam" (ALam <$> arg <*> arg        )
                     , app "AApp" (AApp <$> arg <*> arg        )
                     , app "ALet" (ALet <$> arg <*> arg <*> arg)
                     , app "AIf" (AIf <$> arg <*> arg <*> arg)
                     ]

{-# LINE 95 "CCO/HM/AG.hs" #-}

{-# LINE 39 "CCO\\HM\\..\\AG\\AHM.ag" #-}

type Var = String
{-# LINE 100 "CCO/HM/AG.hs" #-}

{-# LINE 11 "CCO\\HM\\..\\AG\\HM.ag" #-}

instance Tree Tm where
  fromTree (Tm pos t) = T.App "Tm" [fromTree pos, fromTree t]
  toTree = parseTree [app "Tm" (Tm <$> arg <*> arg)]

instance Tree Tm_ where
  fromTree (Nat x)       = T.App "Nat" [fromTree x]
  fromTree (Var x)       = T.App "Var" [fromTree x]
  fromTree Nil           = T.App "Nil" []
  fromTree (Cons t1 t2)   = T.App "Cons" [fromTree t1, fromTree t2]
  fromTree (Prim x t1 t2) = T.App "Prim" [fromTree x,fromTree t1, fromTree t2] --Added here
  fromTree (Lam x t1)    = T.App "Lam" [fromTree x, fromTree t1]
  fromTree (App t1 t2)   = T.App "App" [fromTree t1, fromTree t2]
  fromTree (Let x t1 t2) = T.App "Let" [fromTree x, fromTree t1, fromTree t2]
  fromTree (If exp t1 t2) = T.App "If" [fromTree exp, fromTree t1, fromTree t2]

  toTree = parseTree [ app "Nat" (Nat <$> arg                )
                     , app "Var" (Var <$> arg                )
                     , app "Nil" (pure Nil                   )
                     , app "Cons" (Cons <$> arg <*> arg      )
                     , app "Prim" (Prim <$> arg <*> arg <*> arg)
                     , app "Lam" (Lam <$> arg <*> arg        )
                     , app "App" (App <$> arg <*> arg        )
                     , app "Let" (Let <$> arg <*> arg <*> arg)
                     , app "If" (Let <$> arg <*> arg <*> arg)
                     ]

{-# LINE 130 "CCO/HM/AG.hs" #-}
-- ATm ---------------------------------------------------------
data ATm = ANat (Int)
         | AVar (Var)
         | ANil
         | ACons (ATm) (ATm)
         | APrim (Var) (ATm) (ATm)
         | ALam (Var) (ATm)
         | AApp (ATm) (ATm)
         | ALet (Var) (ATm) (ATm)
         | AIf (ATm) (ATm) (ATm)
-- cata
sem_ATm :: ATm ->
           T_ATm
sem_ATm (ANat _i) =
    (sem_ATm_ANat _i)
sem_ATm (AVar _x) =
    (sem_ATm_AVar _x)
sem_ATm (ANil) =
    (sem_ATm_ANil)
sem_ATm (ACons _t1 _t2) =
    (sem_ATm_ACons (sem_ATm _t1) (sem_ATm _t2))
sem_ATm (APrim _f _t1 _t2) =
    (sem_ATm_APrim _f (sem_ATm _t1) (sem_ATm _t2))
sem_ATm (ALam _x _t1) =
    (sem_ATm_ALam _x (sem_ATm _t1))
sem_ATm (AApp _t1 _t2) =
    (sem_ATm_AApp (sem_ATm _t1) (sem_ATm _t2))
sem_ATm (ALet _x _t1 _t2) =
    (sem_ATm_ALet _x (sem_ATm _t1) (sem_ATm _t2))
sem_ATm (AIf _exp _t1 _t2) =
    (sem_ATm_AIf (sem_ATm _exp) (sem_ATm _t1) (sem_ATm _t2))
-- semantic domain
type T_ATm = ( )
data Inh_ATm = Inh_ATm {}
data Syn_ATm = Syn_ATm {}
wrap_ATm :: T_ATm ->
            Inh_ATm ->
            Syn_ATm
wrap_ATm sem (Inh_ATm) =
    (let ( ) = sem
     in  (Syn_ATm))
sem_ATm_ANat :: Int ->
                T_ATm
sem_ATm_ANat i_ =
    (let
     in  ( ))
sem_ATm_AVar :: Var ->
                T_ATm
sem_ATm_AVar x_ =
    (let
     in  ( ))
sem_ATm_ANil :: T_ATm
sem_ATm_ANil =
    (let
     in  ( ))
sem_ATm_ACons :: T_ATm ->
                 T_ATm ->
                 T_ATm
sem_ATm_ACons t1_ t2_ =
    (let
     in  ( ))
sem_ATm_APrim :: Var ->
                 T_ATm ->
                 T_ATm ->
                 T_ATm
sem_ATm_APrim f_ t1_ t2_ =
    (let
     in  ( ))
sem_ATm_ALam :: Var ->
                T_ATm ->
                T_ATm
sem_ATm_ALam x_ t1_ =
    (let
     in  ( ))
sem_ATm_AApp :: T_ATm ->
                T_ATm ->
                T_ATm
sem_ATm_AApp t1_ t2_ =
    (let
     in  ( ))
sem_ATm_ALet :: Var ->
                T_ATm ->
                T_ATm ->
                T_ATm
sem_ATm_ALet x_ t1_ t2_ =
    (let
     in  ( ))
sem_ATm_AIf :: T_ATm ->
               T_ATm ->
               T_ATm ->
               T_ATm
sem_ATm_AIf exp_ t1_ t2_ =
    (let
     in  ( ))
-- Tm ----------------------------------------------------------
data Tm = Tm (SourcePos) (Tm_)
-- cata
sem_Tm :: Tm ->
          T_Tm
sem_Tm (Tm _pos _t) =
    (sem_Tm_Tm _pos (sem_Tm_ _t))
-- semantic domain
type T_Tm = ( ATm)
data Inh_Tm = Inh_Tm {}
data Syn_Tm = Syn_Tm {tm_Syn_Tm :: ATm}
wrap_Tm :: T_Tm ->
           Inh_Tm ->
           Syn_Tm
wrap_Tm sem (Inh_Tm) =
    (let ( _lhsOtm) = sem
     in  (Syn_Tm _lhsOtm))
sem_Tm_Tm :: SourcePos ->
             T_Tm_ ->
             T_Tm
sem_Tm_Tm pos_ t_ =
    (let _lhsOtm :: ATm
         _tItm :: ATm
         _lhsOtm =
             ({-# LINE 7 "CCO\\HM\\AG\\ToANormal.ag" #-}
              _tItm
              {-# LINE 251 "CCO/HM/AG.hs" #-}
              )
         ( _tItm) =
             t_
     in  ( _lhsOtm))
-- Tm_ ---------------------------------------------------------
data Tm_ = Nat (Int)
         | Var (Var)
         | Nil
         | Cons (Tm) (Tm)
         | Prim (Var) (Tm) (Tm)
         | Lam (Var) (Tm)
         | App (Tm) (Tm)
         | Let (Var) (Tm) (Tm)
         | If (Tm) (Tm) (Tm)
-- cata
sem_Tm_ :: Tm_ ->
           T_Tm_
sem_Tm_ (Nat _i) =
    (sem_Tm__Nat _i)
sem_Tm_ (Var _x) =
    (sem_Tm__Var _x)
sem_Tm_ (Nil) =
    (sem_Tm__Nil)
sem_Tm_ (Cons _t1 _t2) =
    (sem_Tm__Cons (sem_Tm _t1) (sem_Tm _t2))
sem_Tm_ (Prim _f _t1 _t2) =
    (sem_Tm__Prim _f (sem_Tm _t1) (sem_Tm _t2))
sem_Tm_ (Lam _x _t1) =
    (sem_Tm__Lam _x (sem_Tm _t1))
sem_Tm_ (App _t1 _t2) =
    (sem_Tm__App (sem_Tm _t1) (sem_Tm _t2))
sem_Tm_ (Let _x _t1 _t2) =
    (sem_Tm__Let _x (sem_Tm _t1) (sem_Tm _t2))
sem_Tm_ (If _exp _t1 _t2) =
    (sem_Tm__If (sem_Tm _exp) (sem_Tm _t1) (sem_Tm _t2))
-- semantic domain
type T_Tm_ = ( ATm)
data Inh_Tm_ = Inh_Tm_ {}
data Syn_Tm_ = Syn_Tm_ {tm_Syn_Tm_ :: ATm}
wrap_Tm_ :: T_Tm_ ->
            Inh_Tm_ ->
            Syn_Tm_
wrap_Tm_ sem (Inh_Tm_) =
    (let ( _lhsOtm) = sem
     in  (Syn_Tm_ _lhsOtm))
sem_Tm__Nat :: Int ->
               T_Tm_
sem_Tm__Nat i_ =
    (let _lhsOtm :: ATm
         _lhsOtm =
             ({-# LINE 10 "CCO\\HM\\AG\\ToANormal.ag" #-}
              ANat i_
              {-# LINE 304 "CCO/HM/AG.hs" #-}
              )
     in  ( _lhsOtm))
sem_Tm__Var :: Var ->
               T_Tm_
sem_Tm__Var x_ =
    (let _lhsOtm :: ATm
         _lhsOtm =
             ({-# LINE 11 "CCO\\HM\\AG\\ToANormal.ag" #-}
              AVar x_
              {-# LINE 314 "CCO/HM/AG.hs" #-}
              )
     in  ( _lhsOtm))
sem_Tm__Nil :: T_Tm_
sem_Tm__Nil =
    (let _lhsOtm :: ATm
         _lhsOtm =
             ({-# LINE 12 "CCO\\HM\\AG\\ToANormal.ag" #-}
              ANil
              {-# LINE 323 "CCO/HM/AG.hs" #-}
              )
     in  ( _lhsOtm))
sem_Tm__Cons :: T_Tm ->
                T_Tm ->
                T_Tm_
sem_Tm__Cons t1_ t2_ =
    (let _lhsOtm :: ATm
         _t1Itm :: ATm
         _t2Itm :: ATm
         _lhsOtm =
             ({-# LINE 13 "CCO\\HM\\AG\\ToANormal.ag" #-}
              ACons _t1Itm _t2Itm
              {-# LINE 336 "CCO/HM/AG.hs" #-}
              )
         ( _t1Itm) =
             t1_
         ( _t2Itm) =
             t2_
     in  ( _lhsOtm))
sem_Tm__Prim :: Var ->
                T_Tm ->
                T_Tm ->
                T_Tm_
sem_Tm__Prim f_ t1_ t2_ =
    (let _lhsOtm :: ATm
         _t1Itm :: ATm
         _t2Itm :: ATm
         _lhsOtm =
             ({-# LINE 14 "CCO\\HM\\AG\\ToANormal.ag" #-}
              APrim f_ _t1Itm _t2Itm
              {-# LINE 354 "CCO/HM/AG.hs" #-}
              )
         ( _t1Itm) =
             t1_
         ( _t2Itm) =
             t2_
     in  ( _lhsOtm))
sem_Tm__Lam :: Var ->
               T_Tm ->
               T_Tm_
sem_Tm__Lam x_ t1_ =
    (let _lhsOtm :: ATm
         _t1Itm :: ATm
         _lhsOtm =
             ({-# LINE 15 "CCO\\HM\\AG\\ToANormal.ag" #-}
              ALam x_ _t1Itm
              {-# LINE 370 "CCO/HM/AG.hs" #-}
              )
         ( _t1Itm) =
             t1_
     in  ( _lhsOtm))
sem_Tm__App :: T_Tm ->
               T_Tm ->
               T_Tm_
sem_Tm__App t1_ t2_ =
    (let _lhsOtm :: ATm
         _t1Itm :: ATm
         _t2Itm :: ATm
         _lhsOtm =
             ({-# LINE 16 "CCO\\HM\\AG\\ToANormal.ag" #-}
              removeDup [] $ transform (AApp _t1Itm _t2Itm)
              {-# LINE 385 "CCO/HM/AG.hs" #-}
              )
         ( _t1Itm) =
             t1_
         ( _t2Itm) =
             t2_
     in  ( _lhsOtm))
sem_Tm__Let :: Var ->
               T_Tm ->
               T_Tm ->
               T_Tm_
sem_Tm__Let x_ t1_ t2_ =
    (let _lhsOtm :: ATm
         _t1Itm :: ATm
         _t2Itm :: ATm
         _lhsOtm =
             ({-# LINE 17 "CCO\\HM\\AG\\ToANormal.ag" #-}
              ALet x_ _t1Itm _t2Itm
              {-# LINE 403 "CCO/HM/AG.hs" #-}
              )
         ( _t1Itm) =
             t1_
         ( _t2Itm) =
             t2_
     in  ( _lhsOtm))
sem_Tm__If :: T_Tm ->
              T_Tm ->
              T_Tm ->
              T_Tm_
sem_Tm__If exp_ t1_ t2_ =
    (let _lhsOtm :: ATm
         _expItm :: ATm
         _t1Itm :: ATm
         _t2Itm :: ATm
         _lhsOtm =
             ({-# LINE 18 "CCO\\HM\\AG\\ToANormal.ag" #-}
              pullOutLets $ AIf _expItm _t1Itm _t2Itm
              {-# LINE 422 "CCO/HM/AG.hs" #-}
              )
         ( _expItm) =
             exp_
         ( _t1Itm) =
             t1_
         ( _t2Itm) =
             t2_
     in  ( _lhsOtm))