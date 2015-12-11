

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
{-# LINE 15 "CCO\\HM\\AG\\ToANormal.ag" #-}


removeDup :: [String] -> ATm -> ATm
removeDup env tm =
    case tm of
        ALet x t1 t2 -> if x `elem` env
                        then removeDup env t2
                        else ALet x t1 $ removeDup (x:env) t2
        AApp t1 t2 -> if eitherAApp t1 t2
                        then removeDup env (newLets (AApp t1 t2))
                      else tm
        _ -> tm

eitherAApp :: ATm -> ATm -> Bool
eitherAApp (AApp _ _) _ = True
eitherAApp _ (AApp _ _) = True
eitherAApp _ _ = False

transform :: ATm -> ATm
transform (AApp (ALet x lt1 lt2) t2) = ALet x lt1 $ transform $ AApp lt2 t2
transform (AApp t1 (ALet x rt1 rt2)) = ALet x rt1 $ transform $ AApp t1 rt2
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
getName (AVar x) = x
getName (ALam x _) = x
getName (ALet x _ _) = x
getname _ = ""

letName :: ATm -> ATm -> String
letName tm1 tm2 = (getName tm1) ++ (getName tm2)

{-# LINE 66 "CCO/HM/AG.hs" #-}

{-# LINE 9 "CCO\\HM\\..\\AG\\AHM.ag" #-}

instance Tree ATm where
  fromTree (ANat x)       = T.App "ANat" [fromTree x]
  fromTree (AVar x)       = T.App "AVar" [fromTree x]
  fromTree (ALam x t1)    = T.App "ALam" [fromTree x, fromTree t1]
  fromTree (AApp t1 t2)   = T.App "AApp" [fromTree t1, fromTree t2]
  fromTree (ALet x t1 t2) = T.App "ALet" [fromTree x, fromTree t1, fromTree t2]

  toTree = parseTree [ app "ANat" (ANat <$> arg                )
                     , app "AVar" (AVar <$> arg                )
                     , app "ALam" (ALam <$> arg <*> arg        )
                     , app "AApp" (AApp <$> arg <*> arg        )
                     , app "ALet" (ALet <$> arg <*> arg <*> arg)
                     ]

{-# LINE 84 "CCO/HM/AG.hs" #-}

{-# LINE 31 "CCO\\HM\\..\\AG\\AHM.ag" #-}

type Var = String
{-# LINE 89 "CCO/HM/AG.hs" #-}

{-# LINE 11 "CCO\\HM\\..\\AG\\HM.ag" #-}

instance Tree Tm where
  fromTree (Tm pos t) = T.App "Tm" [fromTree pos, fromTree t]
  toTree = parseTree [app "Tm" (Tm <$> arg <*> arg)]

instance Tree Tm_ where
  fromTree (Nat x)       = T.App "Nat" [fromTree x]
  fromTree (Var x)       = T.App "Var" [fromTree x]
  fromTree (Lam x t1)    = T.App "Lam" [fromTree x, fromTree t1]
  fromTree (App t1 t2)   = T.App "App" [fromTree t1, fromTree t2]
  fromTree (Let x t1 t2) = T.App "Let" [fromTree x, fromTree t1, fromTree t2]

  toTree = parseTree [ app "Nat" (Nat <$> arg                )
                     , app "Var" (Var <$> arg                )
                     , app "Lam" (Lam <$> arg <*> arg        )
                     , app "App" (App <$> arg <*> arg        )
                     , app "Let" (Let <$> arg <*> arg <*> arg)
                     ]

{-# LINE 111 "CCO/HM/AG.hs" #-}
-- ATm ---------------------------------------------------------
data ATm = ANat (Int)
         | AVar (Var)
         | ALam (Var) (ATm)
         | AApp (ATm) (ATm)
         | ALet (Var) (ATm) (ATm)
-- cata
sem_ATm :: ATm ->
           T_ATm
sem_ATm (ANat _i) =
    (sem_ATm_ANat _i)
sem_ATm (AVar _x) =
    (sem_ATm_AVar _x)
sem_ATm (ALam _x _t1) =
    (sem_ATm_ALam _x (sem_ATm _t1))
sem_ATm (AApp _t1 _t2) =
    (sem_ATm_AApp (sem_ATm _t1) (sem_ATm _t2))
sem_ATm (ALet _x _t1 _t2) =
    (sem_ATm_ALet _x (sem_ATm _t1) (sem_ATm _t2))
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
              {-# LINE 196 "CCO/HM/AG.hs" #-}
              )
         ( _tItm) =
             t_
     in  ( _lhsOtm))
-- Tm_ ---------------------------------------------------------
data Tm_ = Nat (Int)
         | Var (Var)
         | Lam (Var) (Tm)
         | App (Tm) (Tm)
         | Let (Var) (Tm) (Tm)
-- cata
sem_Tm_ :: Tm_ ->
           T_Tm_
sem_Tm_ (Nat _i) =
    (sem_Tm__Nat _i)
sem_Tm_ (Var _x) =
    (sem_Tm__Var _x)
sem_Tm_ (Lam _x _t1) =
    (sem_Tm__Lam _x (sem_Tm _t1))
sem_Tm_ (App _t1 _t2) =
    (sem_Tm__App (sem_Tm _t1) (sem_Tm _t2))
sem_Tm_ (Let _x _t1 _t2) =
    (sem_Tm__Let _x (sem_Tm _t1) (sem_Tm _t2))
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
              {-# LINE 237 "CCO/HM/AG.hs" #-}
              )
     in  ( _lhsOtm))
sem_Tm__Var :: Var ->
               T_Tm_
sem_Tm__Var x_ =
    (let _lhsOtm :: ATm
         _lhsOtm =
             ({-# LINE 11 "CCO\\HM\\AG\\ToANormal.ag" #-}
              AVar x_
              {-# LINE 247 "CCO/HM/AG.hs" #-}
              )
     in  ( _lhsOtm))
sem_Tm__Lam :: Var ->
               T_Tm ->
               T_Tm_
sem_Tm__Lam x_ t1_ =
    (let _lhsOtm :: ATm
         _t1Itm :: ATm
         _lhsOtm =
             ({-# LINE 12 "CCO\\HM\\AG\\ToANormal.ag" #-}
              ALam x_ _t1Itm
              {-# LINE 259 "CCO/HM/AG.hs" #-}
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
             ({-# LINE 13 "CCO\\HM\\AG\\ToANormal.ag" #-}
              removeDup [] $ transform (AApp _t1Itm _t2Itm)
              {-# LINE 274 "CCO/HM/AG.hs" #-}
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
             ({-# LINE 14 "CCO\\HM\\AG\\ToANormal.ag" #-}
              ALet x_ _t1Itm _t2Itm
              {-# LINE 292 "CCO/HM/AG.hs" #-}
              )
         ( _t1Itm) =
             t1_
         ( _t2Itm) =
             t2_
     in  ( _lhsOtm))