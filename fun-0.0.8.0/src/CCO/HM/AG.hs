

-- UUAGC 0.9.52.1 (CCO/HM/AG.ag)
module CCO.HM.AG where

{-# LINE 2 "CCO\\HM\\..\\AG\\HM.ag" #-}

import CCO.SourcePos
import CCO.Tree                   (Tree (fromTree, toTree))
import qualified CCO.Tree as T    (ATerm (App))
import CCO.Tree.Parser            (parseTree, app, arg)
import Control.Applicative        (Applicative ((<*>)), (<$>))
{-# LINE 14 "CCO/HM/AG.hs" #-}
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

{-# LINE 35 "CCO/HM/AG.hs" #-}

{-# LINE 36 "CCO\\HM\\..\\AG\\HM.ag" #-}

type Var = String    -- ^ Type of variables.
{-# LINE 40 "CCO/HM/AG.hs" #-}
-- Tm ----------------------------------------------------------
data Tm = Tm (SourcePos) (Tm_)
-- cata
sem_Tm :: Tm ->
          T_Tm
sem_Tm (Tm _pos _t) =
    (sem_Tm_Tm _pos (sem_Tm_ _t))
-- semantic domain
type T_Tm = ( )
data Inh_Tm = Inh_Tm {}
data Syn_Tm = Syn_Tm {}
wrap_Tm :: T_Tm ->
           Inh_Tm ->
           Syn_Tm
wrap_Tm sem (Inh_Tm) =
    (let ( ) = sem
     in  (Syn_Tm))
sem_Tm_Tm :: SourcePos ->
             T_Tm_ ->
             T_Tm
sem_Tm_Tm pos_ t_ =
    (let
     in  ( ))
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
type T_Tm_ = ( )
data Inh_Tm_ = Inh_Tm_ {}
data Syn_Tm_ = Syn_Tm_ {}
wrap_Tm_ :: T_Tm_ ->
            Inh_Tm_ ->
            Syn_Tm_
wrap_Tm_ sem (Inh_Tm_) =
    (let ( ) = sem
     in  (Syn_Tm_))
sem_Tm__HNat :: Int ->
                T_Tm_
sem_Tm__HNat i_ =
    (let
     in  ( ))
sem_Tm__HVar :: Var ->
                T_Tm_
sem_Tm__HVar x_ =
    (let
     in  ( ))
sem_Tm__HLam :: Var ->
                T_Tm ->
                T_Tm_
sem_Tm__HLam x_ t1_ =
    (let
     in  ( ))
sem_Tm__HApp :: T_Tm ->
                T_Tm ->
                T_Tm_
sem_Tm__HApp t1_ t2_ =
    (let
     in  ( ))
sem_Tm__HLet :: Var ->
                T_Tm ->
                T_Tm ->
                T_Tm_
sem_Tm__HLet x_ t1_ t2_ =
    (let
     in  ( ))