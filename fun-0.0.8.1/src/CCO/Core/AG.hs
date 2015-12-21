

-- UUAGC 0.9.52.1 (CCO/Core/AG.ag)
module CCO.Core.AG where

import UHC.Util.Pretty
import UHC.Light.Compiler.Base.API    (defaultEHCOpts)
import UHC.Light.Compiler.CoreRun.API (printModule)
import CCO.Component

{-# LINE 2 "CCO\\Core\\AG\\Hm2Cr.ag" #-}

import Data.Maybe
{-# LINE 15 "CCO/Core/AG.hs" #-}

{-# LINE 2 "CCO\\Core\\AG\\ToCoreRun.ag" #-}

import           UHC.Light.Compiler.Base.API
import qualified UHC.Light.Compiler.CoreRun.API as CR
import qualified UHC.Light.Compiler.CoreRun.API.Internal as CRI
{-# LINE 22 "CCO/Core/AG.hs" #-}

{-# LINE 4 "CCO\\Core\\..\\AG\\AHM.ag" #-}

import CCO.Tree (Tree (fromTree, toTree))
import qualified CCO.Tree as T (ATerm (App))
import CCO.Tree.Parser (parseTree, app, arg)
{-# LINE 29 "CCO/Core/AG.hs" #-}

{-# LINE 2 "CCO\\Core\\..\\AG\\Core.ag" #-}

import qualified UHC.Light.Compiler.CoreRun.API as CR
{-# LINE 34 "CCO/Core/AG.hs" #-}
{-# LINE 1 "CCO\\Core\\AG\\EnvMap.ag" #-}

--We're using the map type to store refs of variables. 
--The int is used as offset
--The a is used as a key. 
--The b is the Ref 
type Map a b = (Int,[(a,b)])

--Next will return the next offset of a type and add this as a global ref
next :: Map String Ref -> Ref
next (n,_) = Glob n

getOffSet :: Map String Ref -> Int
getOffSet (n,_) = n

lookup'  :: String -> Map String Ref -> Maybe Ref
lookup' s (_,env) = lookup s env

--insertG will increment the offset and add the new global to the map
insertg :: Map String Ref -> String -> Map String Ref
insertg (n,xs) s = (n + 1,(s, Glob n) : xs)

--insertgWithValue is used to insert a new ref into the map
insertgWithValue :: Map String Ref -> String -> Ref -> Map String Ref
insertgWithValue (n,xs) s r = (n,(s,r):xs)

--insertl is used to insert a new Var with a Loc to the map and increment the offset.
insertl :: Map String Ref -> String -> Map String Ref
insertl (n,xs) s = (n + 1, (s, Loc 0 n) : xs)

--insertl' is used to insert a value into the map and increment the offset.
insertl' :: Map String Ref -> String -> Ref -> Map String Ref
insertl' (n,xs) s r = (n + 1,(s,r) : xs)

--incrEnv is used to increment the lvldiff and resets the offset
incrEnv :: Map String Ref -> Map String Ref
incrEnv (n,lenv) = (0, incr lenv)
  where incr [] = []
        incr ((s,(Loc lvl x)):xs) = (s,Loc (lvl + 1) x) : incr xs
        incr ((s,r):xs) = (s,r) : incr xs

--newOffSet is used to set a new offset 
newOffSet :: Map String Ref -> Int -> Map String Ref
newOffSet (n,lenv) i
    | i >= 1 = (i,lenv)
    | i < 1 = (i,lenv')
    where (_,lenv') = incrEnv (n,lenv)
{-# LINE 82 "CCO/Core/AG.hs" #-}

{-# LINE 1 "CCO\\Core\\AG\\BuildIn.ag" #-}

--This file is used to bind all of the builtin features we added to our language. These features are always added into the file.
--We added the following features: cons, nil, tail, head True, False.

--DefaultBinds is used in the Hm2cr.ag, to add all the bindings to the language.
defaultBinds :: BindL
defaultBinds = [nilBind,consBind,headBind,tailBind, isConsBind, isNilBind, trueBind, falseBind]


trueBind :: Bind
trueBind = Bind (Glob 7) True_

falseBind :: Bind
falseBind = Bind (Glob 6) False_

--IsNil as Glob 5, and is used to check if it is nil
isNilBind :: Bind
isNilBind = Bind (Glob 5) exp
  where exp = Lam [Loc 0 0] $
                Let (Bind (Loc 0 1) (Eval (SExp $ Var $ Loc 0 0))) $
                  Case (Var $ Tag $ (Loc 0 1)) $
                    [SExp $ Var $ Glob 6] ++ [SExp $ Var $ Glob 7]

--isCons has as ref Glob 4 and checks if it is a cons. It uses the Boolbinds to refer to the correct Ref.
isConsBind :: Bind
isConsBind = Bind (Glob 4) exp
  where exp = Lam [Loc 0 0] $
                Let (Bind (Loc 0 1) (Eval (SExp $ Var $ Loc 0 0))) $
                  Case (Var $ Tag $ (Loc 0 1)) $
                    [SExp $ Var $ Glob 7] ++ [SExp $ Var $ Glob 6]

--tailBinds has as ref Glob 3 and refers to Field 1, which is the tail of the list
tailBind :: Bind
tailBind = Bind (Glob 3) exp
  where exp = Lam [Loc 0 0] $
                Let (Bind (Loc 0 1) (Eval (SExp $ Var $ Loc 0 0))) $
                  Case (Var $ Tag $ Field 1 $ (Loc 0 1)) $
                    [SExp $ Var $ Glob 0] ++ [SExp $ Var $ Field 1 $ Loc 0 1]

--headBinds has as ref Glob 2 and refers to Field 0, which is the tail of the list
headBind :: Bind
headBind = Bind (Glob 2) exp
  where exp = Lam [Loc 0 0] $
                Let (Bind (Loc 0 1) (Eval (SExp $ Var $ Loc 0 0))) $
                  Case (Var $ Tag $ Field 1 $ (Loc 0 1)) $
                    [SExp $ Var $ Glob 0] ++ [SExp $ Var $ Field 0 $ Loc 0 1]

--consBind has as ref Glob 1 and makes a Cons
consBind :: Bind
consBind = Bind (Glob 1) exp
  where exp = Lam [Loc 0 0, Loc 0 1] 
                (Cons (Var $ Loc 0 0) (Var $ Loc 0 1))

nilBind :: Bind
nilBind = Bind (Glob 0) Nil
{-# LINE 140 "CCO/Core/AG.hs" #-}

{-# LINE 37 "CCO\\Core\\AG\\Hm2Cr.ag" #-}

natExp :: Int -> Exp
natExp i = SExp $ Int i

varExp :: Map String Ref -> Map String Ref -> String -> Exp
varExp genv lenv s = SExp $ findVarRef genv lenv s

varBind :: Map String Ref -> String -> Exp
varBind genv s = Eval $ SExp $ findVarRef' genv s

--First looks for the Ref in the local environment, if it cannot find it, then
--it will check the global environment. If it wasn't found, then the program crashes
findVarRef :: Map String Ref -> Map String Ref -> String -> SExp
findVarRef (_,genv) (_,lenv) s = Var $ fromMaybe (fromJust (lookup s genv))  (lookup s lenv)

--Same as findVarRef except for just a single environment
findVarRef' :: Map String Ref -> String -> SExp
findVarRef' (_,env) s = Var $ fromJust $ lookup s env

toSExp :: Exp -> SExp
toSExp (SExp s) = s
{-# LINE 164 "CCO/Core/AG.hs" #-}

{-# LINE 69 "CCO\\Core\\AG\\Hm2Cr.ag" #-}


nilBind' :: Map String Ref -> Exp
nilBind' genv = SExp $ findVarRef' genv "nil"

mkCons :: Map String Ref -> Map String Ref -> ATm -> Exp
mkCons genv lenv atm = consExp genv lenv offset atm (SExp $ Var $ Loc 0 offset)
  where
    offset = (listLength atm) - 1

consExp :: Map String Ref -> Map String Ref -> Int -> ATm -> Exp -> Exp
consExp genv lenv n (ACons t1 t2) t3 = case t2 of
                            ACons _ _ -> consExp genv lenv (n-1) t2 (Let (Bind curr (App cons [toSExp t1,Var prev])) t3)
                            ANil -> Let (Bind curr (App cons [toSExp t1, nil])) t3
                            AVar x -> Let (Bind curr (App cons [toSExp t1,ref x])) t3
      where
        cons = SExp $ findVarRef' genv "cons"
        nil = findVarRef' genv "nil"
        curr = Loc 0 n
        prev = Loc 0 (n - 1)
        toSExp (ANat n) = Int n
        toSExp (AVar x) = ref x
        ref x = findVarRef genv lenv x

listLength :: ATm -> Int
listLength (ACons _ l) = 1 + listLength l
listLength _ = 0

{-# LINE 195 "CCO/Core/AG.hs" #-}

{-# LINE 108 "CCO\\Core\\AG\\Hm2Cr.ag" #-}


--Consecutive lambda's are joined
joinLam :: RefL -> Exp -> Exp
joinLam xs (Lam ys t1) = Lam (xs ++ ys) t1
joinLam xs exp = Lam xs exp

updateEnv :: Map String Ref -> Int -> String -> Map String Ref
updateEnv lenv numArgs s = insertl' (newOffSet lenv (numArgs)) s (Loc 0 numArgs)

{-# LINE 208 "CCO/Core/AG.hs" #-}

{-# LINE 132 "CCO\\Core\\AG\\Hm2Cr.ag" #-}

appExp' :: Map String Ref -> Exp -> Exp -> Exp
appExp' lenv exp (Let b exp2) = Lam [] (appExp lenv exp (Let b exp2))
appExp' lenv exp exp2 = appExp lenv exp exp2

appExp :: Map String Ref -> Exp -> Exp -> Exp
appExp lenv exp (Let b exp2) = Let b (appExp lenv exp exp2)
appExp lenv (SExp sexp1) (SExp sexp2) = Let (Bind (Loc 0 $ getOffSet lenv) (Eval (SExp sexp2))) (
                                Let (Bind (Loc 0 $ getOffSet lenv + 1) (Eval (SExp sexp1))) $ 
                                    App (SExp $ Var $ Loc 0 (getOffSet lenv + 1)) [Var $ Loc 0 (getOffSet lenv)])


{-# LINE 223 "CCO/Core/AG.hs" #-}

{-# LINE 160 "CCO\\Core\\AG\\Hm2Cr.ag" #-}

insertRecursion :: Map String Ref -> Map String Ref -> String -> Int -> Map String Ref
insertRecursion genv lenv s lvl 
  | lvl == 0 = insertl' lenv s (getRef $ findVarRef' genv s)
  | otherwise = insertl lenv s

globalLet :: Map String Ref -> Int -> String -> Map String Ref
globalLet genv lvl x 
  | lvl == 0 = insertg genv x
  | otherwise = genv

localLetLvl :: Int -> Int
localLetLvl 0 = 1
localLetLvl lvl = lvl

localLet :: Map String Ref -> Int -> String -> Map String Ref
localLet lenv lvl x
  | lvl > 0 = insertl lenv x
  | otherwise = lenv

letExp :: Map String Ref -> Exp -> Exp -> Exp
letExp genv (Let b t1) exp2 = Let (Bind (next genv) (Lam [] (Let b t1))) exp2
letExp genv exp exp2 = Let (Bind (next genv) exp) exp2

letBinds :: Map String Ref -> ATm -> Exp -> BindL -> BindL
letBinds (n,_) (ALam _ _) exp t2binds = Bind (Glob n) exp : t2binds
letBinds (n,_) _ exp t2binds = Bind (Glob n) (Lam [] exp) : t2binds
{-# LINE 253 "CCO/Core/AG.hs" #-}

{-# LINE 198 "CCO\\Core\\AG\\Hm2Cr.ag" #-}

toPrim :: Exp -> SExp
toPrim (SExp s) = Var $ Loc 0 (off s)
              where off (Var (Loc l o)) = o  
{-# LINE 260 "CCO/Core/AG.hs" #-}

{-# LINE 221 "CCO\\Core\\AG\\Hm2Cr.ag" #-}


createCase :: Exp -> Exp -> Exp
createCase e1@(SExp s) e2 = Let (Bind (getRef s) (Eval e1)) e2

evalVar :: Exp -> Exp
evalVar v@(SExp (Var r)) = Eval v
evalVar exp = exp

getRef :: SExp -> Ref
getRef (Var x) = x
getRef _ = error "It did not solve"

incrOffSet' :: Map String Ref -> Int -> Map String Ref
incrOffSet' (n,xs) i = (n + i, xs)
{-# LINE 278 "CCO/Core/AG.hs" #-}

{-# LINE 10 "CCO\\Core\\..\\AG\\AHM.ag" #-}

instance Tree ATm where
  fromTree (ANat x)        = T.App "ANat"   [fromTree x]
  fromTree (AVar x)        = T.App "AVar"   [fromTree x]
  fromTree (ANil)          = T.App "ANil"   [] 
  fromTree (ACons t1 t2)   = T.App "ACons"  [fromTree t1, fromTree t2]
  fromTree (APrim f t1 t2) = T.App "APrim"  [fromTree f, fromTree t1, fromTree t2]
  fromTree (ALam x t1)     = T.App "ALam"   [fromTree x, fromTree t1]
  fromTree (AApp t1 t2)    = T.App "AApp"   [fromTree t1, fromTree t2]
  fromTree (ALet x t1 t2)  = T.App "ALet"   [fromTree x, fromTree t1, fromTree t2]
  fromTree (AIf exp t1 t2) = T.App "AIf"    [fromTree exp, fromTree t1, fromTree t2]

  toTree = parseTree [ app "ANat"  (ANat  <$> arg                )
                     , app "AVar"  (AVar  <$> arg                )
                     , app "ANil"  (pure ANil                    )
                     , app "ACons" (ACons <$> arg <*> arg        )
                     , app "APrim" (APrim <$> arg <*> arg <*> arg)
                     , app "ALam"  (ALam  <$> arg <*> arg        )
                     , app "AApp"  (AApp  <$> arg <*> arg        )
                     , app "ALet"  (ALet  <$> arg <*> arg <*> arg)
                     , app "AIf"   (AIf   <$> arg <*> arg <*> arg)
                     ]

{-# LINE 304 "CCO/Core/AG.hs" #-}

{-# LINE 40 "CCO\\Core\\..\\AG\\AHM.ag" #-}

type Var = String
{-# LINE 309 "CCO/Core/AG.hs" #-}

{-# LINE 30 "CCO\\Core\\AG.ag" #-}

crprinter :: Component Mod String
crprinter = component $ \mod -> do
  let crmod = crmod_Syn_Mod (wrap_Mod (sem_Mod mod) Inh_Mod)
  return $ show $ printModule defaultEHCOpts crmod
{-# LINE 317 "CCO/Core/AG.hs" #-}
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
         deriving ( Show)
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
type T_ATm = Int ->
             (Map String Ref) ->
             (Map String Ref) ->
             Int ->
             ( BindL,Exp,Ref,ATm)
data Inh_ATm = Inh_ATm {args_Inh_ATm :: Int,genv_Inh_ATm :: (Map String Ref),lenv_Inh_ATm :: (Map String Ref),lvl_Inh_ATm :: Int}
data Syn_ATm = Syn_ATm {binds_Syn_ATm :: BindL,exp_Syn_ATm :: Exp,main_Syn_ATm :: Ref,tm_Syn_ATm :: ATm}
wrap_ATm :: T_ATm ->
            Inh_ATm ->
            Syn_ATm
wrap_ATm sem (Inh_ATm _lhsIargs _lhsIgenv _lhsIlenv _lhsIlvl) =
    (let ( _lhsObinds,_lhsOexp,_lhsOmain,_lhsOtm) = sem _lhsIargs _lhsIgenv _lhsIlenv _lhsIlvl
     in  (Syn_ATm _lhsObinds _lhsOexp _lhsOmain _lhsOtm))
sem_ATm_ANat :: Int ->
                T_ATm
sem_ATm_ANat i_ =
    (\ _lhsIargs
       _lhsIgenv
       _lhsIlenv
       _lhsIlvl ->
         (let _lhsOmain :: Ref
              _lhsOexp :: Exp
              _lhsObinds :: BindL
              _lhsOtm :: ATm
              _lhsOmain =
                  ({-# LINE 30 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   next _lhsIgenv
                   {-# LINE 378 "CCO/Core/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   natExp i_
                   {-# LINE 383 "CCO/Core/AG.hs" #-}
                   )
              _lhsObinds =
                  ({-# LINE 32 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   [Bind (next _lhsIgenv) $ Lam [] $ natExp i_]
                   {-# LINE 388 "CCO/Core/AG.hs" #-}
                   )
              _tm =
                  ({-# LINE 20 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   ANat i_
                   {-# LINE 393 "CCO/Core/AG.hs" #-}
                   )
              _lhsOtm =
                  ({-# LINE 20 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tm
                   {-# LINE 398 "CCO/Core/AG.hs" #-}
                   )
          in  ( _lhsObinds,_lhsOexp,_lhsOmain,_lhsOtm)))
sem_ATm_AVar :: Var ->
                T_ATm
sem_ATm_AVar x_ =
    (\ _lhsIargs
       _lhsIgenv
       _lhsIlenv
       _lhsIlvl ->
         (let _lhsOmain :: Ref
              _lhsOexp :: Exp
              _lhsObinds :: BindL
              _lhsOtm :: ATm
              _lhsOmain =
                  ({-# LINE 33 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   next _lhsIgenv
                   {-# LINE 415 "CCO/Core/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 34 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   varExp _lhsIgenv _lhsIlenv x_
                   {-# LINE 420 "CCO/Core/AG.hs" #-}
                   )
              _lhsObinds =
                  ({-# LINE 35 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   [Bind (next _lhsIgenv) $ Lam [] $ varBind _lhsIgenv x_]
                   {-# LINE 425 "CCO/Core/AG.hs" #-}
                   )
              _tm =
                  ({-# LINE 20 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   AVar x_
                   {-# LINE 430 "CCO/Core/AG.hs" #-}
                   )
              _lhsOtm =
                  ({-# LINE 20 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tm
                   {-# LINE 435 "CCO/Core/AG.hs" #-}
                   )
          in  ( _lhsObinds,_lhsOexp,_lhsOmain,_lhsOtm)))
sem_ATm_ANil :: T_ATm
sem_ATm_ANil =
    (\ _lhsIargs
       _lhsIgenv
       _lhsIlenv
       _lhsIlvl ->
         (let _lhsOmain :: Ref
              _lhsOexp :: Exp
              _lhsObinds :: BindL
              _lhsOtm :: ATm
              _lhsOmain =
                  ({-# LINE 62 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   next _lhsIgenv
                   {-# LINE 451 "CCO/Core/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 63 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   SExp $ findVarRef' _lhsIgenv "nil"
                   {-# LINE 456 "CCO/Core/AG.hs" #-}
                   )
              _lhsObinds =
                  ({-# LINE 64 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   [Bind (next _lhsIgenv) $ nilBind' _lhsIgenv]
                   {-# LINE 461 "CCO/Core/AG.hs" #-}
                   )
              _tm =
                  ({-# LINE 20 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   ANil
                   {-# LINE 466 "CCO/Core/AG.hs" #-}
                   )
              _lhsOtm =
                  ({-# LINE 20 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tm
                   {-# LINE 471 "CCO/Core/AG.hs" #-}
                   )
          in  ( _lhsObinds,_lhsOexp,_lhsOmain,_lhsOtm)))
sem_ATm_ACons :: T_ATm ->
                 T_ATm ->
                 T_ATm
sem_ATm_ACons t1_ t2_ =
    (\ _lhsIargs
       _lhsIgenv
       _lhsIlenv
       _lhsIlvl ->
         (let _lhsOmain :: Ref
              _lhsOexp :: Exp
              _lhsObinds :: BindL
              _lhsOtm :: ATm
              _t1Oargs :: Int
              _t1Ogenv :: (Map String Ref)
              _t1Olenv :: (Map String Ref)
              _t1Olvl :: Int
              _t2Oargs :: Int
              _t2Ogenv :: (Map String Ref)
              _t2Olenv :: (Map String Ref)
              _t2Olvl :: Int
              _t1Ibinds :: BindL
              _t1Iexp :: Exp
              _t1Imain :: Ref
              _t1Itm :: ATm
              _t2Ibinds :: BindL
              _t2Iexp :: Exp
              _t2Imain :: Ref
              _t2Itm :: ATm
              _lhsOmain =
                  ({-# LINE 65 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   next _lhsIgenv
                   {-# LINE 505 "CCO/Core/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 66 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   mkCons _lhsIgenv _lhsIlenv (ACons _t1Itm _t2Itm)
                   {-# LINE 510 "CCO/Core/AG.hs" #-}
                   )
              _lhsObinds =
                  ({-# LINE 67 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   [Bind (next _lhsIgenv) $ mkCons _lhsIgenv _lhsIlenv (ACons _t1Itm _t2Itm)]
                   {-# LINE 515 "CCO/Core/AG.hs" #-}
                   )
              _tm =
                  ({-# LINE 20 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   ACons _t1Itm _t2Itm
                   {-# LINE 520 "CCO/Core/AG.hs" #-}
                   )
              _lhsOtm =
                  ({-# LINE 20 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tm
                   {-# LINE 525 "CCO/Core/AG.hs" #-}
                   )
              _t1Oargs =
                  ({-# LINE 27 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIargs
                   {-# LINE 530 "CCO/Core/AG.hs" #-}
                   )
              _t1Ogenv =
                  ({-# LINE 24 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 535 "CCO/Core/AG.hs" #-}
                   )
              _t1Olenv =
                  ({-# LINE 25 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlenv
                   {-# LINE 540 "CCO/Core/AG.hs" #-}
                   )
              _t1Olvl =
                  ({-# LINE 26 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl
                   {-# LINE 545 "CCO/Core/AG.hs" #-}
                   )
              _t2Oargs =
                  ({-# LINE 27 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIargs
                   {-# LINE 550 "CCO/Core/AG.hs" #-}
                   )
              _t2Ogenv =
                  ({-# LINE 24 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 555 "CCO/Core/AG.hs" #-}
                   )
              _t2Olenv =
                  ({-# LINE 25 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlenv
                   {-# LINE 560 "CCO/Core/AG.hs" #-}
                   )
              _t2Olvl =
                  ({-# LINE 26 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl
                   {-# LINE 565 "CCO/Core/AG.hs" #-}
                   )
              ( _t1Ibinds,_t1Iexp,_t1Imain,_t1Itm) =
                  t1_ _t1Oargs _t1Ogenv _t1Olenv _t1Olvl
              ( _t2Ibinds,_t2Iexp,_t2Imain,_t2Itm) =
                  t2_ _t2Oargs _t2Ogenv _t2Olenv _t2Olvl
          in  ( _lhsObinds,_lhsOexp,_lhsOmain,_lhsOtm)))
sem_ATm_APrim :: Var ->
                 T_ATm ->
                 T_ATm ->
                 T_ATm
sem_ATm_APrim f_ t1_ t2_ =
    (\ _lhsIargs
       _lhsIgenv
       _lhsIlenv
       _lhsIlvl ->
         (let _lhsOmain :: Ref
              _lhsOexp :: Exp
              _lhsObinds :: BindL
              _t1Ogenv :: (Map String Ref)
              _t2Ogenv :: (Map String Ref)
              _t1Olenv :: (Map String Ref)
              _t2Olenv :: (Map String Ref)
              _lhsOtm :: ATm
              _t1Oargs :: Int
              _t1Olvl :: Int
              _t2Oargs :: Int
              _t2Olvl :: Int
              _t1Ibinds :: BindL
              _t1Iexp :: Exp
              _t1Imain :: Ref
              _t1Itm :: ATm
              _t2Ibinds :: BindL
              _t2Iexp :: Exp
              _t2Imain :: Ref
              _t2Itm :: ATm
              _lhsOmain =
                  ({-# LINE 190 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   next _lhsIgenv
                   {-# LINE 604 "CCO/Core/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 191 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   Prim f_ [toPrim _t1Iexp, toPrim _t2Iexp]
                   {-# LINE 609 "CCO/Core/AG.hs" #-}
                   )
              _lhsObinds =
                  ({-# LINE 192 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   []
                   {-# LINE 614 "CCO/Core/AG.hs" #-}
                   )
              _t1Ogenv =
                  ({-# LINE 193 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 619 "CCO/Core/AG.hs" #-}
                   )
              _t2Ogenv =
                  ({-# LINE 194 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 624 "CCO/Core/AG.hs" #-}
                   )
              _t1Olenv =
                  ({-# LINE 195 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlenv
                   {-# LINE 629 "CCO/Core/AG.hs" #-}
                   )
              _t2Olenv =
                  ({-# LINE 196 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlenv
                   {-# LINE 634 "CCO/Core/AG.hs" #-}
                   )
              _tm =
                  ({-# LINE 20 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   APrim f_ _t1Itm _t2Itm
                   {-# LINE 639 "CCO/Core/AG.hs" #-}
                   )
              _lhsOtm =
                  ({-# LINE 20 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tm
                   {-# LINE 644 "CCO/Core/AG.hs" #-}
                   )
              _t1Oargs =
                  ({-# LINE 27 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIargs
                   {-# LINE 649 "CCO/Core/AG.hs" #-}
                   )
              _t1Olvl =
                  ({-# LINE 26 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl
                   {-# LINE 654 "CCO/Core/AG.hs" #-}
                   )
              _t2Oargs =
                  ({-# LINE 27 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIargs
                   {-# LINE 659 "CCO/Core/AG.hs" #-}
                   )
              _t2Olvl =
                  ({-# LINE 26 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl
                   {-# LINE 664 "CCO/Core/AG.hs" #-}
                   )
              ( _t1Ibinds,_t1Iexp,_t1Imain,_t1Itm) =
                  t1_ _t1Oargs _t1Ogenv _t1Olenv _t1Olvl
              ( _t2Ibinds,_t2Iexp,_t2Imain,_t2Itm) =
                  t2_ _t2Oargs _t2Ogenv _t2Olenv _t2Olvl
          in  ( _lhsObinds,_lhsOexp,_lhsOmain,_lhsOtm)))
sem_ATm_ALam :: Var ->
                T_ATm ->
                T_ATm
sem_ATm_ALam x_ t1_ =
    (\ _lhsIargs
       _lhsIgenv
       _lhsIlenv
       _lhsIlvl ->
         (let _lhsOmain :: Ref
              _lhsOexp :: Exp
              _lhsObinds :: BindL
              _t1Ogenv :: (Map String Ref)
              _t1Olenv :: (Map String Ref)
              _t1Olvl :: Int
              _t1Oargs :: Int
              _lhsOtm :: ATm
              _t1Ibinds :: BindL
              _t1Iexp :: Exp
              _t1Imain :: Ref
              _t1Itm :: ATm
              _lhsOmain =
                  ({-# LINE 101 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   next _lhsIgenv
                   {-# LINE 694 "CCO/Core/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 102 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   joinLam [Loc (0) $ 0] _t1Iexp
                   {-# LINE 699 "CCO/Core/AG.hs" #-}
                   )
              _lhsObinds =
                  ({-# LINE 103 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   []
                   {-# LINE 704 "CCO/Core/AG.hs" #-}
                   )
              _t1Ogenv =
                  ({-# LINE 104 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 709 "CCO/Core/AG.hs" #-}
                   )
              _t1Olenv =
                  ({-# LINE 105 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   updateEnv _lhsIlenv _lhsIargs x_
                   {-# LINE 714 "CCO/Core/AG.hs" #-}
                   )
              _t1Olvl =
                  ({-# LINE 106 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl + 1
                   {-# LINE 719 "CCO/Core/AG.hs" #-}
                   )
              _t1Oargs =
                  ({-# LINE 107 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   1 + _lhsIargs
                   {-# LINE 724 "CCO/Core/AG.hs" #-}
                   )
              _tm =
                  ({-# LINE 20 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   ALam x_ _t1Itm
                   {-# LINE 729 "CCO/Core/AG.hs" #-}
                   )
              _lhsOtm =
                  ({-# LINE 20 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tm
                   {-# LINE 734 "CCO/Core/AG.hs" #-}
                   )
              ( _t1Ibinds,_t1Iexp,_t1Imain,_t1Itm) =
                  t1_ _t1Oargs _t1Ogenv _t1Olenv _t1Olvl
          in  ( _lhsObinds,_lhsOexp,_lhsOmain,_lhsOtm)))
sem_ATm_AApp :: T_ATm ->
                T_ATm ->
                T_ATm
sem_ATm_AApp t1_ t2_ =
    (\ _lhsIargs
       _lhsIgenv
       _lhsIlenv
       _lhsIlvl ->
         (let _lhsOmain :: Ref
              _lhsOexp :: Exp
              _lhsObinds :: BindL
              _t1Ogenv :: (Map String Ref)
              _t2Ogenv :: (Map String Ref)
              _t1Olenv :: (Map String Ref)
              _t2Olenv :: (Map String Ref)
              _t1Olvl :: Int
              _t2Olvl :: Int
              _lhsOtm :: ATm
              _t1Oargs :: Int
              _t2Oargs :: Int
              _t1Ibinds :: BindL
              _t1Iexp :: Exp
              _t1Imain :: Ref
              _t1Itm :: ATm
              _t2Ibinds :: BindL
              _t2Iexp :: Exp
              _t2Imain :: Ref
              _t2Itm :: ATm
              _lhsOmain =
                  ({-# LINE 122 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   next _lhsIgenv
                   {-# LINE 770 "CCO/Core/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 123 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   appExp _lhsIlenv _t1Iexp _t2Iexp
                   {-# LINE 775 "CCO/Core/AG.hs" #-}
                   )
              _lhsObinds =
                  ({-# LINE 124 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   [Bind (Glob (getOffSet _lhsIgenv)) (Lam [] $ appExp _lhsIlenv _t1Iexp _t2Iexp)]
                   {-# LINE 780 "CCO/Core/AG.hs" #-}
                   )
              _t1Ogenv =
                  ({-# LINE 125 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 785 "CCO/Core/AG.hs" #-}
                   )
              _t2Ogenv =
                  ({-# LINE 126 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 790 "CCO/Core/AG.hs" #-}
                   )
              _t1Olenv =
                  ({-# LINE 127 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlenv
                   {-# LINE 795 "CCO/Core/AG.hs" #-}
                   )
              _t2Olenv =
                  ({-# LINE 128 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlenv
                   {-# LINE 800 "CCO/Core/AG.hs" #-}
                   )
              _t1Olvl =
                  ({-# LINE 129 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl
                   {-# LINE 805 "CCO/Core/AG.hs" #-}
                   )
              _t2Olvl =
                  ({-# LINE 130 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl
                   {-# LINE 810 "CCO/Core/AG.hs" #-}
                   )
              _tm =
                  ({-# LINE 20 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   AApp _t1Itm _t2Itm
                   {-# LINE 815 "CCO/Core/AG.hs" #-}
                   )
              _lhsOtm =
                  ({-# LINE 20 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tm
                   {-# LINE 820 "CCO/Core/AG.hs" #-}
                   )
              _t1Oargs =
                  ({-# LINE 27 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIargs
                   {-# LINE 825 "CCO/Core/AG.hs" #-}
                   )
              _t2Oargs =
                  ({-# LINE 27 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIargs
                   {-# LINE 830 "CCO/Core/AG.hs" #-}
                   )
              ( _t1Ibinds,_t1Iexp,_t1Imain,_t1Itm) =
                  t1_ _t1Oargs _t1Ogenv _t1Olenv _t1Olvl
              ( _t2Ibinds,_t2Iexp,_t2Imain,_t2Itm) =
                  t2_ _t2Oargs _t2Ogenv _t2Olenv _t2Olvl
          in  ( _lhsObinds,_lhsOexp,_lhsOmain,_lhsOtm)))
sem_ATm_ALet :: Var ->
                T_ATm ->
                T_ATm ->
                T_ATm
sem_ATm_ALet x_ t1_ t2_ =
    (\ _lhsIargs
       _lhsIgenv
       _lhsIlenv
       _lhsIlvl ->
         (let _lhsOmain :: Ref
              _lhsOexp :: Exp
              _lhsObinds :: BindL
              _t1Ogenv :: (Map String Ref)
              _t2Ogenv :: (Map String Ref)
              _t1Olenv :: (Map String Ref)
              _t2Olenv :: (Map String Ref)
              _t1Olvl :: Int
              _t2Olvl :: Int
              _t1Oargs :: Int
              _t2Oargs :: Int
              _lhsOtm :: ATm
              _t1Ibinds :: BindL
              _t1Iexp :: Exp
              _t1Imain :: Ref
              _t1Itm :: ATm
              _t2Ibinds :: BindL
              _t2Iexp :: Exp
              _t2Imain :: Ref
              _t2Itm :: ATm
              _lhsOmain =
                  ({-# LINE 147 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _t2Imain
                   {-# LINE 869 "CCO/Core/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 148 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   letExp _lhsIgenv _t1Iexp _t2Iexp
                   {-# LINE 874 "CCO/Core/AG.hs" #-}
                   )
              _lhsObinds =
                  ({-# LINE 149 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   letBinds _lhsIgenv _t1Itm _t1Iexp _t2Ibinds
                   {-# LINE 879 "CCO/Core/AG.hs" #-}
                   )
              _t1Ogenv =
                  ({-# LINE 150 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 884 "CCO/Core/AG.hs" #-}
                   )
              _genv =
                  ({-# LINE 151 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   globalLet _lhsIgenv _lhsIlvl x_
                   {-# LINE 889 "CCO/Core/AG.hs" #-}
                   )
              _t2Ogenv =
                  ({-# LINE 152 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _genv
                   {-# LINE 894 "CCO/Core/AG.hs" #-}
                   )
              _t1Olenv =
                  ({-# LINE 153 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   incrEnv $ insertRecursion _genv     _lhsIlenv x_ _lhsIlvl
                   {-# LINE 899 "CCO/Core/AG.hs" #-}
                   )
              _t2Olenv =
                  ({-# LINE 154 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   localLet _lhsIlenv _lhsIlvl x_
                   {-# LINE 904 "CCO/Core/AG.hs" #-}
                   )
              _t1Olvl =
                  ({-# LINE 155 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   localLetLvl _lhsIlvl
                   {-# LINE 909 "CCO/Core/AG.hs" #-}
                   )
              _t2Olvl =
                  ({-# LINE 156 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl
                   {-# LINE 914 "CCO/Core/AG.hs" #-}
                   )
              _t1Oargs =
                  ({-# LINE 157 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   0
                   {-# LINE 919 "CCO/Core/AG.hs" #-}
                   )
              _t2Oargs =
                  ({-# LINE 158 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   0
                   {-# LINE 924 "CCO/Core/AG.hs" #-}
                   )
              _tm =
                  ({-# LINE 20 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   ALet x_ _t1Itm _t2Itm
                   {-# LINE 929 "CCO/Core/AG.hs" #-}
                   )
              _lhsOtm =
                  ({-# LINE 20 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tm
                   {-# LINE 934 "CCO/Core/AG.hs" #-}
                   )
              ( _t1Ibinds,_t1Iexp,_t1Imain,_t1Itm) =
                  t1_ _t1Oargs _t1Ogenv _t1Olenv _t1Olvl
              ( _t2Ibinds,_t2Iexp,_t2Imain,_t2Itm) =
                  t2_ _t2Oargs _t2Ogenv _t2Olenv _t2Olvl
          in  ( _lhsObinds,_lhsOexp,_lhsOmain,_lhsOtm)))
sem_ATm_AIf :: T_ATm ->
               T_ATm ->
               T_ATm ->
               T_ATm
sem_ATm_AIf exp_ t1_ t2_ =
    (\ _lhsIargs
       _lhsIgenv
       _lhsIlenv
       _lhsIlvl ->
         (let _lhsOmain :: Ref
              _lhsOexp :: Exp
              _lhsObinds :: BindL
              _expOgenv :: (Map String Ref)
              _t1Ogenv :: (Map String Ref)
              _t2Ogenv :: (Map String Ref)
              _expOlenv :: (Map String Ref)
              _t1Olenv :: (Map String Ref)
              _t2Olenv :: (Map String Ref)
              _expOlvl :: Int
              _t1Olvl :: Int
              _t2Olvl :: Int
              _expOargs :: Int
              _t1Oargs :: Int
              _t2Oargs :: Int
              _lhsOtm :: ATm
              _expIbinds :: BindL
              _expIexp :: Exp
              _expImain :: Ref
              _expItm :: ATm
              _t1Ibinds :: BindL
              _t1Iexp :: Exp
              _t1Imain :: Ref
              _t1Itm :: ATm
              _t2Ibinds :: BindL
              _t2Iexp :: Exp
              _t2Imain :: Ref
              _t2Itm :: ATm
              _lhsOmain =
                  ({-# LINE 205 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   Glob (getOffSet _lhsIgenv)
                   {-# LINE 981 "CCO/Core/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 206 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   createCase _expIexp _exp
                   {-# LINE 986 "CCO/Core/AG.hs" #-}
                   )
              _exp =
                  ({-# LINE 207 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   Case (Var $ Tag $ Loc 0 $ getOffSet _lhsIlenv) [evalVar _t2Iexp, evalVar _t1Iexp]
                   {-# LINE 991 "CCO/Core/AG.hs" #-}
                   )
              _lhsObinds =
                  ({-# LINE 208 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   [Bind (Glob (getOffSet _lhsIgenv)) $ Lam [] $ createCase _expIexp _exp    ]
                   {-# LINE 996 "CCO/Core/AG.hs" #-}
                   )
              _expOgenv =
                  ({-# LINE 209 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 1001 "CCO/Core/AG.hs" #-}
                   )
              _t1Ogenv =
                  ({-# LINE 210 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 1006 "CCO/Core/AG.hs" #-}
                   )
              _t2Ogenv =
                  ({-# LINE 211 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 1011 "CCO/Core/AG.hs" #-}
                   )
              _expOlenv =
                  ({-# LINE 212 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlenv
                   {-# LINE 1016 "CCO/Core/AG.hs" #-}
                   )
              _t1Olenv =
                  ({-# LINE 213 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   incrOffSet' _lhsIlenv 1
                   {-# LINE 1021 "CCO/Core/AG.hs" #-}
                   )
              _t2Olenv =
                  ({-# LINE 214 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   incrOffSet' _lhsIlenv 1
                   {-# LINE 1026 "CCO/Core/AG.hs" #-}
                   )
              _expOlvl =
                  ({-# LINE 215 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl + 1
                   {-# LINE 1031 "CCO/Core/AG.hs" #-}
                   )
              _t1Olvl =
                  ({-# LINE 216 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl + 1
                   {-# LINE 1036 "CCO/Core/AG.hs" #-}
                   )
              _t2Olvl =
                  ({-# LINE 217 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl + 1
                   {-# LINE 1041 "CCO/Core/AG.hs" #-}
                   )
              _expOargs =
                  ({-# LINE 218 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   0
                   {-# LINE 1046 "CCO/Core/AG.hs" #-}
                   )
              _t1Oargs =
                  ({-# LINE 219 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   0
                   {-# LINE 1051 "CCO/Core/AG.hs" #-}
                   )
              _t2Oargs =
                  ({-# LINE 220 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   0
                   {-# LINE 1056 "CCO/Core/AG.hs" #-}
                   )
              _tm =
                  ({-# LINE 20 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   AIf _expItm _t1Itm _t2Itm
                   {-# LINE 1061 "CCO/Core/AG.hs" #-}
                   )
              _lhsOtm =
                  ({-# LINE 20 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tm
                   {-# LINE 1066 "CCO/Core/AG.hs" #-}
                   )
              ( _expIbinds,_expIexp,_expImain,_expItm) =
                  exp_ _expOargs _expOgenv _expOlenv _expOlvl
              ( _t1Ibinds,_t1Iexp,_t1Imain,_t1Itm) =
                  t1_ _t1Oargs _t1Ogenv _t1Olenv _t1Olvl
              ( _t2Ibinds,_t2Iexp,_t2Imain,_t2Itm) =
                  t2_ _t2Oargs _t2Ogenv _t2Olenv _t2Olvl
          in  ( _lhsObinds,_lhsOexp,_lhsOmain,_lhsOtm)))
-- Bind --------------------------------------------------------
data Bind = Bind (Ref) (Exp)
          deriving ( Show)
-- cata
sem_Bind :: Bind ->
            T_Bind
sem_Bind (Bind _x _xexp) =
    (sem_Bind_Bind (sem_Ref _x) (sem_Exp _xexp))
-- semantic domain
type T_Bind = Int ->
              ( ([CR.Bind]),Int)
data Inh_Bind = Inh_Bind {stkoff_Inh_Bind :: Int}
data Syn_Bind = Syn_Bind {crbindl_Syn_Bind :: ([CR.Bind]),stkoff_Syn_Bind :: Int}
wrap_Bind :: T_Bind ->
             Inh_Bind ->
             Syn_Bind
wrap_Bind sem (Inh_Bind _lhsIstkoff) =
    (let ( _lhsOcrbindl,_lhsOstkoff) = sem _lhsIstkoff
     in  (Syn_Bind _lhsOcrbindl _lhsOstkoff))
sem_Bind_Bind :: T_Ref ->
                 T_Exp ->
                 T_Bind
sem_Bind_Bind x_ xexp_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrbindl :: ([CR.Bind])
              _xexpOstkoff :: Int
              _lhsOstkoff :: Int
              _xIcrref :: (CR.RRef)
              _xIcrrefl :: ([CR.RRef])
              _xexpIcrexp :: (CR.Exp)
              _xexpIcrexpl :: ([CR.Exp])
              _lhsOcrbindl =
                  ({-# LINE 61 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_xexpIcrexp]
                   {-# LINE 1109 "CCO/Core/AG.hs" #-}
                   )
              _xexpOstkoff =
                  ({-# LINE 91 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   0
                   {-# LINE 1114 "CCO/Core/AG.hs" #-}
                   )
              _lhsOstkoff =
                  ({-# LINE 92 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff + 1
                   {-# LINE 1119 "CCO/Core/AG.hs" #-}
                   )
              ( _xIcrref,_xIcrrefl) =
                  x_
              ( _xexpIcrexp,_xexpIcrexpl) =
                  xexp_ _xexpOstkoff
          in  ( _lhsOcrbindl,_lhsOstkoff)))
-- BindL -------------------------------------------------------
type BindL = [Bind]
-- cata
sem_BindL :: BindL ->
             T_BindL
sem_BindL list =
    (Prelude.foldr sem_BindL_Cons sem_BindL_Nil (Prelude.map sem_Bind list))
-- semantic domain
type T_BindL = Int ->
               ( ([CR.Bind]),Int)
data Inh_BindL = Inh_BindL {stkoff_Inh_BindL :: Int}
data Syn_BindL = Syn_BindL {crbindl_Syn_BindL :: ([CR.Bind]),stkoff_Syn_BindL :: Int}
wrap_BindL :: T_BindL ->
              Inh_BindL ->
              Syn_BindL
wrap_BindL sem (Inh_BindL _lhsIstkoff) =
    (let ( _lhsOcrbindl,_lhsOstkoff) = sem _lhsIstkoff
     in  (Syn_BindL _lhsOcrbindl _lhsOstkoff))
sem_BindL_Cons :: T_Bind ->
                  T_BindL ->
                  T_BindL
sem_BindL_Cons hd_ tl_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrbindl :: ([CR.Bind])
              _lhsOstkoff :: Int
              _hdOstkoff :: Int
              _tlOstkoff :: Int
              _hdIcrbindl :: ([CR.Bind])
              _hdIstkoff :: Int
              _tlIcrbindl :: ([CR.Bind])
              _tlIstkoff :: Int
              _lhsOcrbindl =
                  ({-# LINE 58 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _hdIcrbindl ++ _tlIcrbindl
                   {-# LINE 1160 "CCO/Core/AG.hs" #-}
                   )
              _lhsOstkoff =
                  ({-# LINE 84 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _tlIstkoff
                   {-# LINE 1165 "CCO/Core/AG.hs" #-}
                   )
              _hdOstkoff =
                  ({-# LINE 84 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 1170 "CCO/Core/AG.hs" #-}
                   )
              _tlOstkoff =
                  ({-# LINE 84 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _hdIstkoff
                   {-# LINE 1175 "CCO/Core/AG.hs" #-}
                   )
              ( _hdIcrbindl,_hdIstkoff) =
                  hd_ _hdOstkoff
              ( _tlIcrbindl,_tlIstkoff) =
                  tl_ _tlOstkoff
          in  ( _lhsOcrbindl,_lhsOstkoff)))
sem_BindL_Nil :: T_BindL
sem_BindL_Nil =
    (\ _lhsIstkoff ->
         (let _lhsOcrbindl :: ([CR.Bind])
              _lhsOstkoff :: Int
              _lhsOcrbindl =
                  ({-# LINE 58 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   []
                   {-# LINE 1190 "CCO/Core/AG.hs" #-}
                   )
              _lhsOstkoff =
                  ({-# LINE 84 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 1195 "CCO/Core/AG.hs" #-}
                   )
          in  ( _lhsOcrbindl,_lhsOstkoff)))
-- Core --------------------------------------------------------
data Core = Core (ATm)
-- cata
sem_Core :: Core ->
            T_Core
sem_Core (Core _tm) =
    (sem_Core_Core (sem_ATm _tm))
-- semantic domain
type T_Core = ( Mod)
data Inh_Core = Inh_Core {}
data Syn_Core = Syn_Core {core_Syn_Core :: Mod}
wrap_Core :: T_Core ->
             Inh_Core ->
             Syn_Core
wrap_Core sem (Inh_Core) =
    (let ( _lhsOcore) = sem
     in  (Syn_Core _lhsOcore))
sem_Core_Core :: T_ATm ->
                 T_Core
sem_Core_Core tm_ =
    (let _lhsOcore :: Mod
         _tmOgenv :: (Map String Ref)
         _tmOlenv :: (Map String Ref)
         _tmOlvl :: Int
         _tmOargs :: Int
         _tmIbinds :: BindL
         _tmIexp :: Exp
         _tmImain :: Ref
         _tmItm :: ATm
         _lhsOcore =
             ({-# LINE 13 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
              Mod (SExp (Var _tmImain)) (defaultBinds ++ _tmIbinds)
              {-# LINE 1230 "CCO/Core/AG.hs" #-}
              )
         _tmOgenv =
             ({-# LINE 14 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
              (8,[("True", Glob 7),("False", Glob 6),("isNil", Glob 5),("isCons", Glob 4),("tail", Glob 3),("head", Glob 2),("cons", Glob 1),("nil",Glob 0)])
              {-# LINE 1235 "CCO/Core/AG.hs" #-}
              )
         _tmOlenv =
             ({-# LINE 15 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
              (0,[])
              {-# LINE 1240 "CCO/Core/AG.hs" #-}
              )
         _tmOlvl =
             ({-# LINE 16 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
              0
              {-# LINE 1245 "CCO/Core/AG.hs" #-}
              )
         _tmOargs =
             ({-# LINE 17 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
              0
              {-# LINE 1250 "CCO/Core/AG.hs" #-}
              )
         ( _tmIbinds,_tmIexp,_tmImain,_tmItm) =
             tm_ _tmOargs _tmOgenv _tmOlenv _tmOlvl
     in  ( _lhsOcore))
-- Exp ---------------------------------------------------------
data Exp = SExp (SExp)
         | True_
         | False_
         | Nil
         | Cons (SExp) (SExp)
         | Lam (RefL) (Exp)
         | App (Exp) (SExpL)
         | Prim (String) (SExpL)
         | Node (Int) (SExpL)
         | Case (SExp) (ExpL)
         | Let (Bind) (Exp)
         | Dbg (String)
         | Eval (Exp)
         deriving ( Show)
-- cata
sem_Exp :: Exp ->
           T_Exp
sem_Exp (SExp _sexp) =
    (sem_Exp_SExp (sem_SExp _sexp))
sem_Exp (True_) =
    (sem_Exp_True_)
sem_Exp (False_) =
    (sem_Exp_False_)
sem_Exp (Nil) =
    (sem_Exp_Nil)
sem_Exp (Cons _t1 _t2) =
    (sem_Exp_Cons (sem_SExp _t1) (sem_SExp _t2))
sem_Exp (Lam _args _body) =
    (sem_Exp_Lam (sem_RefL _args) (sem_Exp _body))
sem_Exp (App _func _args) =
    (sem_Exp_App (sem_Exp _func) (sem_SExpL _args))
sem_Exp (Prim _func _args) =
    (sem_Exp_Prim _func (sem_SExpL _args))
sem_Exp (Node _tag _args) =
    (sem_Exp_Node _tag (sem_SExpL _args))
sem_Exp (Case _sexp _alts) =
    (sem_Exp_Case (sem_SExp _sexp) (sem_ExpL _alts))
sem_Exp (Let _bind _body) =
    (sem_Exp_Let (sem_Bind _bind) (sem_Exp _body))
sem_Exp (Dbg _info) =
    (sem_Exp_Dbg _info)
sem_Exp (Eval _body) =
    (sem_Exp_Eval (sem_Exp _body))
-- semantic domain
type T_Exp = Int ->
             ( (CR.Exp),([CR.Exp]))
data Inh_Exp = Inh_Exp {stkoff_Inh_Exp :: Int}
data Syn_Exp = Syn_Exp {crexp_Syn_Exp :: (CR.Exp),crexpl_Syn_Exp :: ([CR.Exp])}
wrap_Exp :: T_Exp ->
            Inh_Exp ->
            Syn_Exp
wrap_Exp sem (Inh_Exp _lhsIstkoff) =
    (let ( _lhsOcrexp,_lhsOcrexpl) = sem _lhsIstkoff
     in  (Syn_Exp _lhsOcrexp _lhsOcrexpl))
sem_Exp_SExp :: T_SExp ->
                T_Exp
sem_Exp_SExp sexp_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _lhsOcrexp :: (CR.Exp)
              _sexpIcrsexpl :: ([CR.SExp])
              _crexp =
                  ({-# LINE 36 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   CR.mkExp  (head       _sexpIcrsexpl)
                   {-# LINE 1320 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 55 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 1325 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 1330 "CCO/Core/AG.hs" #-}
                   )
              ( _sexpIcrsexpl) =
                  sexp_
          in  ( _lhsOcrexp,_lhsOcrexpl)))
sem_Exp_True_ :: T_Exp
sem_Exp_True_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _lhsOcrexp :: (CR.Exp)
              _crexp =
                  ({-# LINE 38 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   CR.mkTup  1 []
                   {-# LINE 1343 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 55 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 1348 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 1353 "CCO/Core/AG.hs" #-}
                   )
          in  ( _lhsOcrexp,_lhsOcrexpl)))
sem_Exp_False_ :: T_Exp
sem_Exp_False_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _lhsOcrexp :: (CR.Exp)
              _crexp =
                  ({-# LINE 37 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   CR.mkTup  0 []
                   {-# LINE 1364 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 55 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 1369 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 1374 "CCO/Core/AG.hs" #-}
                   )
          in  ( _lhsOcrexp,_lhsOcrexpl)))
sem_Exp_Nil :: T_Exp
sem_Exp_Nil =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _lhsOcrexp :: (CR.Exp)
              _crexp =
                  ({-# LINE 39 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   CR.mkTup  0 []
                   {-# LINE 1385 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 55 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 1390 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 1395 "CCO/Core/AG.hs" #-}
                   )
          in  ( _lhsOcrexp,_lhsOcrexpl)))
sem_Exp_Cons :: T_SExp ->
                T_SExp ->
                T_Exp
sem_Exp_Cons t1_ t2_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _lhsOcrexp :: (CR.Exp)
              _t1Icrsexpl :: ([CR.SExp])
              _t2Icrsexpl :: ([CR.SExp])
              _crexp =
                  ({-# LINE 40 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   CR.mkTup  1 [head     _t1Icrsexpl,    head _t2Icrsexpl]
                   {-# LINE 1410 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 55 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 1415 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 1420 "CCO/Core/AG.hs" #-}
                   )
              ( _t1Icrsexpl) =
                  t1_
              ( _t2Icrsexpl) =
                  t2_
          in  ( _lhsOcrexp,_lhsOcrexpl)))
sem_Exp_Lam :: T_RefL ->
               T_Exp ->
               T_Exp
sem_Exp_Lam args_ body_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _bodyOstkoff :: Int
              _lhsOcrexp :: (CR.Exp)
              _argsIcrrefl :: ([CR.RRef])
              _bodyIcrexp :: (CR.Exp)
              _bodyIcrexpl :: ([CR.Exp])
              _crexp =
                  ({-# LINE 41 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   CR.mkLam  (length     _argsIcrrefl)   100 _bodyIcrexp
                   {-# LINE 1441 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 55 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 1446 "CCO/Core/AG.hs" #-}
                   )
              _bodyOstkoff =
                  ({-# LINE 95 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   length _argsIcrrefl
                   {-# LINE 1451 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 1456 "CCO/Core/AG.hs" #-}
                   )
              ( _argsIcrrefl) =
                  args_
              ( _bodyIcrexp,_bodyIcrexpl) =
                  body_ _bodyOstkoff
          in  ( _lhsOcrexp,_lhsOcrexpl)))
sem_Exp_App :: T_Exp ->
               T_SExpL ->
               T_Exp
sem_Exp_App func_ args_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _lhsOcrexp :: (CR.Exp)
              _funcOstkoff :: Int
              _funcIcrexp :: (CR.Exp)
              _funcIcrexpl :: ([CR.Exp])
              _argsIcrsexpl :: ([CR.SExp])
              _crexp =
                  ({-# LINE 42 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   CR.mkApp  _funcIcrexp _argsIcrsexpl
                   {-# LINE 1477 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 55 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 1482 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 1487 "CCO/Core/AG.hs" #-}
                   )
              _funcOstkoff =
                  ({-# LINE 82 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 1492 "CCO/Core/AG.hs" #-}
                   )
              ( _funcIcrexp,_funcIcrexpl) =
                  func_ _funcOstkoff
              ( _argsIcrsexpl) =
                  args_
          in  ( _lhsOcrexp,_lhsOcrexpl)))
sem_Exp_Prim :: String ->
                T_SExpL ->
                T_Exp
sem_Exp_Prim func_ args_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _lhsOcrexp :: (CR.Exp)
              _argsIcrsexpl :: ([CR.SExp])
              _crexp =
                  ({-# LINE 43 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   CR.mkFFI  func_       _argsIcrsexpl
                   {-# LINE 1510 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 55 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 1515 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 1520 "CCO/Core/AG.hs" #-}
                   )
              ( _argsIcrsexpl) =
                  args_
          in  ( _lhsOcrexp,_lhsOcrexpl)))
sem_Exp_Node :: Int ->
                T_SExpL ->
                T_Exp
sem_Exp_Node tag_ args_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _lhsOcrexp :: (CR.Exp)
              _argsIcrsexpl :: ([CR.SExp])
              _crexp =
                  ({-# LINE 44 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   CR.mkTup  tag_        _argsIcrsexpl
                   {-# LINE 1536 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 55 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 1541 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 1546 "CCO/Core/AG.hs" #-}
                   )
              ( _argsIcrsexpl) =
                  args_
          in  ( _lhsOcrexp,_lhsOcrexpl)))
sem_Exp_Case :: T_SExp ->
                T_ExpL ->
                T_Exp
sem_Exp_Case sexp_ alts_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _lhsOcrexp :: (CR.Exp)
              _altsOstkoff :: Int
              _sexpIcrsexpl :: ([CR.SExp])
              _altsIcrexpl :: ([CR.Exp])
              _crexp =
                  ({-# LINE 45 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   CR.mkCase (head       _sexpIcrsexpl)  _altsIcrexpl
                   {-# LINE 1564 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 55 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 1569 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 1574 "CCO/Core/AG.hs" #-}
                   )
              _altsOstkoff =
                  ({-# LINE 82 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 1579 "CCO/Core/AG.hs" #-}
                   )
              ( _sexpIcrsexpl) =
                  sexp_
              ( _altsIcrexpl) =
                  alts_ _altsOstkoff
          in  ( _lhsOcrexp,_lhsOcrexpl)))
sem_Exp_Let :: T_Bind ->
               T_Exp ->
               T_Exp
sem_Exp_Let bind_ body_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _lhsOcrexp :: (CR.Exp)
              _bindOstkoff :: Int
              _bodyOstkoff :: Int
              _bindIcrbindl :: ([CR.Bind])
              _bindIstkoff :: Int
              _bodyIcrexp :: (CR.Exp)
              _bodyIcrexpl :: ([CR.Exp])
              _crexp =
                  ({-# LINE 46 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   CR.mkLet  _lhsIstkoff _bindIcrbindl   _bodyIcrexp
                   {-# LINE 1602 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 55 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 1607 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 1612 "CCO/Core/AG.hs" #-}
                   )
              _bindOstkoff =
                  ({-# LINE 84 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 1617 "CCO/Core/AG.hs" #-}
                   )
              _bodyOstkoff =
                  ({-# LINE 82 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _bindIstkoff
                   {-# LINE 1622 "CCO/Core/AG.hs" #-}
                   )
              ( _bindIcrbindl,_bindIstkoff) =
                  bind_ _bindOstkoff
              ( _bodyIcrexp,_bodyIcrexpl) =
                  body_ _bodyOstkoff
          in  ( _lhsOcrexp,_lhsOcrexpl)))
sem_Exp_Dbg :: String ->
               T_Exp
sem_Exp_Dbg info_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _lhsOcrexp :: (CR.Exp)
              _crexp =
                  ({-# LINE 47 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   CR.mkDbg  info_
                   {-# LINE 1638 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 55 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 1643 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 1648 "CCO/Core/AG.hs" #-}
                   )
          in  ( _lhsOcrexp,_lhsOcrexpl)))
sem_Exp_Eval :: T_Exp ->
                T_Exp
sem_Exp_Eval body_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _lhsOcrexp :: (CR.Exp)
              _bodyOstkoff :: Int
              _bodyIcrexp :: (CR.Exp)
              _bodyIcrexpl :: ([CR.Exp])
              _crexp =
                  ({-# LINE 48 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   CR.mkEval _bodyIcrexp
                   {-# LINE 1663 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 55 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 1668 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 1673 "CCO/Core/AG.hs" #-}
                   )
              _bodyOstkoff =
                  ({-# LINE 82 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 1678 "CCO/Core/AG.hs" #-}
                   )
              ( _bodyIcrexp,_bodyIcrexpl) =
                  body_ _bodyOstkoff
          in  ( _lhsOcrexp,_lhsOcrexpl)))
-- ExpL --------------------------------------------------------
type ExpL = [Exp]
-- cata
sem_ExpL :: ExpL ->
            T_ExpL
sem_ExpL list =
    (Prelude.foldr sem_ExpL_Cons sem_ExpL_Nil (Prelude.map sem_Exp list))
-- semantic domain
type T_ExpL = Int ->
              ( ([CR.Exp]))
data Inh_ExpL = Inh_ExpL {stkoff_Inh_ExpL :: Int}
data Syn_ExpL = Syn_ExpL {crexpl_Syn_ExpL :: ([CR.Exp])}
wrap_ExpL :: T_ExpL ->
             Inh_ExpL ->
             Syn_ExpL
wrap_ExpL sem (Inh_ExpL _lhsIstkoff) =
    (let ( _lhsOcrexpl) = sem _lhsIstkoff
     in  (Syn_ExpL _lhsOcrexpl))
sem_ExpL_Cons :: T_Exp ->
                 T_ExpL ->
                 T_ExpL
sem_ExpL_Cons hd_ tl_ =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _hdOstkoff :: Int
              _tlOstkoff :: Int
              _hdIcrexp :: (CR.Exp)
              _hdIcrexpl :: ([CR.Exp])
              _tlIcrexpl :: ([CR.Exp])
              _lhsOcrexpl =
                  ({-# LINE 33 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _hdIcrexpl ++ _tlIcrexpl
                   {-# LINE 1715 "CCO/Core/AG.hs" #-}
                   )
              _hdOstkoff =
                  ({-# LINE 82 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 1720 "CCO/Core/AG.hs" #-}
                   )
              _tlOstkoff =
                  ({-# LINE 82 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 1725 "CCO/Core/AG.hs" #-}
                   )
              ( _hdIcrexp,_hdIcrexpl) =
                  hd_ _hdOstkoff
              ( _tlIcrexpl) =
                  tl_ _tlOstkoff
          in  ( _lhsOcrexpl)))
sem_ExpL_Nil :: T_ExpL
sem_ExpL_Nil =
    (\ _lhsIstkoff ->
         (let _lhsOcrexpl :: ([CR.Exp])
              _lhsOcrexpl =
                  ({-# LINE 33 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   []
                   {-# LINE 1739 "CCO/Core/AG.hs" #-}
                   )
          in  ( _lhsOcrexpl)))
-- Mod ---------------------------------------------------------
data Mod = Mod (Exp) (BindL)
-- cata
sem_Mod :: Mod ->
           T_Mod
sem_Mod (Mod _main _binds) =
    (sem_Mod_Mod (sem_Exp _main) (sem_BindL _binds))
-- semantic domain
type T_Mod = ( (CR.Mod))
data Inh_Mod = Inh_Mod {}
data Syn_Mod = Syn_Mod {crmod_Syn_Mod :: (CR.Mod)}
wrap_Mod :: T_Mod ->
            Inh_Mod ->
            Syn_Mod
wrap_Mod sem (Inh_Mod) =
    (let ( _lhsOcrmod) = sem
     in  (Syn_Mod _lhsOcrmod))
sem_Mod_Mod :: T_Exp ->
               T_BindL ->
               T_Mod
sem_Mod_Mod main_ binds_ =
    (let _lhsOcrmod :: (CR.Mod)
         _bindsOstkoff :: Int
         _mainOstkoff :: Int
         _mainIcrexp :: (CR.Exp)
         _mainIcrexpl :: ([CR.Exp])
         _bindsIcrbindl :: ([CR.Bind])
         _bindsIstkoff :: Int
         _lhsOcrmod =
             ({-# LINE 16 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              CR.mkModWithMetas (mkHNm "Main") Nothing (length _bindsIcrbindl + 100)
              [CR.mkMetaDataType (mkHNm "Bool") [CR.mkMetaDataCon (mkHNm "False") 0, CR.mkMetaDataCon (mkHNm "True") 1]]
              (CRI.crarrayFromList _bindsIcrbindl) (CR.mkEval _mainIcrexp)
              {-# LINE 1775 "CCO/Core/AG.hs" #-}
              )
         _bindsOstkoff =
             ({-# LINE 87 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              0
              {-# LINE 1780 "CCO/Core/AG.hs" #-}
              )
         _mainOstkoff =
             ({-# LINE 88 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              _bindsIstkoff
              {-# LINE 1785 "CCO/Core/AG.hs" #-}
              )
         ( _mainIcrexp,_mainIcrexpl) =
             main_ _mainOstkoff
         ( _bindsIcrbindl,_bindsIstkoff) =
             binds_ _bindsOstkoff
     in  ( _lhsOcrmod))
-- Ref ---------------------------------------------------------
data Ref = Glob (Int)
         | Loc (Int) (Int)
         | Tag (Ref)
         | Field (Int) (Ref)
         deriving ( Show)
-- cata
sem_Ref :: Ref ->
           T_Ref
sem_Ref (Glob _offset) =
    (sem_Ref_Glob _offset)
sem_Ref (Loc _levdiff _offset) =
    (sem_Ref_Loc _levdiff _offset)
sem_Ref (Tag _ref) =
    (sem_Ref_Tag (sem_Ref _ref))
sem_Ref (Field _fld _ref) =
    (sem_Ref_Field _fld (sem_Ref _ref))
-- semantic domain
type T_Ref = ( (CR.RRef),([CR.RRef]))
data Inh_Ref = Inh_Ref {}
data Syn_Ref = Syn_Ref {crref_Syn_Ref :: (CR.RRef),crrefl_Syn_Ref :: ([CR.RRef])}
wrap_Ref :: T_Ref ->
            Inh_Ref ->
            Syn_Ref
wrap_Ref sem (Inh_Ref) =
    (let ( _lhsOcrref,_lhsOcrrefl) = sem
     in  (Syn_Ref _lhsOcrref _lhsOcrrefl))
sem_Ref_Glob :: Int ->
                T_Ref
sem_Ref_Glob offset_ =
    (let _lhsOcrrefl :: ([CR.RRef])
         _lhsOcrref :: (CR.RRef)
         _lhsOcrrefl =
             ({-# LINE 70 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              [CR.mkModRef offset_]
              {-# LINE 1827 "CCO/Core/AG.hs" #-}
              )
         _lhsOcrref =
             ({-# LINE 71 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              CR.mkModRef offset_
              {-# LINE 1832 "CCO/Core/AG.hs" #-}
              )
     in  ( _lhsOcrref,_lhsOcrrefl))
sem_Ref_Loc :: Int ->
               Int ->
               T_Ref
sem_Ref_Loc levdiff_ offset_ =
    (let _lhsOcrrefl :: ([CR.RRef])
         _lhsOcrref :: (CR.RRef)
         _lhsOcrrefl =
             ({-# LINE 72 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              [CR.mkLocDifRef levdiff_ offset_]
              {-# LINE 1844 "CCO/Core/AG.hs" #-}
              )
         _lhsOcrref =
             ({-# LINE 73 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              CR.mkLocDifRef levdiff_ offset_
              {-# LINE 1849 "CCO/Core/AG.hs" #-}
              )
     in  ( _lhsOcrref,_lhsOcrrefl))
sem_Ref_Tag :: T_Ref ->
               T_Ref
sem_Ref_Tag ref_ =
    (let _lhsOcrrefl :: ([CR.RRef])
         _lhsOcrref :: (CR.RRef)
         _refIcrref :: (CR.RRef)
         _refIcrrefl :: ([CR.RRef])
         _lhsOcrrefl =
             ({-# LINE 74 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              [CRI.RRef_Tag _refIcrref]
              {-# LINE 1862 "CCO/Core/AG.hs" #-}
              )
         _lhsOcrref =
             ({-# LINE 67 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              _refIcrref
              {-# LINE 1867 "CCO/Core/AG.hs" #-}
              )
         ( _refIcrref,_refIcrrefl) =
             ref_
     in  ( _lhsOcrref,_lhsOcrrefl))
sem_Ref_Field :: Int ->
                 T_Ref ->
                 T_Ref
sem_Ref_Field fld_ ref_ =
    (let _lhsOcrrefl :: ([CR.RRef])
         _lhsOcrref :: (CR.RRef)
         _refIcrref :: (CR.RRef)
         _refIcrrefl :: ([CR.RRef])
         _lhsOcrrefl =
             ({-# LINE 75 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              [CRI.RRef_Fld _refIcrref fld_]
              {-# LINE 1883 "CCO/Core/AG.hs" #-}
              )
         _lhsOcrref =
             ({-# LINE 67 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              _refIcrref
              {-# LINE 1888 "CCO/Core/AG.hs" #-}
              )
         ( _refIcrref,_refIcrrefl) =
             ref_
     in  ( _lhsOcrref,_lhsOcrrefl))
-- RefL --------------------------------------------------------
type RefL = [Ref]
-- cata
sem_RefL :: RefL ->
            T_RefL
sem_RefL list =
    (Prelude.foldr sem_RefL_Cons sem_RefL_Nil (Prelude.map sem_Ref list))
-- semantic domain
type T_RefL = ( ([CR.RRef]))
data Inh_RefL = Inh_RefL {}
data Syn_RefL = Syn_RefL {crrefl_Syn_RefL :: ([CR.RRef])}
wrap_RefL :: T_RefL ->
             Inh_RefL ->
             Syn_RefL
wrap_RefL sem (Inh_RefL) =
    (let ( _lhsOcrrefl) = sem
     in  (Syn_RefL _lhsOcrrefl))
sem_RefL_Cons :: T_Ref ->
                 T_RefL ->
                 T_RefL
sem_RefL_Cons hd_ tl_ =
    (let _lhsOcrrefl :: ([CR.RRef])
         _hdIcrref :: (CR.RRef)
         _hdIcrrefl :: ([CR.RRef])
         _tlIcrrefl :: ([CR.RRef])
         _lhsOcrrefl =
             ({-# LINE 64 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              _hdIcrrefl ++ _tlIcrrefl
              {-# LINE 1921 "CCO/Core/AG.hs" #-}
              )
         ( _hdIcrref,_hdIcrrefl) =
             hd_
         ( _tlIcrrefl) =
             tl_
     in  ( _lhsOcrrefl))
sem_RefL_Nil :: T_RefL
sem_RefL_Nil =
    (let _lhsOcrrefl :: ([CR.RRef])
         _lhsOcrrefl =
             ({-# LINE 64 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              []
              {-# LINE 1934 "CCO/Core/AG.hs" #-}
              )
     in  ( _lhsOcrrefl))
-- SExp --------------------------------------------------------
data SExp = Int (Int)
          | Var (Ref)
          deriving ( Show)
-- cata
sem_SExp :: SExp ->
            T_SExp
sem_SExp (Int _i) =
    (sem_SExp_Int _i)
sem_SExp (Var _x) =
    (sem_SExp_Var (sem_Ref _x))
-- semantic domain
type T_SExp = ( ([CR.SExp]))
data Inh_SExp = Inh_SExp {}
data Syn_SExp = Syn_SExp {crsexpl_Syn_SExp :: ([CR.SExp])}
wrap_SExp :: T_SExp ->
             Inh_SExp ->
             Syn_SExp
wrap_SExp sem (Inh_SExp) =
    (let ( _lhsOcrsexpl) = sem
     in  (Syn_SExp _lhsOcrsexpl))
sem_SExp_Int :: Int ->
                T_SExp
sem_SExp_Int i_ =
    (let _lhsOcrsexpl :: ([CR.SExp])
         _lhsOcrsexpl =
             ({-# LINE 24 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              [CR.mkInteger' $ toInteger i_]
              {-# LINE 1965 "CCO/Core/AG.hs" #-}
              )
     in  ( _lhsOcrsexpl))
sem_SExp_Var :: T_Ref ->
                T_SExp
sem_SExp_Var x_ =
    (let _lhsOcrsexpl :: ([CR.SExp])
         _xIcrref :: (CR.RRef)
         _xIcrrefl :: ([CR.RRef])
         _lhsOcrsexpl =
             ({-# LINE 25 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              [CR.mkVar' $ head _xIcrrefl]
              {-# LINE 1977 "CCO/Core/AG.hs" #-}
              )
         ( _xIcrref,_xIcrrefl) =
             x_
     in  ( _lhsOcrsexpl))
-- SExpL -------------------------------------------------------
type SExpL = [SExp]
-- cata
sem_SExpL :: SExpL ->
             T_SExpL
sem_SExpL list =
    (Prelude.foldr sem_SExpL_Cons sem_SExpL_Nil (Prelude.map sem_SExp list))
-- semantic domain
type T_SExpL = ( ([CR.SExp]))
data Inh_SExpL = Inh_SExpL {}
data Syn_SExpL = Syn_SExpL {crsexpl_Syn_SExpL :: ([CR.SExp])}
wrap_SExpL :: T_SExpL ->
              Inh_SExpL ->
              Syn_SExpL
wrap_SExpL sem (Inh_SExpL) =
    (let ( _lhsOcrsexpl) = sem
     in  (Syn_SExpL _lhsOcrsexpl))
sem_SExpL_Cons :: T_SExp ->
                  T_SExpL ->
                  T_SExpL
sem_SExpL_Cons hd_ tl_ =
    (let _lhsOcrsexpl :: ([CR.SExp])
         _hdIcrsexpl :: ([CR.SExp])
         _tlIcrsexpl :: ([CR.SExp])
         _lhsOcrsexpl =
             ({-# LINE 21 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              _hdIcrsexpl ++ _tlIcrsexpl
              {-# LINE 2009 "CCO/Core/AG.hs" #-}
              )
         ( _hdIcrsexpl) =
             hd_
         ( _tlIcrsexpl) =
             tl_
     in  ( _lhsOcrsexpl))
sem_SExpL_Nil :: T_SExpL
sem_SExpL_Nil =
    (let _lhsOcrsexpl :: ([CR.SExp])
         _lhsOcrsexpl =
             ({-# LINE 21 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              []
              {-# LINE 2022 "CCO/Core/AG.hs" #-}
              )
     in  ( _lhsOcrsexpl))