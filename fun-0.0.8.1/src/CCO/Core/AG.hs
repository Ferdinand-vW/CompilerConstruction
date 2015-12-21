

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
--The int is used as an offset
--The a is used as a key. 
--The b is the Ref 
type Map a b = (Int,[(a,b)])

--Next will return the next offset of a type and add this as a global ref
next :: Map String Ref -> Ref
next (n,_) = Glob n

getOffSet :: Map String Ref -> Int
getOffSet (n,_) = n

--insertg will increment the offset and add the new global to the map
insertg :: Map String Ref -> String -> Map String Ref
insertg (n,xs) s = (n + 1,(s, Glob n) : xs)

--insertl is used to insert a new Var with a Loc to the map and increment the offset.
insertl :: Map String Ref -> String -> Map String Ref
insertl (n,xs) s = (n + 1, (s, Loc 0 n) : xs)

--insertl' is used to insert a value into the map and increment the offset.
insertref :: Map String Ref -> String -> Ref -> Map String Ref
insertref (n,xs) s r = (n + 1,(s,r) : xs)

--incrEnv is used to increment the lvldiff and resets the offset
incrEnv :: Map String Ref -> Map String Ref
incrEnv (n,lenv) = (0, incr lenv)
  where incr [] = []
        incr ((s,(Loc lvl x)):xs) = (s,Loc (lvl + 1) x) : incr xs
        incr ((s,r):xs) = (s,r) : incr xs

--This will increment the offset of the map
incrOffSet' :: Map String Ref -> Int -> Map String Ref
incrOffSet' (n,xs) i = (n + i, xs)
{-# LINE 72 "CCO/Core/AG.hs" #-}

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
{-# LINE 130 "CCO/Core/AG.hs" #-}

{-# LINE 39 "CCO\\Core\\AG\\Hm2Cr.ag" #-}


natExp :: Int -> Exp
natExp i = SExp $ Int i

varExp :: Map String Ref -> Map String Ref -> String -> Exp
varExp genv lenv s = SExp $ findVarRef genv lenv s

varBind :: Map String Ref -> String -> Exp
varBind genv s = Eval $ SExp $ findVarRef' genv s

--First it looks for the Ref in the local environment, if it cannot find it, then
--it will check the global environment. If it wasn't found, then the program crashes.
--It is assumed that the Ref exists in either one of the two environments. If it does not
--then there must be a programming error.
findVarRef :: Map String Ref -> Map String Ref -> String -> SExp
findVarRef (_,genv) (_,lenv) s = Var $ fromMaybe (fromJust (lookup s genv))  (lookup s lenv)

--Same as findVarRef except for just a single environment
findVarRef' :: Map String Ref -> String -> SExp
findVarRef' (_,env) s = Var $ fromJust $ lookup s env

toSExp :: Exp -> SExp
toSExp (SExp s) = s
{-# LINE 157 "CCO/Core/AG.hs" #-}

{-# LINE 74 "CCO\\Core\\AG\\Hm2Cr.ag" #-}



nilBind' :: Map String Ref -> Exp
nilBind' genv = SExp $ findVarRef' genv "nil"

--makes a new expression for lists. We first determine the length of the list, which we can then
--use to determine the offset for the top of the list.
mkCons :: Map String Ref -> Map String Ref -> ATm -> Exp
mkCons genv lenv atm = consExp genv lenv offset atm (SExp $ Var $ Loc 0 offset)
  where
    offset = listLength atm - 1 + (getOffSet lenv)

--Recursively go through the given list and add a let binding for each cons
consExp :: Map String Ref -> Map String Ref -> Int -> ATm -> Exp -> Exp
consExp genv lenv n (ACons t1 t2) t3 = case t2 of
                            ACons _ _ -> consExp genv lenv (n-1) t2 (evalVar (toSExp t1))
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
        evalVar r = Let (Bind curr $ App cons [r,Var prev]) t3

listLength :: ATm -> Int
listLength (ACons _ l) = 1 + listLength l
listLength _ = 0

{-# LINE 193 "CCO/Core/AG.hs" #-}

{-# LINE 118 "CCO\\Core\\AG\\Hm2Cr.ag" #-}


--We join the bindings of two consecutive lambda's, so that
--they do not appear as consecutive lambda's
joinLam :: RefL -> Exp -> Exp
joinLam xs (Lam ys t1) = Lam (xs ++ ys) t1
joinLam xs exp = Lam xs exp

--When we find a lambda we have to increment the lvls of all the variables
--already stored in the local environment. the function incrEnv does this for us.
--However if we have multiple consecutive lambda's they shouldn't all increment the lvl
--it should only happen once. newOffSetIncr does this for us, we also pass it the offset
--it should be given after insertion of all the lambda arguments.
updateEnv :: Map String Ref -> Int -> String -> Map String Ref
updateEnv lenv numArgs s = insertref (newOffSetIncr lenv (numArgs)) s (Loc 0 numArgs)

--We check whether this is the first lambda argument, if it is we increment the lvls of
--the local variables and pass it the new offset. Otherwise we only pass the new offset.
newOffSetIncr :: Map String Ref -> Int -> Map String Ref
newOffSetIncr (n,lenv) i
    | i >= 1 = (i,lenv)
    | i < 1 = (i,lenv')
    where (_,lenv') = incrEnv (n,lenv)

{-# LINE 220 "CCO/Core/AG.hs" #-}

{-# LINE 156 "CCO\\Core\\AG\\Hm2Cr.ag" #-}

--Before we do an application, we want to evaluate both arguments given.
--So we create a let binding to do both evaluations.
appExp :: Map String Ref -> Exp -> Exp -> Exp
appExp lenv (SExp sexp1) (SExp sexp2) = Let (Bind (Loc 0 $ getOffSet lenv) (Eval (SExp sexp2))) (
                                Let (Bind (Loc 0 $ getOffSet lenv + 1) (Eval (SExp sexp1))) $ 
                                    App (SExp $ Var $ Loc 0 (getOffSet lenv + 1)) [Var $ Loc 0 (getOffSet lenv)])


{-# LINE 232 "CCO/Core/AG.hs" #-}

{-# LINE 181 "CCO\\Core\\AG\\Hm2Cr.ag" #-}


--This function inserts a global let's variable into the local environment of it's own binding
--and thus creating recursion. As the variable was inserted into the global environment, we 
--simply have to ask for it
insertRecursion :: Map String Ref -> Map String Ref -> String -> Int -> Map String Ref
insertRecursion genv lenv s lvl 
  | lvl == 0 = insertref lenv s (getRef $ findVarRef' genv s)
  | otherwise = lenv

--GlobalLet only adds a new ref to the global map when its level is 0.
globalLet :: Map String Ref -> Int -> String -> Map String Ref
globalLet genv lvl x 
  | lvl == 0 = insertg genv x
  | otherwise = genv

--LocalLetLvl needs to be > 0, because it already contains a local lvl zero.
localLetLvl :: Int -> Int
localLetLvl 0 = 1
localLetLvl lvl = lvl

--LocalLet will only insert a new local environment, when its level is higher then 0
localLet :: Map String Ref -> Int -> String -> Map String Ref
localLet lenv lvl x
  | lvl > 0 = insertl lenv x
  | otherwise = lenv


--Thus function creates a let expression for us, but depending on its binding
--it might have to add a lambda
letExp :: Map String Ref -> Exp -> Exp -> Exp
letExp genv (Let b t1) exp2 = Let (Bind (next genv) (Lam [] (Let b t1))) exp2
letExp genv exp exp2 = Let (Bind (next genv) exp) exp2

--Similar to letExp except for bindings.
letBinds :: Map String Ref -> ATm -> Exp -> BindL -> BindL
letBinds (n,_) (ALam _ _) exp t2binds = Bind (Glob n) exp : t2binds
letBinds (n,_) (ANat _) exp t2binds = Bind (Glob n) exp : t2binds
letBinds (n,_) _ exp t2binds = Bind (Glob n) (Lam [] exp) : t2binds
{-# LINE 274 "CCO/Core/AG.hs" #-}

{-# LINE 231 "CCO\\Core\\AG\\Hm2Cr.ag" #-}

--toPrim is used to get SExp for the prim
toPrim :: Exp -> SExp
toPrim (SExp s) = Var $ Loc 0 (off s)
              where off (Var (Loc l o)) = o  
{-# LINE 282 "CCO/Core/AG.hs" #-}

{-# LINE 255 "CCO\\Core\\AG\\Hm2Cr.ag" #-}


--AddCases adds a Let before the case for the AIf expr
createCase :: Exp -> Exp -> Exp
createCase e1@(SExp s) e2 = Let (Bind (getRef s) (Eval e1)) e2

--EvalVar evaluates a SExp. This is neccesary in the AIf.
evalVar :: Exp -> Exp
evalVar v@(SExp (Var r)) = Eval v
evalVar exp = exp

getRef :: SExp -> Ref
getRef (Var x) = x
getRef _ = error "Pattern match does not exist."


{-# LINE 301 "CCO/Core/AG.hs" #-}

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

{-# LINE 327 "CCO/Core/AG.hs" #-}

{-# LINE 40 "CCO\\Core\\..\\AG\\AHM.ag" #-}

type Var = String
{-# LINE 332 "CCO/Core/AG.hs" #-}

{-# LINE 30 "CCO\\Core\\AG.ag" #-}

crprinter :: Component Mod String
crprinter = component $ \mod -> do
  let crmod = crmod_Syn_Mod (wrap_Mod (sem_Mod mod) Inh_Mod)
  return $ show $ printModule defaultEHCOpts crmod
{-# LINE 340 "CCO/Core/AG.hs" #-}
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
                  ({-# LINE 32 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   next _lhsIgenv
                   {-# LINE 401 "CCO/Core/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 33 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   natExp i_
                   {-# LINE 406 "CCO/Core/AG.hs" #-}
                   )
              _lhsObinds =
                  ({-# LINE 34 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   [Bind (next _lhsIgenv) $ natExp i_]
                   {-# LINE 411 "CCO/Core/AG.hs" #-}
                   )
              _tm =
                  ({-# LINE 22 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   ANat i_
                   {-# LINE 416 "CCO/Core/AG.hs" #-}
                   )
              _lhsOtm =
                  ({-# LINE 22 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tm
                   {-# LINE 421 "CCO/Core/AG.hs" #-}
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
                  ({-# LINE 35 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   next _lhsIgenv
                   {-# LINE 438 "CCO/Core/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 36 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   varExp _lhsIgenv _lhsIlenv x_
                   {-# LINE 443 "CCO/Core/AG.hs" #-}
                   )
              _lhsObinds =
                  ({-# LINE 37 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   [Bind (next _lhsIgenv) $ Lam [] $ varBind _lhsIgenv x_]
                   {-# LINE 448 "CCO/Core/AG.hs" #-}
                   )
              _tm =
                  ({-# LINE 22 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   AVar x_
                   {-# LINE 453 "CCO/Core/AG.hs" #-}
                   )
              _lhsOtm =
                  ({-# LINE 22 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tm
                   {-# LINE 458 "CCO/Core/AG.hs" #-}
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
                  ({-# LINE 67 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   next _lhsIgenv
                   {-# LINE 474 "CCO/Core/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 68 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   SExp $ findVarRef' _lhsIgenv "nil"
                   {-# LINE 479 "CCO/Core/AG.hs" #-}
                   )
              _lhsObinds =
                  ({-# LINE 69 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   [Bind (next _lhsIgenv) $ nilBind' _lhsIgenv]
                   {-# LINE 484 "CCO/Core/AG.hs" #-}
                   )
              _tm =
                  ({-# LINE 22 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   ANil
                   {-# LINE 489 "CCO/Core/AG.hs" #-}
                   )
              _lhsOtm =
                  ({-# LINE 22 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tm
                   {-# LINE 494 "CCO/Core/AG.hs" #-}
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
                  ({-# LINE 70 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   next _lhsIgenv
                   {-# LINE 528 "CCO/Core/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 71 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   mkCons _lhsIgenv _lhsIlenv (ACons _t1Itm _t2Itm)
                   {-# LINE 533 "CCO/Core/AG.hs" #-}
                   )
              _lhsObinds =
                  ({-# LINE 72 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   [Bind (next _lhsIgenv) $ mkCons _lhsIgenv _lhsIlenv (ACons _t1Itm _t2Itm)]
                   {-# LINE 538 "CCO/Core/AG.hs" #-}
                   )
              _tm =
                  ({-# LINE 22 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   ACons _t1Itm _t2Itm
                   {-# LINE 543 "CCO/Core/AG.hs" #-}
                   )
              _lhsOtm =
                  ({-# LINE 22 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tm
                   {-# LINE 548 "CCO/Core/AG.hs" #-}
                   )
              _t1Oargs =
                  ({-# LINE 29 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIargs
                   {-# LINE 553 "CCO/Core/AG.hs" #-}
                   )
              _t1Ogenv =
                  ({-# LINE 26 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 558 "CCO/Core/AG.hs" #-}
                   )
              _t1Olenv =
                  ({-# LINE 27 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlenv
                   {-# LINE 563 "CCO/Core/AG.hs" #-}
                   )
              _t1Olvl =
                  ({-# LINE 28 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl
                   {-# LINE 568 "CCO/Core/AG.hs" #-}
                   )
              _t2Oargs =
                  ({-# LINE 29 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIargs
                   {-# LINE 573 "CCO/Core/AG.hs" #-}
                   )
              _t2Ogenv =
                  ({-# LINE 26 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 578 "CCO/Core/AG.hs" #-}
                   )
              _t2Olenv =
                  ({-# LINE 27 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlenv
                   {-# LINE 583 "CCO/Core/AG.hs" #-}
                   )
              _t2Olvl =
                  ({-# LINE 28 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl
                   {-# LINE 588 "CCO/Core/AG.hs" #-}
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
                  ({-# LINE 223 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   next _lhsIgenv
                   {-# LINE 627 "CCO/Core/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 224 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   Prim f_ [toPrim _t1Iexp, toPrim _t2Iexp]
                   {-# LINE 632 "CCO/Core/AG.hs" #-}
                   )
              _lhsObinds =
                  ({-# LINE 225 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   []
                   {-# LINE 637 "CCO/Core/AG.hs" #-}
                   )
              _t1Ogenv =
                  ({-# LINE 226 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 642 "CCO/Core/AG.hs" #-}
                   )
              _t2Ogenv =
                  ({-# LINE 227 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 647 "CCO/Core/AG.hs" #-}
                   )
              _t1Olenv =
                  ({-# LINE 228 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlenv
                   {-# LINE 652 "CCO/Core/AG.hs" #-}
                   )
              _t2Olenv =
                  ({-# LINE 229 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlenv
                   {-# LINE 657 "CCO/Core/AG.hs" #-}
                   )
              _tm =
                  ({-# LINE 22 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   APrim f_ _t1Itm _t2Itm
                   {-# LINE 662 "CCO/Core/AG.hs" #-}
                   )
              _lhsOtm =
                  ({-# LINE 22 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tm
                   {-# LINE 667 "CCO/Core/AG.hs" #-}
                   )
              _t1Oargs =
                  ({-# LINE 29 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIargs
                   {-# LINE 672 "CCO/Core/AG.hs" #-}
                   )
              _t1Olvl =
                  ({-# LINE 28 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl
                   {-# LINE 677 "CCO/Core/AG.hs" #-}
                   )
              _t2Oargs =
                  ({-# LINE 29 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIargs
                   {-# LINE 682 "CCO/Core/AG.hs" #-}
                   )
              _t2Olvl =
                  ({-# LINE 28 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl
                   {-# LINE 687 "CCO/Core/AG.hs" #-}
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
                  ({-# LINE 111 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   next _lhsIgenv
                   {-# LINE 717 "CCO/Core/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 112 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   joinLam [Loc (0) $ 0] _t1Iexp
                   {-# LINE 722 "CCO/Core/AG.hs" #-}
                   )
              _lhsObinds =
                  ({-# LINE 113 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   []
                   {-# LINE 727 "CCO/Core/AG.hs" #-}
                   )
              _t1Ogenv =
                  ({-# LINE 114 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 732 "CCO/Core/AG.hs" #-}
                   )
              _t1Olenv =
                  ({-# LINE 115 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   updateEnv _lhsIlenv _lhsIargs x_
                   {-# LINE 737 "CCO/Core/AG.hs" #-}
                   )
              _t1Olvl =
                  ({-# LINE 116 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl + 1
                   {-# LINE 742 "CCO/Core/AG.hs" #-}
                   )
              _t1Oargs =
                  ({-# LINE 117 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   1 + _lhsIargs
                   {-# LINE 747 "CCO/Core/AG.hs" #-}
                   )
              _tm =
                  ({-# LINE 22 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   ALam x_ _t1Itm
                   {-# LINE 752 "CCO/Core/AG.hs" #-}
                   )
              _lhsOtm =
                  ({-# LINE 22 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tm
                   {-# LINE 757 "CCO/Core/AG.hs" #-}
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
                  ({-# LINE 146 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   next _lhsIgenv
                   {-# LINE 793 "CCO/Core/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 147 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   appExp _lhsIlenv _t1Iexp _t2Iexp
                   {-# LINE 798 "CCO/Core/AG.hs" #-}
                   )
              _lhsObinds =
                  ({-# LINE 148 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   [Bind (Glob (getOffSet _lhsIgenv)) (Lam [] $ appExp _lhsIlenv _t1Iexp _t2Iexp)]
                   {-# LINE 803 "CCO/Core/AG.hs" #-}
                   )
              _t1Ogenv =
                  ({-# LINE 149 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 808 "CCO/Core/AG.hs" #-}
                   )
              _t2Ogenv =
                  ({-# LINE 150 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 813 "CCO/Core/AG.hs" #-}
                   )
              _t1Olenv =
                  ({-# LINE 151 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlenv
                   {-# LINE 818 "CCO/Core/AG.hs" #-}
                   )
              _t2Olenv =
                  ({-# LINE 152 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlenv
                   {-# LINE 823 "CCO/Core/AG.hs" #-}
                   )
              _t1Olvl =
                  ({-# LINE 153 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl
                   {-# LINE 828 "CCO/Core/AG.hs" #-}
                   )
              _t2Olvl =
                  ({-# LINE 154 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl
                   {-# LINE 833 "CCO/Core/AG.hs" #-}
                   )
              _tm =
                  ({-# LINE 22 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   AApp _t1Itm _t2Itm
                   {-# LINE 838 "CCO/Core/AG.hs" #-}
                   )
              _lhsOtm =
                  ({-# LINE 22 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tm
                   {-# LINE 843 "CCO/Core/AG.hs" #-}
                   )
              _t1Oargs =
                  ({-# LINE 29 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIargs
                   {-# LINE 848 "CCO/Core/AG.hs" #-}
                   )
              _t2Oargs =
                  ({-# LINE 29 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIargs
                   {-# LINE 853 "CCO/Core/AG.hs" #-}
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
                  ({-# LINE 168 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _t2Imain
                   {-# LINE 892 "CCO/Core/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 169 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   letExp _lhsIgenv _t1Iexp _t2Iexp
                   {-# LINE 897 "CCO/Core/AG.hs" #-}
                   )
              _lhsObinds =
                  ({-# LINE 170 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   letBinds _lhsIgenv _t1Itm _t1Iexp _t2Ibinds
                   {-# LINE 902 "CCO/Core/AG.hs" #-}
                   )
              _t1Ogenv =
                  ({-# LINE 171 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 907 "CCO/Core/AG.hs" #-}
                   )
              _genv =
                  ({-# LINE 172 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   globalLet _lhsIgenv _lhsIlvl x_
                   {-# LINE 912 "CCO/Core/AG.hs" #-}
                   )
              _t2Ogenv =
                  ({-# LINE 173 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _genv
                   {-# LINE 917 "CCO/Core/AG.hs" #-}
                   )
              _t1Olenv =
                  ({-# LINE 174 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   incrEnv $ insertRecursion _genv     _lhsIlenv x_ _lhsIlvl
                   {-# LINE 922 "CCO/Core/AG.hs" #-}
                   )
              _t2Olenv =
                  ({-# LINE 175 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   localLet _lhsIlenv _lhsIlvl x_
                   {-# LINE 927 "CCO/Core/AG.hs" #-}
                   )
              _t1Olvl =
                  ({-# LINE 176 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   localLetLvl _lhsIlvl
                   {-# LINE 932 "CCO/Core/AG.hs" #-}
                   )
              _t2Olvl =
                  ({-# LINE 177 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl
                   {-# LINE 937 "CCO/Core/AG.hs" #-}
                   )
              _t1Oargs =
                  ({-# LINE 178 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   0
                   {-# LINE 942 "CCO/Core/AG.hs" #-}
                   )
              _t2Oargs =
                  ({-# LINE 179 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   0
                   {-# LINE 947 "CCO/Core/AG.hs" #-}
                   )
              _tm =
                  ({-# LINE 22 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   ALet x_ _t1Itm _t2Itm
                   {-# LINE 952 "CCO/Core/AG.hs" #-}
                   )
              _lhsOtm =
                  ({-# LINE 22 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tm
                   {-# LINE 957 "CCO/Core/AG.hs" #-}
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
                  ({-# LINE 239 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   Glob (getOffSet _lhsIgenv)
                   {-# LINE 1004 "CCO/Core/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 240 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   createCase _expIexp _exp
                   {-# LINE 1009 "CCO/Core/AG.hs" #-}
                   )
              _exp =
                  ({-# LINE 241 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   Case (Var $ Tag $ Loc 0 $ getOffSet _lhsIlenv) [evalVar _t2Iexp, evalVar _t1Iexp]
                   {-# LINE 1014 "CCO/Core/AG.hs" #-}
                   )
              _lhsObinds =
                  ({-# LINE 242 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   [Bind (Glob (getOffSet _lhsIgenv)) $ Lam [] $ createCase _expIexp _exp    ]
                   {-# LINE 1019 "CCO/Core/AG.hs" #-}
                   )
              _expOgenv =
                  ({-# LINE 243 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 1024 "CCO/Core/AG.hs" #-}
                   )
              _t1Ogenv =
                  ({-# LINE 244 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 1029 "CCO/Core/AG.hs" #-}
                   )
              _t2Ogenv =
                  ({-# LINE 245 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 1034 "CCO/Core/AG.hs" #-}
                   )
              _expOlenv =
                  ({-# LINE 246 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlenv
                   {-# LINE 1039 "CCO/Core/AG.hs" #-}
                   )
              _t1Olenv =
                  ({-# LINE 247 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   incrOffSet' _lhsIlenv 1
                   {-# LINE 1044 "CCO/Core/AG.hs" #-}
                   )
              _t2Olenv =
                  ({-# LINE 248 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   incrOffSet' _lhsIlenv 1
                   {-# LINE 1049 "CCO/Core/AG.hs" #-}
                   )
              _expOlvl =
                  ({-# LINE 249 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl + 1
                   {-# LINE 1054 "CCO/Core/AG.hs" #-}
                   )
              _t1Olvl =
                  ({-# LINE 250 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl + 1
                   {-# LINE 1059 "CCO/Core/AG.hs" #-}
                   )
              _t2Olvl =
                  ({-# LINE 251 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl + 1
                   {-# LINE 1064 "CCO/Core/AG.hs" #-}
                   )
              _expOargs =
                  ({-# LINE 252 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   0
                   {-# LINE 1069 "CCO/Core/AG.hs" #-}
                   )
              _t1Oargs =
                  ({-# LINE 253 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   0
                   {-# LINE 1074 "CCO/Core/AG.hs" #-}
                   )
              _t2Oargs =
                  ({-# LINE 254 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   0
                   {-# LINE 1079 "CCO/Core/AG.hs" #-}
                   )
              _tm =
                  ({-# LINE 22 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   AIf _expItm _t1Itm _t2Itm
                   {-# LINE 1084 "CCO/Core/AG.hs" #-}
                   )
              _lhsOtm =
                  ({-# LINE 22 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tm
                   {-# LINE 1089 "CCO/Core/AG.hs" #-}
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
                   {-# LINE 1132 "CCO/Core/AG.hs" #-}
                   )
              _xexpOstkoff =
                  ({-# LINE 91 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   0
                   {-# LINE 1137 "CCO/Core/AG.hs" #-}
                   )
              _lhsOstkoff =
                  ({-# LINE 92 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff + 1
                   {-# LINE 1142 "CCO/Core/AG.hs" #-}
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
                   {-# LINE 1183 "CCO/Core/AG.hs" #-}
                   )
              _lhsOstkoff =
                  ({-# LINE 84 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _tlIstkoff
                   {-# LINE 1188 "CCO/Core/AG.hs" #-}
                   )
              _hdOstkoff =
                  ({-# LINE 84 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 1193 "CCO/Core/AG.hs" #-}
                   )
              _tlOstkoff =
                  ({-# LINE 84 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _hdIstkoff
                   {-# LINE 1198 "CCO/Core/AG.hs" #-}
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
                   {-# LINE 1213 "CCO/Core/AG.hs" #-}
                   )
              _lhsOstkoff =
                  ({-# LINE 84 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 1218 "CCO/Core/AG.hs" #-}
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
              {-# LINE 1253 "CCO/Core/AG.hs" #-}
              )
         _tmOgenv =
             ({-# LINE 14 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
              (8,[("True", Glob 7),("False", Glob 6),("isNil", Glob 5),
                ("isCons", Glob 4),("tail", Glob 3),("head", Glob 2),
                ("cons", Glob 1),("nil",Glob 0)])
              {-# LINE 1260 "CCO/Core/AG.hs" #-}
              )
         _tmOlenv =
             ({-# LINE 17 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
              (0,[])
              {-# LINE 1265 "CCO/Core/AG.hs" #-}
              )
         _tmOlvl =
             ({-# LINE 18 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
              0
              {-# LINE 1270 "CCO/Core/AG.hs" #-}
              )
         _tmOargs =
             ({-# LINE 19 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
              0
              {-# LINE 1275 "CCO/Core/AG.hs" #-}
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
                   {-# LINE 1345 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 55 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 1350 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 1355 "CCO/Core/AG.hs" #-}
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
                   {-# LINE 1368 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 55 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 1373 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 1378 "CCO/Core/AG.hs" #-}
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
                   {-# LINE 1389 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 55 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 1394 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 1399 "CCO/Core/AG.hs" #-}
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
                   {-# LINE 1435 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 55 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 1440 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 1445 "CCO/Core/AG.hs" #-}
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
                   {-# LINE 1466 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 55 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 1471 "CCO/Core/AG.hs" #-}
                   )
              _bodyOstkoff =
                  ({-# LINE 95 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   length _argsIcrrefl
                   {-# LINE 1476 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 1481 "CCO/Core/AG.hs" #-}
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
                   {-# LINE 1502 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 55 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 1507 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 1512 "CCO/Core/AG.hs" #-}
                   )
              _funcOstkoff =
                  ({-# LINE 82 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 1517 "CCO/Core/AG.hs" #-}
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
                   {-# LINE 1535 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 55 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 1540 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 1545 "CCO/Core/AG.hs" #-}
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
                   {-# LINE 1561 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 55 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 1566 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 1571 "CCO/Core/AG.hs" #-}
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
                   {-# LINE 1589 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 55 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 1594 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 1599 "CCO/Core/AG.hs" #-}
                   )
              _altsOstkoff =
                  ({-# LINE 82 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 1604 "CCO/Core/AG.hs" #-}
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
                   {-# LINE 1627 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 55 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 1632 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 1637 "CCO/Core/AG.hs" #-}
                   )
              _bindOstkoff =
                  ({-# LINE 84 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 1642 "CCO/Core/AG.hs" #-}
                   )
              _bodyOstkoff =
                  ({-# LINE 82 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _bindIstkoff
                   {-# LINE 1647 "CCO/Core/AG.hs" #-}
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
                   {-# LINE 1688 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 55 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 1693 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 1698 "CCO/Core/AG.hs" #-}
                   )
              _bodyOstkoff =
                  ({-# LINE 82 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 1703 "CCO/Core/AG.hs" #-}
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
                   {-# LINE 1740 "CCO/Core/AG.hs" #-}
                   )
              _hdOstkoff =
                  ({-# LINE 82 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 1745 "CCO/Core/AG.hs" #-}
                   )
              _tlOstkoff =
                  ({-# LINE 82 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 1750 "CCO/Core/AG.hs" #-}
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
                   {-# LINE 1764 "CCO/Core/AG.hs" #-}
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
              {-# LINE 1800 "CCO/Core/AG.hs" #-}
              )
         _bindsOstkoff =
             ({-# LINE 87 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              0
              {-# LINE 1805 "CCO/Core/AG.hs" #-}
              )
         _mainOstkoff =
             ({-# LINE 88 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              _bindsIstkoff
              {-# LINE 1810 "CCO/Core/AG.hs" #-}
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
              {-# LINE 1852 "CCO/Core/AG.hs" #-}
              )
         _lhsOcrref =
             ({-# LINE 71 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              CR.mkModRef offset_
              {-# LINE 1857 "CCO/Core/AG.hs" #-}
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
              {-# LINE 1869 "CCO/Core/AG.hs" #-}
              )
         _lhsOcrref =
             ({-# LINE 73 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              CR.mkLocDifRef levdiff_ offset_
              {-# LINE 1874 "CCO/Core/AG.hs" #-}
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
              {-# LINE 1887 "CCO/Core/AG.hs" #-}
              )
         _lhsOcrref =
             ({-# LINE 67 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              _refIcrref
              {-# LINE 1892 "CCO/Core/AG.hs" #-}
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
              {-# LINE 1908 "CCO/Core/AG.hs" #-}
              )
         _lhsOcrref =
             ({-# LINE 67 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              _refIcrref
              {-# LINE 1913 "CCO/Core/AG.hs" #-}
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
              {-# LINE 1946 "CCO/Core/AG.hs" #-}
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
              {-# LINE 1959 "CCO/Core/AG.hs" #-}
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
              {-# LINE 1990 "CCO/Core/AG.hs" #-}
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
              {-# LINE 2002 "CCO/Core/AG.hs" #-}
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
              {-# LINE 2034 "CCO/Core/AG.hs" #-}
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
              {-# LINE 2047 "CCO/Core/AG.hs" #-}
              )
     in  ( _lhsOcrsexpl))