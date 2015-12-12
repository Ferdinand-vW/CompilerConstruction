

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
{-# LINE 21 "CCO/Core/AG.hs" #-}

{-# LINE 2 "CCO\\Core\\..\\AG\\AHM.ag" #-}

import CCO.Tree (Tree (fromTree, toTree))
import qualified CCO.Tree as T (ATerm (App))
import CCO.Tree.Parser (parseTree, app, arg)
{-# LINE 28 "CCO/Core/AG.hs" #-}

{-# LINE 2 "CCO\\Core\\..\\AG\\Core.ag" #-}

import qualified UHC.Light.Compiler.CoreRun.API as CR
{-# LINE 33 "CCO/Core/AG.hs" #-}
{-# LINE 6 "CCO\\Core\\AG\\Hm2Cr.ag" #-}

type Map a b = [(a,b)]
{-# LINE 37 "CCO/Core/AG.hs" #-}

{-# LINE 75 "CCO\\Core\\AG\\Hm2Cr.ag" #-}


joinLam :: RefL -> Exp -> Exp
joinLam xs (Lam ys t1) = Lam (xs ++ ys) t1
joinLam xs exp = Lam xs exp

fromExp :: Exp -> SExp
fromExp (SExp s) = s

fromPrim :: Int -> Exp -> SExp
fromPrim lvl (SExp s) = Var $ Loc lvl (off s)
              where off (Var (Loc l o)) = o


findVarRef :: Map String Ref -> Map String Ref -> ATm -> SExp
findVarRef genv lenv (AVar x) = Var $ fromMaybe (fromJust (lookup x genv)) (lookup x lenv)

getOffSet :: Map String Ref -> Int
getOffSet xs = length xs

insertg :: Map String Ref -> String -> Map String Ref
insertg xs s = (s, Glob $ getOffSet xs) : xs

insertl :: Map String Ref -> Int -> String -> Map String Ref
insertl xs lvl s = (s, Loc lvl $ getOffSet xs) : xs
{-# LINE 65 "CCO/Core/AG.hs" #-}

{-# LINE 9 "CCO\\Core\\..\\AG\\AHM.ag" #-}

instance Tree ATm where
  fromTree (ANat x)       = T.App "ANat" [fromTree x]
  fromTree (AVar x)       = T.App "AVar" [fromTree x]
  fromTree (APrim f t1 t2) = T.App "APrim" [fromTree f, fromTree t1, fromTree t2]
  fromTree (ALam x t1)    = T.App "ALam" [fromTree x, fromTree t1]
  fromTree (AApp t1 t2)   = T.App "AApp" [fromTree t1, fromTree t2]
  fromTree (ALet x t1 t2) = T.App "ALet" [fromTree x, fromTree t1, fromTree t2]

  toTree = parseTree [ app "ANat" (ANat <$> arg                )
                     , app "AVar" (AVar <$> arg                )
                     , app "APrim" (APrim <$> arg <*> arg <*> arg)
                     , app "ALam" (ALam <$> arg <*> arg        )
                     , app "AApp" (AApp <$> arg <*> arg        )
                     , app "ALet" (ALet <$> arg <*> arg <*> arg)
                     ]

{-# LINE 85 "CCO/Core/AG.hs" #-}

{-# LINE 33 "CCO\\Core\\..\\AG\\AHM.ag" #-}

type Var = String
{-# LINE 90 "CCO/Core/AG.hs" #-}

{-# LINE 28 "CCO\\Core\\AG.ag" #-}

crprinter :: Component Mod String
crprinter = component $ \mod -> do
  let crmod = crmod_Syn_Mod (wrap_Mod (sem_Mod mod) Inh_Mod)
  return $ show $ printModule defaultEHCOpts crmod
{-# LINE 98 "CCO/Core/AG.hs" #-}
-- ATm ---------------------------------------------------------
data ATm = ANat (Int)
         | AVar (Var)
         | APrim (Var) (ATm) (ATm)
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
sem_ATm (APrim _f _t1 _t2) =
    (sem_ATm_APrim _f (sem_ATm _t1) (sem_ATm _t2))
sem_ATm (ALam _x _t1) =
    (sem_ATm_ALam _x (sem_ATm _t1))
sem_ATm (AApp _t1 _t2) =
    (sem_ATm_AApp (sem_ATm _t1) (sem_ATm _t2))
sem_ATm (ALet _x _t1 _t2) =
    (sem_ATm_ALet _x (sem_ATm _t1) (sem_ATm _t2))
-- semantic domain
type T_ATm = (Map String Ref) ->
             (Map String Ref) ->
             Int ->
             ( BindL,Exp,Ref,ATm)
data Inh_ATm = Inh_ATm {genv_Inh_ATm :: (Map String Ref),lenv_Inh_ATm :: (Map String Ref),lvl_Inh_ATm :: Int}
data Syn_ATm = Syn_ATm {binds_Syn_ATm :: BindL,exp_Syn_ATm :: Exp,main_Syn_ATm :: Ref,tm_Syn_ATm :: ATm}
wrap_ATm :: T_ATm ->
            Inh_ATm ->
            Syn_ATm
wrap_ATm sem (Inh_ATm _lhsIgenv _lhsIlenv _lhsIlvl) =
    (let ( _lhsObinds,_lhsOexp,_lhsOmain,_lhsOtm) = sem _lhsIgenv _lhsIlenv _lhsIlvl
     in  (Syn_ATm _lhsObinds _lhsOexp _lhsOmain _lhsOtm))
sem_ATm_ANat :: Int ->
                T_ATm
sem_ATm_ANat i_ =
    (\ _lhsIgenv
       _lhsIlenv
       _lhsIlvl ->
         (let _lhsOmain :: Ref
              _lhsOexp :: Exp
              _lhsObinds :: BindL
              _lhsOtm :: ATm
              _lhsOmain =
                  ({-# LINE 34 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   Glob (getOffSet _lhsIgenv)
                   {-# LINE 147 "CCO/Core/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 35 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   SExp (Int i_)
                   {-# LINE 152 "CCO/Core/AG.hs" #-}
                   )
              _lhsObinds =
                  ({-# LINE 36 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   []
                   {-# LINE 157 "CCO/Core/AG.hs" #-}
                   )
              _tm =
                  ({-# LINE 25 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   ANat i_
                   {-# LINE 162 "CCO/Core/AG.hs" #-}
                   )
              _lhsOtm =
                  ({-# LINE 25 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tm
                   {-# LINE 167 "CCO/Core/AG.hs" #-}
                   )
          in  ( _lhsObinds,_lhsOexp,_lhsOmain,_lhsOtm)))
sem_ATm_AVar :: Var ->
                T_ATm
sem_ATm_AVar x_ =
    (\ _lhsIgenv
       _lhsIlenv
       _lhsIlvl ->
         (let _lhsOmain :: Ref
              _lhsOexp :: Exp
              _lhsObinds :: BindL
              _lhsOtm :: ATm
              _lhsOmain =
                  ({-# LINE 37 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   Glob (getOffSet _lhsIgenv)
                   {-# LINE 183 "CCO/Core/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 38 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   SExp $ findVarRef _lhsIgenv _lhsIlenv (AVar x_)
                   {-# LINE 188 "CCO/Core/AG.hs" #-}
                   )
              _lhsObinds =
                  ({-# LINE 39 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   []
                   {-# LINE 193 "CCO/Core/AG.hs" #-}
                   )
              _tm =
                  ({-# LINE 25 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   AVar x_
                   {-# LINE 198 "CCO/Core/AG.hs" #-}
                   )
              _lhsOtm =
                  ({-# LINE 25 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tm
                   {-# LINE 203 "CCO/Core/AG.hs" #-}
                   )
          in  ( _lhsObinds,_lhsOexp,_lhsOmain,_lhsOtm)))
sem_ATm_APrim :: Var ->
                 T_ATm ->
                 T_ATm ->
                 T_ATm
sem_ATm_APrim f_ t1_ t2_ =
    (\ _lhsIgenv
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
              _t1Ibinds :: BindL
              _t1Iexp :: Exp
              _t1Imain :: Ref
              _t1Itm :: ATm
              _t2Ibinds :: BindL
              _t2Iexp :: Exp
              _t2Imain :: Ref
              _t2Itm :: ATm
              _lhsOmain =
                  ({-# LINE 64 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   Glob (getOffSet _lhsIgenv)
                   {-# LINE 235 "CCO/Core/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 65 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   Prim f_ [fromPrim _lhsIlvl _t1Iexp, fromPrim _lhsIlvl _t2Iexp]
                   {-# LINE 240 "CCO/Core/AG.hs" #-}
                   )
              _lhsObinds =
                  ({-# LINE 66 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   []
                   {-# LINE 245 "CCO/Core/AG.hs" #-}
                   )
              _t1Ogenv =
                  ({-# LINE 67 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 250 "CCO/Core/AG.hs" #-}
                   )
              _t2Ogenv =
                  ({-# LINE 68 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 255 "CCO/Core/AG.hs" #-}
                   )
              _t1Olenv =
                  ({-# LINE 69 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlenv
                   {-# LINE 260 "CCO/Core/AG.hs" #-}
                   )
              _t2Olenv =
                  ({-# LINE 70 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlenv
                   {-# LINE 265 "CCO/Core/AG.hs" #-}
                   )
              _t1Olvl =
                  ({-# LINE 71 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl
                   {-# LINE 270 "CCO/Core/AG.hs" #-}
                   )
              _t2Olvl =
                  ({-# LINE 72 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl
                   {-# LINE 275 "CCO/Core/AG.hs" #-}
                   )
              _tm =
                  ({-# LINE 25 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   APrim f_ _t1Itm _t2Itm
                   {-# LINE 280 "CCO/Core/AG.hs" #-}
                   )
              _lhsOtm =
                  ({-# LINE 25 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tm
                   {-# LINE 285 "CCO/Core/AG.hs" #-}
                   )
              ( _t1Ibinds,_t1Iexp,_t1Imain,_t1Itm) =
                  t1_ _t1Ogenv _t1Olenv _t1Olvl
              ( _t2Ibinds,_t2Iexp,_t2Imain,_t2Itm) =
                  t2_ _t2Ogenv _t2Olenv _t2Olvl
          in  ( _lhsObinds,_lhsOexp,_lhsOmain,_lhsOtm)))
sem_ATm_ALam :: Var ->
                T_ATm ->
                T_ATm
sem_ATm_ALam x_ t1_ =
    (\ _lhsIgenv
       _lhsIlenv
       _lhsIlvl ->
         (let _lhsOmain :: Ref
              _lhsOexp :: Exp
              _lhsObinds :: BindL
              _t1Ogenv :: (Map String Ref)
              _t1Olenv :: (Map String Ref)
              _t1Olvl :: Int
              _lhsOtm :: ATm
              _t1Ibinds :: BindL
              _t1Iexp :: Exp
              _t1Imain :: Ref
              _t1Itm :: ATm
              _lhsOmain =
                  ({-# LINE 40 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   Glob (getOffSet _lhsIgenv)
                   {-# LINE 313 "CCO/Core/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 41 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   joinLam [Loc 0 $ getOffSet _lhsIlenv] _t1Iexp
                   {-# LINE 318 "CCO/Core/AG.hs" #-}
                   )
              _lhsObinds =
                  ({-# LINE 42 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   []
                   {-# LINE 323 "CCO/Core/AG.hs" #-}
                   )
              _t1Ogenv =
                  ({-# LINE 43 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 328 "CCO/Core/AG.hs" #-}
                   )
              _t1Olenv =
                  ({-# LINE 44 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   insertl _lhsIlenv 0 x_
                   {-# LINE 333 "CCO/Core/AG.hs" #-}
                   )
              _t1Olvl =
                  ({-# LINE 45 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl + 1
                   {-# LINE 338 "CCO/Core/AG.hs" #-}
                   )
              _tm =
                  ({-# LINE 25 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   ALam x_ _t1Itm
                   {-# LINE 343 "CCO/Core/AG.hs" #-}
                   )
              _lhsOtm =
                  ({-# LINE 25 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tm
                   {-# LINE 348 "CCO/Core/AG.hs" #-}
                   )
              ( _t1Ibinds,_t1Iexp,_t1Imain,_t1Itm) =
                  t1_ _t1Ogenv _t1Olenv _t1Olvl
          in  ( _lhsObinds,_lhsOexp,_lhsOmain,_lhsOtm)))
sem_ATm_AApp :: T_ATm ->
                T_ATm ->
                T_ATm
sem_ATm_AApp t1_ t2_ =
    (\ _lhsIgenv
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
              _t1Ibinds :: BindL
              _t1Iexp :: Exp
              _t1Imain :: Ref
              _t1Itm :: ATm
              _t2Ibinds :: BindL
              _t2Iexp :: Exp
              _t2Imain :: Ref
              _t2Itm :: ATm
              _lhsOmain =
                  ({-# LINE 46 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   Glob (getOffSet _lhsIgenv)
                   {-# LINE 381 "CCO/Core/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 47 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   App (_t1Iexp) [fromExp _t2Iexp]
                   {-# LINE 386 "CCO/Core/AG.hs" #-}
                   )
              _lhsObinds =
                  ({-# LINE 48 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   [Bind (Glob (getOffSet _lhsIgenv)) (App (_t1Iexp) [fromExp _t2Iexp])]
                   {-# LINE 391 "CCO/Core/AG.hs" #-}
                   )
              _t1Ogenv =
                  ({-# LINE 49 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 396 "CCO/Core/AG.hs" #-}
                   )
              _t2Ogenv =
                  ({-# LINE 50 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 401 "CCO/Core/AG.hs" #-}
                   )
              _t1Olenv =
                  ({-# LINE 51 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlenv
                   {-# LINE 406 "CCO/Core/AG.hs" #-}
                   )
              _t2Olenv =
                  ({-# LINE 52 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlenv
                   {-# LINE 411 "CCO/Core/AG.hs" #-}
                   )
              _t1Olvl =
                  ({-# LINE 53 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl
                   {-# LINE 416 "CCO/Core/AG.hs" #-}
                   )
              _t2Olvl =
                  ({-# LINE 54 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl
                   {-# LINE 421 "CCO/Core/AG.hs" #-}
                   )
              _tm =
                  ({-# LINE 25 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   AApp _t1Itm _t2Itm
                   {-# LINE 426 "CCO/Core/AG.hs" #-}
                   )
              _lhsOtm =
                  ({-# LINE 25 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tm
                   {-# LINE 431 "CCO/Core/AG.hs" #-}
                   )
              ( _t1Ibinds,_t1Iexp,_t1Imain,_t1Itm) =
                  t1_ _t1Ogenv _t1Olenv _t1Olvl
              ( _t2Ibinds,_t2Iexp,_t2Imain,_t2Itm) =
                  t2_ _t2Ogenv _t2Olenv _t2Olvl
          in  ( _lhsObinds,_lhsOexp,_lhsOmain,_lhsOtm)))
sem_ATm_ALet :: Var ->
                T_ATm ->
                T_ATm ->
                T_ATm
sem_ATm_ALet x_ t1_ t2_ =
    (\ _lhsIgenv
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
              _t1Ibinds :: BindL
              _t1Iexp :: Exp
              _t1Imain :: Ref
              _t1Itm :: ATm
              _t2Ibinds :: BindL
              _t2Iexp :: Exp
              _t2Imain :: Ref
              _t2Itm :: ATm
              _lhsOmain =
                  ({-# LINE 55 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _t2Imain
                   {-# LINE 467 "CCO/Core/AG.hs" #-}
                   )
              _lhsOexp =
                  ({-# LINE 56 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   Let (Bind (Glob (getOffSet _lhsIgenv)) _t1Iexp) _t2Iexp
                   {-# LINE 472 "CCO/Core/AG.hs" #-}
                   )
              _lhsObinds =
                  ({-# LINE 57 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   Bind (Glob (getOffSet _lhsIgenv)) _t1Iexp : _t2Ibinds
                   {-# LINE 477 "CCO/Core/AG.hs" #-}
                   )
              _t1Ogenv =
                  ({-# LINE 58 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIgenv
                   {-# LINE 482 "CCO/Core/AG.hs" #-}
                   )
              _t2Ogenv =
                  ({-# LINE 59 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   insertg _lhsIgenv x_
                   {-# LINE 487 "CCO/Core/AG.hs" #-}
                   )
              _t1Olenv =
                  ({-# LINE 60 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlenv
                   {-# LINE 492 "CCO/Core/AG.hs" #-}
                   )
              _t2Olenv =
                  ({-# LINE 61 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlenv
                   {-# LINE 497 "CCO/Core/AG.hs" #-}
                   )
              _t1Olvl =
                  ({-# LINE 62 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl
                   {-# LINE 502 "CCO/Core/AG.hs" #-}
                   )
              _t2Olvl =
                  ({-# LINE 63 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIlvl
                   {-# LINE 507 "CCO/Core/AG.hs" #-}
                   )
              _tm =
                  ({-# LINE 25 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   ALet x_ _t1Itm _t2Itm
                   {-# LINE 512 "CCO/Core/AG.hs" #-}
                   )
              _lhsOtm =
                  ({-# LINE 25 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tm
                   {-# LINE 517 "CCO/Core/AG.hs" #-}
                   )
              ( _t1Ibinds,_t1Iexp,_t1Imain,_t1Itm) =
                  t1_ _t1Ogenv _t1Olenv _t1Olvl
              ( _t2Ibinds,_t2Iexp,_t2Imain,_t2Itm) =
                  t2_ _t2Ogenv _t2Olenv _t2Olvl
          in  ( _lhsObinds,_lhsOexp,_lhsOmain,_lhsOtm)))
-- Bind --------------------------------------------------------
data Bind = Bind (Ref) (Exp)
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
              _xIcrrefl :: ([CR.RRef])
              _xexpIcrexp :: (CR.Exp)
              _xexpIcrexpl :: ([CR.Exp])
              _lhsOcrbindl =
                  ({-# LINE 46 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_xexpIcrexp]
                   {-# LINE 556 "CCO/Core/AG.hs" #-}
                   )
              _xexpOstkoff =
                  ({-# LINE 70 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   0
                   {-# LINE 561 "CCO/Core/AG.hs" #-}
                   )
              _lhsOstkoff =
                  ({-# LINE 71 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff + 1
                   {-# LINE 566 "CCO/Core/AG.hs" #-}
                   )
              ( _xIcrrefl) =
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
                  ({-# LINE 43 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _hdIcrbindl ++ _tlIcrbindl
                   {-# LINE 607 "CCO/Core/AG.hs" #-}
                   )
              _lhsOstkoff =
                  ({-# LINE 63 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _tlIstkoff
                   {-# LINE 612 "CCO/Core/AG.hs" #-}
                   )
              _hdOstkoff =
                  ({-# LINE 63 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 617 "CCO/Core/AG.hs" #-}
                   )
              _tlOstkoff =
                  ({-# LINE 63 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _hdIstkoff
                   {-# LINE 622 "CCO/Core/AG.hs" #-}
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
                  ({-# LINE 43 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   []
                   {-# LINE 637 "CCO/Core/AG.hs" #-}
                   )
              _lhsOstkoff =
                  ({-# LINE 63 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 642 "CCO/Core/AG.hs" #-}
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
         _tmIbinds :: BindL
         _tmIexp :: Exp
         _tmImain :: Ref
         _tmItm :: ATm
         _lhsOcore =
             ({-# LINE 18 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
              Mod (SExp (Var _tmImain)) _tmIbinds
              {-# LINE 676 "CCO/Core/AG.hs" #-}
              )
         _tmOgenv =
             ({-# LINE 19 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
              []
              {-# LINE 681 "CCO/Core/AG.hs" #-}
              )
         _tmOlenv =
             ({-# LINE 20 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
              []
              {-# LINE 686 "CCO/Core/AG.hs" #-}
              )
         _tmOlvl =
             ({-# LINE 21 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
              -1
              {-# LINE 691 "CCO/Core/AG.hs" #-}
              )
         ( _tmIbinds,_tmIexp,_tmImain,_tmItm) =
             tm_ _tmOgenv _tmOlenv _tmOlvl
     in  ( _lhsOcore))
-- Exp ---------------------------------------------------------
data Exp = SExp (SExp)
         | Lam (RefL) (Exp)
         | App (Exp) (SExpL)
         | Prim (String) (SExpL)
         | Node (Int) (SExpL)
         | Case (SExp) (ExpL)
         | Let (Bind) (Exp)
         | Dbg (String)
-- cata
sem_Exp :: Exp ->
           T_Exp
sem_Exp (SExp _sexp) =
    (sem_Exp_SExp (sem_SExp _sexp))
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
                  ({-# LINE 30 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   CR.mkExp (head _sexpIcrsexpl)
                   {-# LINE 745 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 40 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 750 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 25 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 755 "CCO/Core/AG.hs" #-}
                   )
              ( _sexpIcrsexpl) =
                  sexp_
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
                  ({-# LINE 31 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   CR.mkLam (length _argsIcrrefl) 100 _bodyIcrexp
                   {-# LINE 774 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 40 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 779 "CCO/Core/AG.hs" #-}
                   )
              _bodyOstkoff =
                  ({-# LINE 74 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   length _argsIcrrefl
                   {-# LINE 784 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 25 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 789 "CCO/Core/AG.hs" #-}
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
                  ({-# LINE 32 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   CR.mkApp _funcIcrexp _argsIcrsexpl
                   {-# LINE 810 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 40 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 815 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 25 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 820 "CCO/Core/AG.hs" #-}
                   )
              _funcOstkoff =
                  ({-# LINE 61 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 825 "CCO/Core/AG.hs" #-}
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
                  ({-# LINE 33 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   CR.mkFFI func_       _argsIcrsexpl
                   {-# LINE 843 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 40 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 848 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 25 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 853 "CCO/Core/AG.hs" #-}
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
                  ({-# LINE 34 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   CR.mkTup tag_        _argsIcrsexpl
                   {-# LINE 869 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 40 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 874 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 25 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 879 "CCO/Core/AG.hs" #-}
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
                  ({-# LINE 35 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   CR.mkCase (head _sexpIcrsexpl) _altsIcrexpl
                   {-# LINE 897 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 40 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 902 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 25 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 907 "CCO/Core/AG.hs" #-}
                   )
              _altsOstkoff =
                  ({-# LINE 61 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 912 "CCO/Core/AG.hs" #-}
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
                  ({-# LINE 36 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   CR.mkLet _lhsIstkoff _bindIcrbindl _bodyIcrexp
                   {-# LINE 935 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 40 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 940 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 25 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 945 "CCO/Core/AG.hs" #-}
                   )
              _bindOstkoff =
                  ({-# LINE 63 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 950 "CCO/Core/AG.hs" #-}
                   )
              _bodyOstkoff =
                  ({-# LINE 61 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _bindIstkoff
                   {-# LINE 955 "CCO/Core/AG.hs" #-}
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
                  ({-# LINE 37 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   CR.mkDbg info_
                   {-# LINE 971 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 40 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 976 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 25 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 981 "CCO/Core/AG.hs" #-}
                   )
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
                  ({-# LINE 27 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _hdIcrexpl ++ _tlIcrexpl
                   {-# LINE 1016 "CCO/Core/AG.hs" #-}
                   )
              _hdOstkoff =
                  ({-# LINE 61 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 1021 "CCO/Core/AG.hs" #-}
                   )
              _tlOstkoff =
                  ({-# LINE 61 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 1026 "CCO/Core/AG.hs" #-}
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
                  ({-# LINE 27 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   []
                   {-# LINE 1040 "CCO/Core/AG.hs" #-}
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
             ({-# LINE 15 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              CR.mkMod (mkHNm "Main") Nothing (length _bindsIcrbindl + 100) _bindsIcrbindl _mainIcrexp
              {-# LINE 1074 "CCO/Core/AG.hs" #-}
              )
         _bindsOstkoff =
             ({-# LINE 66 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              0
              {-# LINE 1079 "CCO/Core/AG.hs" #-}
              )
         _mainOstkoff =
             ({-# LINE 67 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              _bindsIstkoff
              {-# LINE 1084 "CCO/Core/AG.hs" #-}
              )
         ( _mainIcrexp,_mainIcrexpl) =
             main_ _mainOstkoff
         ( _bindsIcrbindl,_bindsIstkoff) =
             binds_ _bindsOstkoff
     in  ( _lhsOcrmod))
-- Ref ---------------------------------------------------------
data Ref = Glob (Int)
         | Loc (Int) (Int)
-- cata
sem_Ref :: Ref ->
           T_Ref
sem_Ref (Glob _offset) =
    (sem_Ref_Glob _offset)
sem_Ref (Loc _levdiff _offset) =
    (sem_Ref_Loc _levdiff _offset)
-- semantic domain
type T_Ref = ( ([CR.RRef]))
data Inh_Ref = Inh_Ref {}
data Syn_Ref = Syn_Ref {crrefl_Syn_Ref :: ([CR.RRef])}
wrap_Ref :: T_Ref ->
            Inh_Ref ->
            Syn_Ref
wrap_Ref sem (Inh_Ref) =
    (let ( _lhsOcrrefl) = sem
     in  (Syn_Ref _lhsOcrrefl))
sem_Ref_Glob :: Int ->
                T_Ref
sem_Ref_Glob offset_ =
    (let _lhsOcrrefl :: ([CR.RRef])
         _lhsOcrrefl =
             ({-# LINE 52 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              [CR.mkModRef offset_]
              {-# LINE 1118 "CCO/Core/AG.hs" #-}
              )
     in  ( _lhsOcrrefl))
sem_Ref_Loc :: Int ->
               Int ->
               T_Ref
sem_Ref_Loc levdiff_ offset_ =
    (let _lhsOcrrefl :: ([CR.RRef])
         _lhsOcrrefl =
             ({-# LINE 53 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              [CR.mkLocDifRef levdiff_ offset_]
              {-# LINE 1129 "CCO/Core/AG.hs" #-}
              )
     in  ( _lhsOcrrefl))
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
         _hdIcrrefl :: ([CR.RRef])
         _tlIcrrefl :: ([CR.RRef])
         _lhsOcrrefl =
             ({-# LINE 49 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              _hdIcrrefl ++ _tlIcrrefl
              {-# LINE 1159 "CCO/Core/AG.hs" #-}
              )
         ( _hdIcrrefl) =
             hd_
         ( _tlIcrrefl) =
             tl_
     in  ( _lhsOcrrefl))
sem_RefL_Nil :: T_RefL
sem_RefL_Nil =
    (let _lhsOcrrefl :: ([CR.RRef])
         _lhsOcrrefl =
             ({-# LINE 49 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              []
              {-# LINE 1172 "CCO/Core/AG.hs" #-}
              )
     in  ( _lhsOcrrefl))
-- SExp --------------------------------------------------------
data SExp = Int (Int)
          | Var (Ref)
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
             ({-# LINE 21 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              [CR.mkInt' i_]
              {-# LINE 1202 "CCO/Core/AG.hs" #-}
              )
     in  ( _lhsOcrsexpl))
sem_SExp_Var :: T_Ref ->
                T_SExp
sem_SExp_Var x_ =
    (let _lhsOcrsexpl :: ([CR.SExp])
         _xIcrrefl :: ([CR.RRef])
         _lhsOcrsexpl =
             ({-# LINE 22 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              [CR.mkVar' $ head _xIcrrefl]
              {-# LINE 1213 "CCO/Core/AG.hs" #-}
              )
         ( _xIcrrefl) =
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
             ({-# LINE 18 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              _hdIcrsexpl ++ _tlIcrsexpl
              {-# LINE 1245 "CCO/Core/AG.hs" #-}
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
             ({-# LINE 18 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              []
              {-# LINE 1258 "CCO/Core/AG.hs" #-}
              )
     in  ( _lhsOcrsexpl))