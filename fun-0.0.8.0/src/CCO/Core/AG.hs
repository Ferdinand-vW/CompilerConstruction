

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

{-# LINE 2 "CCO\\Core\\..\\Ag\\HM.ag" #-}

import CCO.SourcePos
import CCO.Tree                   (Tree (fromTree, toTree))
import qualified CCO.Tree as T    (ATerm (App))
import CCO.Tree.Parser            (parseTree, app, arg)
import Control.Applicative        (Applicative ((<*>)), (<$>))
{-# LINE 30 "CCO/Core/AG.hs" #-}

{-# LINE 2 "CCO\\Core\\..\\AG\\Core.ag" #-}

import qualified UHC.Light.Compiler.CoreRun.API as CR
{-# LINE 35 "CCO/Core/AG.hs" #-}
{-# LINE 6 "CCO\\Core\\AG\\Hm2Cr.ag" #-}

type Map a b = [(a,b)]
{-# LINE 39 "CCO/Core/AG.hs" #-}

{-# LINE 39 "CCO\\Core\\AG\\Hm2Cr.ag" #-}


tmToTm_ :: Tm -> Tm_
tmToTm_ (Tm pos t) = t

fromExp :: Exp -> SExp
fromExp (SExp s) = s 

tmToSExp :: Map String Int -> Tm_ -> SExp
tmToSExp map (HNat i) = Int i
tmToSExp map (HVar x) = Var (Loc 0 (lookup' map x))

getOffSet :: Map String Int -> Int
getOffSet xs = length xs

insert :: Map String Int -> String -> Map String Int
insert xs s = (s, getOffSet xs) : xs

lookup' :: Map String Int -> String -> Int
lookup' [] _ = error "Was not able to find it"
lookup' ((x,y):xy) s
    | x == s = y
    | otherwise = lookup' xy s

{-# LINE 66 "CCO/Core/AG.hs" #-}

{-# LINE 11 "CCO\\Core\\..\\Ag\\HM.ag" #-}

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

{-# LINE 88 "CCO/Core/AG.hs" #-}

{-# LINE 36 "CCO\\Core\\..\\Ag\\HM.ag" #-}

type Var = String    -- ^ Type of variables.
{-# LINE 93 "CCO/Core/AG.hs" #-}

{-# LINE 28 "CCO\\Core\\AG.ag" #-}

crprinter :: Component Mod String
crprinter = component $ \mod -> do
  let crmod = crmod_Syn_Mod (wrap_Mod (sem_Mod mod) Inh_Mod)
  return $ show $ printModule defaultEHCOpts crmod
{-# LINE 101 "CCO/Core/AG.hs" #-}
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
                   {-# LINE 134 "CCO/Core/AG.hs" #-}
                   )
              _xexpOstkoff =
                  ({-# LINE 70 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   0
                   {-# LINE 139 "CCO/Core/AG.hs" #-}
                   )
              _lhsOstkoff =
                  ({-# LINE 71 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff + 1
                   {-# LINE 144 "CCO/Core/AG.hs" #-}
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
                   {-# LINE 185 "CCO/Core/AG.hs" #-}
                   )
              _lhsOstkoff =
                  ({-# LINE 63 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _tlIstkoff
                   {-# LINE 190 "CCO/Core/AG.hs" #-}
                   )
              _hdOstkoff =
                  ({-# LINE 63 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 195 "CCO/Core/AG.hs" #-}
                   )
              _tlOstkoff =
                  ({-# LINE 63 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _hdIstkoff
                   {-# LINE 200 "CCO/Core/AG.hs" #-}
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
                   {-# LINE 215 "CCO/Core/AG.hs" #-}
                   )
              _lhsOstkoff =
                  ({-# LINE 63 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 220 "CCO/Core/AG.hs" #-}
                   )
          in  ( _lhsOcrbindl,_lhsOstkoff)))
-- Core --------------------------------------------------------
data Core = Core (Tm)
-- cata
sem_Core :: Core ->
            T_Core
sem_Core (Core _tm) =
    (sem_Core_Core (sem_Tm _tm))
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
sem_Core_Core :: T_Tm ->
                 T_Core
sem_Core_Core tm_ =
    (let _lhsOcore :: Mod
         _tmOenv :: (Map String Int)
         _tmIt :: Exp
         _lhsOcore =
             ({-# LINE 18 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
              Mod _tmIt []
              {-# LINE 249 "CCO/Core/AG.hs" #-}
              )
         _tmOenv =
             ({-# LINE 19 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
              []
              {-# LINE 254 "CCO/Core/AG.hs" #-}
              )
         ( _tmIt) =
             tm_ _tmOenv
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
                   {-# LINE 308 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 40 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 313 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 25 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 318 "CCO/Core/AG.hs" #-}
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
                   {-# LINE 337 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 40 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 342 "CCO/Core/AG.hs" #-}
                   )
              _bodyOstkoff =
                  ({-# LINE 74 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   length _argsIcrrefl
                   {-# LINE 347 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 25 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 352 "CCO/Core/AG.hs" #-}
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
                   {-# LINE 373 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 40 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 378 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 25 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 383 "CCO/Core/AG.hs" #-}
                   )
              _funcOstkoff =
                  ({-# LINE 61 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 388 "CCO/Core/AG.hs" #-}
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
                   {-# LINE 406 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 40 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 411 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 25 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 416 "CCO/Core/AG.hs" #-}
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
                   {-# LINE 432 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 40 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 437 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 25 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 442 "CCO/Core/AG.hs" #-}
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
              _sexpIcrsexpl :: ([CR.SExp])
              _altsIcrexpl :: ([CR.Exp])
              _crexp =
                  ({-# LINE 35 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   CR.mkCase (head _sexpIcrsexpl) _altsIcrexpl
                   {-# LINE 459 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 40 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 464 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 25 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 469 "CCO/Core/AG.hs" #-}
                   )
              ( _sexpIcrsexpl) =
                  sexp_
              ( _altsIcrexpl) =
                  alts_
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
                   {-# LINE 492 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 40 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 497 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 25 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 502 "CCO/Core/AG.hs" #-}
                   )
              _bindOstkoff =
                  ({-# LINE 63 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _lhsIstkoff
                   {-# LINE 507 "CCO/Core/AG.hs" #-}
                   )
              _bodyOstkoff =
                  ({-# LINE 61 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _bindIstkoff
                   {-# LINE 512 "CCO/Core/AG.hs" #-}
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
                   {-# LINE 528 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexpl =
                  ({-# LINE 40 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   [_crexp]
                   {-# LINE 533 "CCO/Core/AG.hs" #-}
                   )
              _lhsOcrexp =
                  ({-# LINE 25 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
                   _crexp
                   {-# LINE 538 "CCO/Core/AG.hs" #-}
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
type T_ExpL = ( ([CR.Exp]))
data Inh_ExpL = Inh_ExpL {}
data Syn_ExpL = Syn_ExpL {crexpl_Syn_ExpL :: ([CR.Exp])}
wrap_ExpL :: T_ExpL ->
             Inh_ExpL ->
             Syn_ExpL
wrap_ExpL sem (Inh_ExpL) =
    (let ( _lhsOcrexpl) = sem
     in  (Syn_ExpL _lhsOcrexpl))
sem_ExpL_Cons :: T_Exp ->
                 T_ExpL ->
                 T_ExpL
sem_ExpL_Cons hd_ tl_ =
    (let _lhsOcrexpl :: ([CR.Exp])
         _hdOstkoff :: Int
         _hdIcrexp :: (CR.Exp)
         _hdIcrexpl :: ([CR.Exp])
         _tlIcrexpl :: ([CR.Exp])
         _lhsOcrexpl =
             ({-# LINE 27 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              _hdIcrexpl ++ _tlIcrexpl
              {-# LINE 570 "CCO/Core/AG.hs" #-}
              )
         _hdOstkoff =
             ({-# LINE 61 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              error "missing rule: ExpL.Cons.hd.stkoff"
              {-# LINE 575 "CCO/Core/AG.hs" #-}
              )
         ( _hdIcrexp,_hdIcrexpl) =
             hd_ _hdOstkoff
         ( _tlIcrexpl) =
             tl_
     in  ( _lhsOcrexpl))
sem_ExpL_Nil :: T_ExpL
sem_ExpL_Nil =
    (let _lhsOcrexpl :: ([CR.Exp])
         _lhsOcrexpl =
             ({-# LINE 27 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              []
              {-# LINE 588 "CCO/Core/AG.hs" #-}
              )
     in  ( _lhsOcrexpl))
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
              {-# LINE 622 "CCO/Core/AG.hs" #-}
              )
         _bindsOstkoff =
             ({-# LINE 66 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              0
              {-# LINE 627 "CCO/Core/AG.hs" #-}
              )
         _mainOstkoff =
             ({-# LINE 67 "CCO\\Core\\AG\\ToCoreRun.ag" #-}
              _bindsIstkoff
              {-# LINE 632 "CCO/Core/AG.hs" #-}
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
              [CR.mkGlobRef 0 offset_]
              {-# LINE 666 "CCO/Core/AG.hs" #-}
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
              {-# LINE 677 "CCO/Core/AG.hs" #-}
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
              {-# LINE 707 "CCO/Core/AG.hs" #-}
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
              {-# LINE 720 "CCO/Core/AG.hs" #-}
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
              {-# LINE 750 "CCO/Core/AG.hs" #-}
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
              {-# LINE 761 "CCO/Core/AG.hs" #-}
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
              {-# LINE 793 "CCO/Core/AG.hs" #-}
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
              {-# LINE 806 "CCO/Core/AG.hs" #-}
              )
     in  ( _lhsOcrsexpl))
-- Tm ----------------------------------------------------------
data Tm = Tm (SourcePos) (Tm_)
-- cata
sem_Tm :: Tm ->
          T_Tm
sem_Tm (Tm _pos _t) =
    (sem_Tm_Tm _pos (sem_Tm_ _t))
-- semantic domain
type T_Tm = (Map String Int) ->
            ( Exp)
data Inh_Tm = Inh_Tm {env_Inh_Tm :: (Map String Int)}
data Syn_Tm = Syn_Tm {t_Syn_Tm :: Exp}
wrap_Tm :: T_Tm ->
           Inh_Tm ->
           Syn_Tm
wrap_Tm sem (Inh_Tm _lhsIenv) =
    (let ( _lhsOt) = sem _lhsIenv
     in  (Syn_Tm _lhsOt))
sem_Tm_Tm :: SourcePos ->
             T_Tm_ ->
             T_Tm
sem_Tm_Tm pos_ t_ =
    (\ _lhsIenv ->
         (let _lhsOt :: Exp
              _tOenv :: (Map String Int)
              _tIt :: Exp
              _lhsOt =
                  ({-# LINE 23 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _tIt
                   {-# LINE 838 "CCO/Core/AG.hs" #-}
                   )
              _tOenv =
                  ({-# LINE 24 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIenv
                   {-# LINE 843 "CCO/Core/AG.hs" #-}
                   )
              ( _tIt) =
                  t_ _tOenv
          in  ( _lhsOt)))
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
type T_Tm_ = (Map String Int) ->
             ( Exp)
data Inh_Tm_ = Inh_Tm_ {env_Inh_Tm_ :: (Map String Int)}
data Syn_Tm_ = Syn_Tm_ {t_Syn_Tm_ :: Exp}
wrap_Tm_ :: T_Tm_ ->
            Inh_Tm_ ->
            Syn_Tm_
wrap_Tm_ sem (Inh_Tm_ _lhsIenv) =
    (let ( _lhsOt) = sem _lhsIenv
     in  (Syn_Tm_ _lhsOt))
sem_Tm__HNat :: Int ->
                T_Tm_
sem_Tm__HNat i_ =
    (\ _lhsIenv ->
         (let _lhsOt :: Exp
              _lhsOt =
                  ({-# LINE 27 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   SExp (Int i_)
                   {-# LINE 886 "CCO/Core/AG.hs" #-}
                   )
          in  ( _lhsOt)))
sem_Tm__HVar :: Var ->
                T_Tm_
sem_Tm__HVar x_ =
    (\ _lhsIenv ->
         (let _lhsOt :: Exp
              _lhsOt =
                  ({-# LINE 28 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   SExp $ tmToSExp _lhsIenv (HVar x_)
                   {-# LINE 897 "CCO/Core/AG.hs" #-}
                   )
          in  ( _lhsOt)))
sem_Tm__HLam :: Var ->
                T_Tm ->
                T_Tm_
sem_Tm__HLam x_ t1_ =
    (\ _lhsIenv ->
         (let _lhsOt :: Exp
              _t1Oenv :: (Map String Int)
              _t1It :: Exp
              _lhsOt =
                  ({-# LINE 29 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   SExp (Int 2)
                   {-# LINE 911 "CCO/Core/AG.hs" #-}
                   )
              _t1Oenv =
                  ({-# LINE 30 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   insert _lhsIenv x_
                   {-# LINE 916 "CCO/Core/AG.hs" #-}
                   )
              ( _t1It) =
                  t1_ _t1Oenv
          in  ( _lhsOt)))
sem_Tm__HApp :: T_Tm ->
                T_Tm ->
                T_Tm_
sem_Tm__HApp t1_ t2_ =
    (\ _lhsIenv ->
         (let _lhsOt :: Exp
              _t1Oenv :: (Map String Int)
              _t2Oenv :: (Map String Int)
              _t1It :: Exp
              _t2It :: Exp
              _lhsOt =
                  ({-# LINE 31 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   App (_t1It) [fromExp _t2It]
                   {-# LINE 934 "CCO/Core/AG.hs" #-}
                   )
              _t1Oenv =
                  ({-# LINE 32 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIenv
                   {-# LINE 939 "CCO/Core/AG.hs" #-}
                   )
              _t2Oenv =
                  ({-# LINE 33 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIenv
                   {-# LINE 944 "CCO/Core/AG.hs" #-}
                   )
              ( _t1It) =
                  t1_ _t1Oenv
              ( _t2It) =
                  t2_ _t2Oenv
          in  ( _lhsOt)))
sem_Tm__HLet :: Var ->
                T_Tm ->
                T_Tm ->
                T_Tm_
sem_Tm__HLet x_ t1_ t2_ =
    (\ _lhsIenv ->
         (let _lhsOt :: Exp
              _t1Oenv :: (Map String Int)
              _t2Oenv :: (Map String Int)
              _t1It :: Exp
              _t2It :: Exp
              _lhsOt =
                  ({-# LINE 34 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   Let (Bind (Glob 5) (SExp (Int 4))) _t2It
                   {-# LINE 965 "CCO/Core/AG.hs" #-}
                   )
              _t1Oenv =
                  ({-# LINE 35 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   _lhsIenv
                   {-# LINE 970 "CCO/Core/AG.hs" #-}
                   )
              _t2Oenv =
                  ({-# LINE 36 "CCO\\Core\\AG\\Hm2Cr.ag" #-}
                   insert _lhsIenv x_
                   {-# LINE 975 "CCO/Core/AG.hs" #-}
                   )
              ( _t1It) =
                  t1_ _t1Oenv
              ( _t2It) =
                  t2_ _t2Oenv
          in  ( _lhsOt)))