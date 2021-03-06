-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Core.Base
-- Copyright   :  (c) 2014 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  atze@uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- Simple wrapper for/around CoreRun from uhc-light
--
-------------------------------------------------------------------------------

module CCO.Core.Base (
    -- * Syntax
    Ref (Glob, Loc, Tag)                     
  , RefL
  , SExp (Var, Int)                     
  , SExpL
  , Exp (SExp, Lam, App, Let, Prim, Node, Case, Dbg)
  , ExpL
  , Bind (Bind)                         
  , BindL
  , Mod (Mod)
  , hm2cr                          
) where

import CCO.Feedback
import CCO.Core.AG
import CCO.Tree                   (Tree (fromTree, toTree))
import qualified CCO.Tree as T    (ATerm (App))
import CCO.Tree.Parser            (parseTree, app, arg)
import Control.Applicative        (Applicative ((<*>)), (<$>))

-------------------------------------------------------------------------------
-- Tree instances
-------------------------------------------------------------------------------

instance Tree Mod where
  fromTree (Mod mn bs) = T.App "Mod" [fromTree mn, fromTree bs]
  toTree = parseTree [app "Mod" (Mod <$> arg <*> arg)]

instance Tree Bind where
  fromTree (Bind x e) = T.App "Bind" [fromTree x, fromTree e]
  toTree = parseTree [app "Bind" (Bind <$> arg <*> arg)]

instance Tree SExp where
  fromTree (Int i)   = T.App "Int" [fromTree i]
  fromTree (Var r)   = T.App "Var" [fromTree r]

  toTree = parseTree [ app "Int" (Int <$> arg)
                     , app "Var" (Var <$> arg)
                     ]

instance Tree Ref where
  fromTree (Glob  o)  = T.App "Glob" [fromTree o]
  fromTree (Loc d o)  = T.App "Loc" [fromTree d, fromTree o]
  fromTree (Tag r)  = T.App "Tag" [fromTree r]
  fromTree (Field i r) = T.App "Field" [fromTree i,fromTree r]

  toTree = parseTree [ app "Glob" (Glob <$> arg)
                     , app "Loc"  (Loc  <$> arg <*> arg)
                     , app "Tag"  (Tag  <$> arg)
                     , app "Field" (Field <$> arg <*> arg)
                     ]

instance Tree Exp where
  fromTree (SExp se   )   = T.App "SExp" [fromTree se]
  fromTree (True_) = T.App "True_" []
  fromTree (False_) = T.App "False_" []
  fromTree (Cons t1 t2) = T.App "Cons" [fromTree t1, fromTree t2]
  fromTree (Nil) = T.App "Nil" []
  fromTree (Lam  as bd)   = T.App "Lam"  [fromTree as, fromTree bd]
  fromTree (App  fn as)   = T.App "App"  [fromTree fn, fromTree as]
  fromTree (Prim fn as)   = T.App "Prim" [fromTree fn, fromTree as]
  fromTree (Node tg as)   = T.App "Node" [fromTree tg, fromTree as]
  fromTree (Case se as)   = T.App "Case" [fromTree se, fromTree as]
  fromTree (Let  bn bd)   = T.App "Let"  [fromTree bn, fromTree bd]
  fromTree (Dbg  i    )   = T.App "Dbg"  [fromTree i]
  fromTree (Eval e)       =  T.App "Eval" [fromTree e]

  toTree = parseTree [ app "SExp" (SExp <$> arg        )
                     , app "Cons" (Cons <$> arg <*> arg)
                     , app "Nil" (pure Nil)
                     , app "True_" (pure True_)
                     , app "False_" (pure False_)
                     , app "Lam"  (Lam  <$> arg <*> arg)
                     , app "App"  (App  <$> arg <*> arg)
                     , app "Prim" (Prim <$> arg <*> arg)
                     , app "Node" (Node <$> arg <*> arg)
                     , app "Case" (Case <$> arg <*> arg)
                     , app "Let"  (Let  <$> arg <*> arg)
                     , app "Dbg"  (Dbg  <$> arg        )
                     , app "Eval" (Eval <$> arg)
                     ]


hm2cr :: ATm -> Feedback Mod
hm2cr tm = return $ core_Syn_Core $ wrap_Core (sem_Core (Core tm)) (Inh_Core)