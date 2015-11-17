

-- UUAGC 0.9.52.1 (Typing.ag)

{-# LINE 2 "Typing.ag" #-}

import CCO.Printing
import CCO.SourcePos        (SourcePos)
import CCO.Tree             (ATerm (App), Tree (fromTree, toTree))
import CCO.Tree.Parser      (parseTree, app)
import Control.Applicative  (Applicative (pure))
import CCO.Diag (Diag(...), Diag (...))
{-# LINE 14 "Typing.hs" #-}
{-# LINE 14 "Typing.ag" #-}


data Ty = Prog Ident 
        | Interp Ident Ident 
        | Comp Ident Ident Ident 
        | PlatF Ident 
        | Runnable 
        | Framework 
        | Err 
        | CompileTy Ident Ident 
        | ExecInter Ident Ident Ident Ident 
        | ExecPlat Ident Ident
         deriving (Eq, Show)

instance Tree Ty where
    fromTree Prog x = App "Prog" [x]
    fromTree Interp x y = App "Interp" [x,y]
    fromTree Comp x y z = App "Comp" [x,y,z]
    fromTree PlatF x = App "PlatF" [x]
    fromTree Runnable = App "Runnable" []
    fromTree Framework = App "Framework" []
    fromTree Err = App "Err" []
    fromTree CompileTy x y = App "CompileTy" [x,y]
    fromTree ExecInter w x y z = App "ExecInter" [w,x,y,z]
    fromTree ExecPlat x y = App "ExecPlat" [x,y]

    toTree = parseTree [ app "Prog" (Prog <$> arg),
                         app "Interp" (Interp <$> arg <*> arg),
                         app "Comp" (Comp <$> arg <*> arg <*> arg),
                         app "PlatF" (PlatF <$> arg),
                         app "Runnable" (pure Runnable),
                         app "Framework" (pure Framework),
                         app "Err" (pure Err),
                         app "CompileTy" (CompileTy <$> arg <*> arg),
                         app "ExecInter" (ExecInter <$> arg <*> arg <*> arg <*> arg),
                         app "ExecPlat" (ExecPlat <$> arg <*> arg),
                         app ""]



match :: Ty -> Ty -> Bool
match _ Err = False
match Err _ = False
match Runnable Prog = True
match Runnable Interp = True
match Runnable Comp = True
match Prog Runnable = True
match Interp Runnable = True
match Comp Runnable = True
match Framework Interp = True
match Framework PlatF = True
match Interp Framework = True
match PlatF Framework = True
match ty1 ty2 = ty1 == ty2


typeOfExecute :: Ty -> Ty -> Ty
typeOfExecute ty1 ty2
    | not (match ty1 (PlatF)) && (match ty2 PlatF || match ty2 Interp) = ty1
    | otherwise = Err


typeOfCompile :: Ty -> Ty -> Ty
typeOfCompile ty1 ty2
    | not (match ty1 PlatF) && (match ty2 Comp) = ty
    | otherwise = Err

{-# LINE 83 "Typing.hs" #-}

{-# LINE 90 "Typing.ag" #-}

data TyErr = TyErr SourcePos String Ty Ty

instance Printable TyErr where
    pp = ppTyErr
{-# LINE 91 "Typing.hs" #-}

{-# LINE 98 "Typing.ag" #-}

-- | Pretty prints a type error message.
ppTyErr :: TyErr -> Doc
ppTyErr (TyErr pos descr expected inferred)
  = above [ppHeader, text " ", ppExpected, ppInferred]
  where
    ppHeader   = wrapped $
                 describeSourcePos pos ++ ": Type error: " ++ descr ++ "."
    ppExpected = text "? expected : " >|< showable expected
    ppInferred = text "? inferred : " >|< showable inferred
{-# LINE 104 "Typing.hs" #-}