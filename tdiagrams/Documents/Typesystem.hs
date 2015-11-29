
---------------------------------------------------------------------------(ty-prog)
Γ |- (Ty cons:Prog  source : (just (l: Ident): Maybe a)   target :Nothing   platform : Nothing)

---------------------------------------------------------------------------(ty-interp)
Γ |- (Ty cons:Interp  source : (Just (l: Ident))   target :Nothing    platform : (Just (m: Ident)))

---------------------------------------------------------------------------(ty-comp)
Γ |- (Ty cons:Comp  source : (Just (l: Ident))   target :(Just (t: Ident))) : Ty   platform : (Just (l: Ident)))

---------------------------------------------------------------------------(ty-platf)
Γ |- (Ty cons:PlatF  source : Nothing  target : Nothing    platform : Nothing) : Ty


---------------------------------------------------------------------------
r |- t1 : Ty t2 : Ty
---------------------------------------------------------------------------(ty-execute)
r |- t2 : Ty --Also not sure what to add here

---------------------------------------------------------------------------
r |- ty: Ty ty2: Ty
----------------------------------------------------------------------------(ty-compile)
Γ |- translate: Ty -- Do I need to add this? It does not matter what the result is, only the type matters...


----------------------[t-true]
r |- true : Bool

----------------------[t-false]
r |- false : Bool

--------------------------[t-MaybeJust]
r |- just x : Maybe a

---------------------------[t-MaybeNothing]
r |- nothing : Maybe a


---------------------------[t-ProgTyCons]
r |- prog : TyCons

---------------------------[t-InterpTyCons]
r |- interp : TyCons

---------------------------[t-CompTyCons]
r |- comp : TyCons

---------------------------[t-PlatfTyCons]
r |- platf : TyCons

---------------------------[t-ProgTyCons]
r |- prog : TyCons




Lemma checkRunnable-true
r |- match_prog_runnable V match-interp-runnable V match-comp-runnable V match-eq-t1-t2
-----------------------------------------------------------------------------------------------------------(checkrunnable-true)
                r |- true : Bool               


Lemma checkRunnable:
r |- match-interp-Framework V match-platf-framework V match-nq-t1-t2
-----------------------------------------------------------------------------------------------------------(checkrunnable-false)
                r |- false : Bool    


Lemma checkComp-true:
r |- match-eq-t1-t2
-----------------------------------------------------------------------------------------------------------(checkrunnable-true)
r |- true : Bool               


Lemma checkComp-false:
r |- match_prog_runnable V match-interp-runnable V match-comp-runnable V match-interp-Framework V match-platf-framework V match-nq-t1-t2
-----------------------------------------------------------------------------------------------------------(checkrunnable-false)
                r |- false : Bool    



Lemma checkFramework-true:
r |- match-platf-framewor V match-interp-framework V match-eq-t1-t2
-----------------------------------------------------------------------------------------------------------(checkrunnable-true)
                r |- true : Bool               


Lemma checkFramework-true:
match-prog-runnable V match-interp-runnable V match-comp-runnable V match-nq-t1-t2
-----------------------------------------------------------------------------------------------------------(checkrunnable-false)
                r |- false : Bool    



Lemma match-prog-runnable: 
r |- t1: Prog |- t2: Runnable    
------------------------(match-prog-runnable)
r |- true : Bool

Lemma match-interp-runnable: 
r |- t1: Interp |- t2: Runnable    
------------------------(match-interp-runnable)
r |- true : Bool

Lemma match-comp-runnable: 
r |- t1: Comp |- t2: Runnable    
------------------------(match-comp-runnable)
r |- true : Bool

Lemma match-interp-framework: 
r |- t1: Interp |- t2: Framework    
------------------------(match-interp-framework)
r |- true : Bool

Lemma match-platf-framework: 
r |- t1: PlatF |- t2: Framework    
------------------------(match-platf-framework)
r |- true : Bool

Lemma match-eq-t1-t2: 
r |- t1: TyCons |- t2: TyCons    
------------------------(match-eq-t1-t2)
r |- t1 == t2 : Bool -> r
------------------------
r |- true : Bool

Lemma match-nq-t1-t2: 
r |- t1: TyCons |- t2: TyCons    
------------------------(match-nq-t1-t2)
r |- t1 /= t2 : Bool -> r
------------------------
r |- false : Bool


Lemma matchInfo:
r |- matchInfo-eq V matchinfo-nothing V matchInfo-nothingLeft V matchInfo-nothingRight
--------------------------------------------------------------------------------------(matchinfo-true)
r |- True : Bool

Lemma matchInfo-eq:
--matchInfo :: Maybe Ident -> Maybe Ident -> Bool
r |- just i : Maybe Ident   r |- just j |- Maybe Ident
--------------------------------------------------
r |- i == j : Bool
--------------------------------------------------
r |- true : Bool


Lemma matchInfo-nq
--matchInfo :: Maybe Ident -> Maybe Ident -> Bool
r |- just i : Maybe Ident    r |- just j |- Maybe Ident
--------------------------------------------------
r |- i /= j : Bool
--------------------------------------------------
r |- false : Bool


Lemma matchInfo-nothing:
r |- nothing : Maybe Ident   r |- nothing : Maybe Ident
------------------------------------------------------(matchinfo-nothing)
r |- true : Bool


Lemma matchInfo-nothingLeft:
r |- nothing : Maybe Ident   r |- just j |- Maybe Ident
------------------------------------------------------(matcInfo-nothingLeft)
r |- true : Bool


Lemma matchInfo-nothingRight:
r |- just i : Maybe Ident   r |- just i |- Maybe Ident
------------------------------------------------------(matchInfo-nothingRight)
r |- true : Bool


Lemma translate-prog-comp
r |- (Ty prog: TyCons s1: Maybe Ident t1: Maybe Ident m1: Maybe Ident)   r |- (Ty comp: TyCons s2: Maybe Ident t2: Maybe Ident m2: Maybe Ident)
-------------------------------------------------------------------------------------------------------------------------------------------------(translate-prog-comp)
r |- Ty prog t2 t1 m1


Lemma translate-interp-comp
r |- (Ty interp: TyCons s1: Maybe Ident t1: Maybe Ident m1: Maybe Ident)   r |- (Ty comp: TyCons s2: Maybe Ident t2: Maybe Ident m2: Maybe Ident)
-------------------------------------------------------------------------------------------------------------------------------------------------(translate-interp-comp)
r |- Ty prog s1 t1 t2

Lemma translate-comp-comp
r |- (Ty comp: TyCons s1: Maybe Ident t1: Maybe Ident m1: Maybe Ident)   r |- (Ty comp: TyCons s2: Maybe Ident t2: Maybe Ident m2: Maybe Ident)
-------------------------------------------------------------------------------------------------------------------------------------------------(translate-comp-comp)
r |- Ty comp s1 t1 t2

--not sure if correct, but still need to add all the cases of the translate function... What can I do with the _

Lemma checkifmatch-prog-interp
r |- (Ty comp: TyCons s1: Maybe Ident t1: Maybe Ident m1: Maybe Ident)   r |- (Ty interp: TyCons s2: Maybe Ident t2: Maybe Ident m2: Maybe Ident)
-------------------------------------------------------------------------------------------------------------------(checkifmatch-prog-interp-correct)
r |- matchinfo-true




r |- pos: (Double, Double) (Ty comp: TyCons s1: Maybe Ident t1: Maybe Ident m1: Maybe Ident)   r |- (Ty interp: TyCons s2: Maybe Ident t2: Maybe Ident m2: Maybe Ident)
-------------------------------------------------------------------------------------------------------------------(checkifmatch-prog-interp-ill)
r |- matchinfo-false


match Prog Runnable       = True : Bool

Prog:TyCons     Runnable:TyCons
    match Prog Runnable: Bool          True: Bool
            match Prog Runnable = True : Bool


match Interp Runnable     = True

Interp:TyCons     Runnable:TyCons
    match Interp Runnable : Bool          True: Bool
            match Interp Runnable = True : Bool


match Comp Runnable       = True

Comp:TyCons     Runnable: TyCons
    match Comp Runnable : Bool          True: Bool
            match Comp Runnable = True : Bool


match Interp Framework    = True

Interp:TyCons           Framework:TyCons
    match Interp Framework : Bool          True: Bool
            match Interp Framework = True : Bool


match PlatF Framework     = True

PlatF: TyCons         Framework: TyCons
    match PlatF Framework : Bool          True: Bool
            match PlatF Framework = True : Bool


match ty1 ty2 = ty1 == ty2

ty1: TyCons     ty2: TyCons         True: Bool      False: Bool
            ty1 == ty2: Bool       ty1 /= ty2: Bool
            ty1 == t2 -> True      ty1 == t2 -> False
                match ty1 ty2 = ty1 == ty2: Bool



checkRunnable :: SourcePos -> TyCons -> [TyErr]
checkRunnable pos ty | match ty Runnable = []
                     | otherwise          = [TyErr pos nonExe (show Runnable) (show $ cons ty)]
    where
        nonExe = "Cannot execute a non-runnable"

                            True: Bool          False Bool
pos: SourcePos   ty: Ty      match ty Runnable: Bool




translate (Ty Prof s1 t1 m1) (Ty Comp s2 t2 m2) = Ty Prof t2 t1 m1 : Ty
Prog: TyCons    s1,t1,m1: Maybe Ident   Comp: TyCons    s2,t2,m2: Maybe Ident
    Ty Prog s1 t1 m1: Ty                     Ty Comp s2 t2 m2: Ty
                match (TyProg s1 t1 m1) (Ty Comp s2 t2 m2) = Ty Prog t2 t1 m1: Ty


translate (Ty Interp s1 t1 m1) (Ty Comp s2 t2 m2) = Ty Interp s1 t1 t2

Interp: TyCons    s1,t1,m1: Maybe Ident   Comp: TyCons    s2,t2m2: Maybe Ident
    Ty Interp s1 t1 m1: Ty                     Ty Comp s2 t2 m2: Ty
                match (TyProg s1 t1 m1) (Ty Comp s2 t2 m2) = Ty Interp t2 t1 m1: Ty



translate (Ty Comp s1 t1 m1) (Ty Comp s2 t2 m2) = Ty Comp s1 t1 t2

Comp: TyCons    s1,t1,m1: Maybe Ident   Comp: TyCons    s2,t2m2: Maybe Ident
    Ty Comp s1 t1 m1: Ty                     Ty Comp s2 t2 m2: Ty
                match (TyProg s1 t1 m1) (Ty Comp s2 t2 m2) = Ty Comp t2 t1 m1: Ty



translate tyinfo1 _ = tyinfo1?

Prog: TyCons    s1,t1,m1: Maybe Ident   Comp: TyCons    s2,t2m2: Maybe Ident
    TyProg s1 t1 m1: Ty                     Ty Comp s2 t2 m2: Ty
                match (TyProg s1 t1 m1) (Ty Comp s2 t2 m2) = Ty Prog t2 t1 m1: Ty 