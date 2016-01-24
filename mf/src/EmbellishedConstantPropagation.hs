module EmbellishedConstantPropagation
(
ecp,
ContextLattice(..),
Context(..)
)
where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe

import Analysis
import MonotoneFramework
import Administration
import ConstantPropagation

type Context = [Label]
type ContextLattice a = M.Map Context (Lattice a)

ecp :: Int -> ProgramInfo -> IO (Analysis (ContextLattice Int))
ecp k p = 
        let join = joinContext
            lm = contextMeet
            btm = M.singleton [] M.empty
            tfunc = embTransferFunction k ifl
            fl = Administration.flow p
            ifl = Administration.interflow p
            extrL = Administration.init p
            extrV = M.singleton [] (foldr (\x y -> M.insert x Top y) M.empty (vars p))
            mframe = MonotoneFramework join btm lm tfunc fl ifl extrL extrV
        in  analyse mframe (blocks p)

joinContext :: ContextLattice Int -> ContextLattice Int -> ContextLattice Int
joinContext cl1 cl2 = M.unionWith joinOp cl1 cl2

contextMeet :: ContextLattice Int -> ContextLattice Int -> Bool
contextMeet cl1 cl2
      | M.keys cl1 /= M.keys cl2 = False
      | otherwise = foldr (\(l1,l2) y -> setMeet l1 l2 && y) True (zip (M.elems cl1) (M.elems cl2))

embTransferFunction :: Int -> InterFlow -> M.Map Label (ContextLattice Int) -> Block -> Label -> ContextLattice Int -> ContextLattice Int
embTransferFunction k _ _ b@(B_CallEntry _ _ _ _ _) l clat = updateCallStack k b l clat
embTransferFunction k iflow arr b@(B_CallExit _ _ _ _ _) l clat = returnTransfer k arr iflow b l clat
embTransferFunction k _ _ block l clat = M.map (transferFunction block) clat

updateCallStack :: Int -> Block -> Label -> ContextLattice Int -> ContextLattice Int
updateCallStack k block l clat = 
    let clatList = M.toList clat
        emptyContext = ([],M.empty)
        contextList = if k <= 0
                        then clatList
                        else emptyContext : (map addLabel clatList)
        mdup = findDupContext contextList --filter (\(xs,latt) -> not $ M.null latt) 

    in case mdup of
        Nothing -> M.fromList $ map (\(cxt,latt) -> (cxt, transferFunction block latt)) contextList
        Just dup -> let (dups,ndups) = toMergeContexts dup contextList ([],[])
                        mergedDups = mergeContexts dups
                    in M.singleton (fst $ head dups) mergedDups `M.union` M.fromList (map (\(cxt,latt) -> (cxt, transferFunction block latt)) ndups)
    where addLabel :: ([Int],Lattice Int) -> ([Int],Lattice Int)
          addLabel (xs,latt) = (take k (l:xs), latt) 
          findDupContext :: Eq a => [(a,b)] -> Maybe a
          findDupContext [] = Nothing
          findDupContext (x:xs) = case foldr (\y found -> if fst x == fst y
                                                    then Just x
                                                    else found) Nothing xs of
                                        Nothing -> findDupContext xs
                                        Just dup -> Just $ fst dup
          toMergeContexts :: Eq a => a -> [(a,b)] -> ([(a,b)],[(a,b)]) -> ([(a,b)],[(a,b)])
          toMergeContexts _ [] acc = acc
          toMergeContexts ctx (x:xs) (dups, ndups) 
                            | ctx == fst x = toMergeContexts ctx xs (x : dups, ndups)
                            | otherwise = toMergeContexts ctx xs (dups, x : ndups)
          mergeContexts :: [(Context,Lattice Int)] -> Lattice Int
          mergeContexts [] = M.empty
          mergeContexts ((_,x):xs) = transferFunction block x `joinOp` mergeContexts xs

returnTransfer :: Int -> M.Map Label (ContextLattice Int) -> InterFlow -> Block -> Label -> ContextLattice Int -> ContextLattice Int
returnTransfer k arr iflow (B_CallExit _ _ cout pargs pout) l retCtx =
    let callLabel = case getCallLabel iflow l of
                      Nothing -> error "calllabel nothing"
                      Just cl -> cl
        callCtx = case M.lookup callLabel arr of
                      Nothing -> error "callCtx Nothing"
                      Just cctx -> cctx
        retCtxList = removeContexts retCtx callLabel --Remove any contexts that do not start with the CallLabel
                                                     --At the same time for any contexts that do start with the CallLabel
                                                     --throw the CallLabel away from the context
    in M.intersectionWith (\x y -> mergeLattice x y callCtx (M.fromList retCtxList)) callCtx (M.fromList retCtxList) --Only store contexts that reside in both the call
                                                                       --and return context. At the same time merge the lattices
                                                                       --of two the same contexts, while taking care of unshadowing
                                                                       --and updating the return value of the procedure
  where 
    removeContexts ctx l = if k <= 0
                            then M.toList ctx
                            else ([], M.empty) : [(tail x, latt) | (x,latt) <- M.toList ctx , length x > 0, head x == l]
    mergeLattice callLat retLat callctx retctx
          | M.null callLat = M.empty
          | otherwise = M.mapWithKey 
                (\k retVal -> let callVal = case M.lookup k callLat of
                                              Nothing -> error $ show callLat ++ "   " ++ show retLat
                                              Just val -> val
                              in if k == cout --Variable to store the result of the procedure in
                                    then fromJust $ M.lookup pout retLat --retVal
                                    else if k `L.elem` (pout : pargs) --If parameter of procedure, then use old value
                                      then callVal
                                      else retVal) retLat



getCallLabel :: InterFlow -> Label -> Maybe Label
getCallLabel [] _ = Nothing
getCallLabel ((c,_,_,r):xs) l
                | l == r = Just c
                | otherwise = getCallLabel xs l


--We do not care what the previous value was here so we forget it
--Just parse the expression, obviously only works for IAssign right now
transferFunction :: Block -> Lattice Int -> Lattice Int
transferFunction (B_IAssign var expr) st = M.adjust (\_ -> parseExp expr st) var st
transferFunction (B_CallEntry name prms _ args out) st = transferCall st prms args out
transferFunction (B_ProcEntry _ _ _) st = st
transferFunction (B_ProcExit) st = st
transferFunction (B_CallExit _ _ _ _ _) st = error "call exit error"
transferFunction _ st = st


transferCall :: Lattice Int -> Exprs -> [Var] -> Var -> Lattice Int
transferCall latt exprs args out
    | M.null latt = latt
    | otherwise =  
        M.singleton out Top `M.union`
        foldr (\(val,arg) y -> if M.member arg y
                                    then M.adjust (\_ -> val) arg y
                                    else M.insert arg val y) latt (zip lvals args)
    where lvals = map (\(I expr) -> parseExp expr latt) exprs