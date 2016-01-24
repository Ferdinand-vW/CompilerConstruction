module Monotone.EmbellishedConstantPropagation
(
ecp,
ContextLattice(..),
Context(..)
)
where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe

import Administration
import Monotone.Analysis
import Monotone.MonotoneFramework
import Monotone.ConstantPropagation

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

--Add any new contexts from the second context lattice to the first one.
--For any same contexts join the lattices as is done normally in ConstantPropagation
joinContext :: ContextLattice Int -> ContextLattice Int -> ContextLattice Int
joinContext cl1 cl2 = M.unionWith joinOp cl1 cl2

--For two context lattices to be consistent they must have the same contexts
--If do they, then also the lattices for each context must be consistent.
--Again we can reuse the way it is done in Constant Propagation
contextMeet :: ContextLattice Int -> ContextLattice Int -> Bool
contextMeet cl1 cl2
      | M.keys cl1 /= M.keys cl2 = False
      | otherwise = foldr (\(l1,l2) y -> setMeet l1 l2 && y) True (zip (M.elems cl1) (M.elems cl2))

--The embedded transfer function requires to be binary in certain cases, which is why we pass a
--Map Label (ContextLattice Int), which is defined and build in the Analysis.
--We have two specific cases for a CallEntry and CallExit. CallExit is binary.
--For all other cases we reuse the transferfunction of ConstantPropagation for each Context
embTransferFunction :: Int -> InterFlow -> M.Map Label (ContextLattice Int) -> Block -> Label -> ContextLattice Int -> ContextLattice Int
embTransferFunction k _ _ b@(B_CallEntry _ _ _ _ _) l clat      = callTransfer k b l clat
embTransferFunction k iflow arr b@(B_CallExit _ _ _ _ _) l clat = returnTransfer k arr iflow b l clat
embTransferFunction k _ _ block l clat                          = M.map (transferFunction M.empty block l) clat

--Handles the transfer for a call entry. This function does the following:
--1. Add the Call Label to each context with a max size of k
--2. Add the empty context
--3. Find a possible duplicate context. There can be at most one
--4. Find all duplicate contexts
--5. Apply transfer function on these contexts
--6. Merge the duplicate contexts
--7. Apply transfer function on non duplicate contexts
--8. Rebuild the ContextLattice
callTransfer :: Int -> Block -> Label -> ContextLattice Int -> ContextLattice Int
callTransfer k (B_CallEntry name prms _ args out) l clat = 
    let clatList = M.toList clat
        emptyContext = ([],M.empty)
        contextList = if k <= 0
                        then clatList
                        else emptyContext : (map addLabel clatList) --(2),(1)
        mdup = findDupContext contextList --(3)

    in case mdup of
        Nothing -> M.fromList $ map (\(cxt,latt) -> (cxt, transferCall latt prms args out)) contextList --(8),(7)
        Just dup -> let (dups,ndups) = toMergeContexts dup contextList ([],[]) --(4)
                        mergedDups = mergeContexts dups --(5,6)
                    in M.singleton (fst $ head dups) mergedDups 
                      `M.union`  --(8)
                       M.fromList (map (\(cxt,latt) -> (cxt, transferCall latt prms args out)) ndups) --(7)
    where addLabel :: ([Int],Lattice Int) -> ([Int],Lattice Int)
          addLabel (xs,latt) = (take k (l:xs), latt) 
          findDupContext :: Eq a => [(a,b)] -> Maybe a --Finds a single duplicate Context
          findDupContext [] = Nothing --For each context see if the rest of the contexts contains a duplicate
          findDupContext (x:xs) = case foldr (\y found -> if fst x == fst y
                                                    then Just x
                                                    else found) Nothing xs of
                                        Nothing -> findDupContext xs
                                        Just dup -> Just $ fst dup
           --Partition the duplicate contexts from the non-duplicate contexts by comparing the context, found
           --using findDupContext, to each context in the to list converted ContextLattice
          toMergeContexts :: Eq a => a -> [(a,b)] -> ([(a,b)],[(a,b)]) -> ([(a,b)],[(a,b)])
          toMergeContexts _ [] acc = acc
          toMergeContexts ctx (x:xs) (dups, ndups)
                            | ctx == fst x = toMergeContexts ctx xs (x : dups,     ndups)
                            | otherwise    = toMergeContexts ctx xs (    dups, x : ndups)
          --Merges duplicate contexts by first applying the transfer function to each lattice of each context
          --and then merging the lattices using joinOp
          mergeContexts :: [(Context,Lattice Int)] -> Lattice Int
          mergeContexts [] = M.empty
          mergeContexts ((_,latt):xs) = transferCall latt prms args out `joinOp` mergeContexts xs

getCallLabel :: InterFlow -> Label -> Maybe Label
getCallLabel [] _ = Nothing
getCallLabel ((c,_,_,r):xs) l
                | l == r = Just c
                | otherwise = getCallLabel xs l

transferCall :: Lattice Int -> Exprs -> [Var] -> Var -> Lattice Int
transferCall latt exprs args out
    | M.null latt = latt
    | otherwise =  
        M.singleton out Top `M.union`
        foldr (\(val,arg) y -> if M.member arg y
                                    then M.adjust (\_ -> val) arg y
                                    else M.insert arg val y) latt (zip lvals args)
    where lvals = map (\(I expr) -> parseExp expr latt) exprs

--Binary tranfer function for returning from a procedure. This function does the following:
--1. Find the call label corresponding to this return
--2. Find the context corresponding to that call
--3. Remove contexts from the return context that do not have the call label on top of the stack
--4. For each other context in the return context, remove the call label from their stacks
--5. For every context that appears in both the call context and the return context, merge their lattices
--6. Remove any context that does not appear on both the call context and return context
returnTransfer :: Int -> M.Map Label (ContextLattice Int) -> InterFlow -> Block -> Label -> ContextLattice Int -> ContextLattice Int
returnTransfer k arr iflow (B_CallExit _ _ cout pargs pout) l retCtx =
    let callLabel = fromJust $ getCallLabel iflow l --(1)
        callCtx = fromJust $ M.lookup callLabel arr --(2)
        retCtxList = removeContexts retCtx callLabel --(3),(4)
                                                     --Remove any contexts that do not start with the CallLabel
                                                     --At the same time for any contexts that do start with the CallLabel
                                                     --throw the CallLabel away from the context
    in M.intersectionWith (\x y ->    --(6)
          mergeLattice x y) --(5)
          callCtx (M.fromList retCtxList)--Only store contexts that reside in both the call
                                         --and return context. At the same time merge the lattices
                                         --of two the same contexts, while taking care of unshadowing
                                         --and updating the return value of the procedure
  where 
    --Removes contexts who's head does not equal the call label. However, if k = 0, then we don't do anything
    removeContexts ctx l = if k <= 0
                            then M.toList ctx
                            else ([], M.empty) : [(tail x, latt) | (x,latt) <- M.toList ctx , length x > 0, head x == l]
    --merge two lattices
    --If the variable is the variable that we have to store the result of the procedure in, then lookup the value of
    --the result value in the return lattice and store that.
    --else if the variable was used in the procedure as a parameter, then use the value of the call lattice
    --otherwise just use the value in the return lattice. These variables are global and may have been changed in the procedure
    mergeLattice callLat retLat
          | M.null callLat = M.empty
          | otherwise = M.mapWithKey 
                (\k retVal -> let callVal = fromJust $ M.lookup k callLat
                              in if k == cout --Variable to store the result of the procedure in
                                    then fromJust $ M.lookup pout retLat --retVal
                                    else if k `L.elem` (pout : pargs) --If parameter of procedure, then use old value
                                      then callVal
                                      else retVal) retLat