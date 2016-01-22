module EmbellishedConstantPropagation
(
ecp
)
where

import qualified Data.Map as M

import Analysis
import MonotoneFramework
import Administration
import ConstantPropagation

type Context = [Label]
type ContextLattice a = M.Map Context (Lattice a)

ecp :: Int -> ProgramInfo -> IO (Analysis (ContextLattice Int))
ecp k p = 
        let join = undefined
            lm = undefined
            tfunc = embTransferFunction k
            fl = Administration.flow p
            ifl = Administration.interflow p
            extrL = Administration.init p
            extrV = M.singleton [] (foldr (\x y -> M.insert x Top y) M.empty (vars p))
            mframe = MonotoneFramework join M.empty lm tfunc fl ifl extrL extrV
        in  analyse mframe (blocks p)


embTransferFunction :: Int -> Block -> Label -> ContextLattice Int -> ContextLattice Int
embTransferFunction k b@(B_CallEntry name prms args out) l clat = updateCallStack k b l clat
embTransferFunction k block l clat = M.map (transferFunction block) clat

updateCallStack :: Int -> Block -> Label -> ContextLattice Int -> ContextLattice Int
updateCallStack k block l clat = 
    let clatList = M.toList clat
        newcontext = ([l],M.empty)
        contextList = newcontext : map addLabel clatList
        mdup = findDupContext contextList

    in case mdup of
        Nothing -> M.fromList contextList
        Just dup -> let (dups,ndups) = toMergeContexts dup contextList ([],[])
                        mergedDups = mergeContexts dups
                    in M.singleton (fst $ head dups) mergedDups `M.union` M.fromList (map (\(cxt,latt) -> (cxt, transferFunction block latt)) ndups)
    where addLabel :: ([Int],Lattice Int) -> ([Int],Lattice Int)
          addLabel (x,latt) = case x of
                                [] -> (x,M.empty)
                                xs -> (take k (l:xs),latt) 
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




--We do not care what the previous value was here so we forget it
--Just parse the expression, obviously only works for IAssign right now
transferFunction :: Block -> Lattice Int -> Lattice Int
transferFunction (B_IAssign var expr) st = M.adjust (\_ -> parseExp expr st) var st
transferFunction (B_CallEntry name prms args out) st = transferCall st prms args out
transferFunction (B_ProcEntry ) st = undefined
transferFunction (B_ProcExit) st = undefined
transferFunction (B_CallExit name out) st = undefined
transferFunction _ st = st


transferCall :: Lattice Int -> Exprs -> [Var] -> Var -> Lattice Int
transferCall latt exprs args out = M.singleton out Top `M.union`
    foldr (\(val,arg) y -> if M.member arg y
                                then M.adjust (\_ -> val) arg y
                                else M.insert arg val y) latt (zip lvals args)
    where lvals = map (\(I expr) -> parseExp expr latt) exprs