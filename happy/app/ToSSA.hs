module ToSSA where

{-
This file aims to transform blocks in CFG into SSA
We only consider ToCFG2, hence from data Node we only need to deal with Block, Test, and Empty
-}

{-
Problems:
1) x:=1;if (tt) then {x:=x+2} else {x:=x+1};skip
2) phi functions must have a mistake, bcs it is taking too much time to compute
-}

import ToCFG2
import Syntax
import Com

import Data.List

type VarTrack = [(Var, [Var])]
type JoinNodes = [([NodeId], NodeId)]

runSSA :: String -> IO()
runSSA s = do
  let c = testCom s
      (g, es, entry, exit, next) = toCFG c 0 
      (g', vart) = toSSAGraph g es [] 
  putStrLn (drawCFG g' es)
  
-- runGraph :: String -> Graph
-- runGraph s = let c = testCom s
--                  (g, es, entry, exit, next) = toCFG c 0
--                  join_nodes = endDiv es []              
--                  (g', vart) = toSSAGraph g [] join_nodes
--              in g'

edges1 = [(0,Uncond,1),(1,Uncond,2),(2,Uncond,3),(2,Uncond,4),(3,Uncond,5),(4,Uncond,5),(1,Uncond,6),(6,Uncond,7),(6,Uncond,8),(5,Uncond,9),(7,Uncond,9),(8,Uncond,9)] :: Edges
edges2 = [(0,Uncond,1),(1,Uncond,2),(1,Uncond,3),(1,Uncond,4),(2,Uncond,5),(2,Uncond,6),(3,Uncond,7),(4,Uncond,7),(5,Uncond,7),(6,Uncond,7),(1,Uncond,8),(8,Uncond,7)] :: Edges

--toSSA :: (Graph, Edges, [NodeId], [NodeId], NodeId) -> (Graph, Edges, [NodeId], [NodeId], NodeId)
toSSAGraph :: Graph -> Edges -> VarTrack -> (Graph, VarTrack)
toSSAGraph [] _ vart = ([], vart)
toSSAGraph (h:t) eds vart = let (g, vart') = toSSANode h vart 
                                (t', vart'') = toSSAGraph t eds vart' 
                            in (g:t', vart'')

toSSANode :: Node -> VarTrack -> (Node, VarTrack)
toSSANode (Empty n) vart = (Empty n, vart)
toSSANode (Test n bexp) vart = let (bexpSSA, vart') = toSSAListCFGNot bexp vart
                               in (Test n bexpSSA, vart')
toSSANode (Block n lcom) vart = let (lcomSSA, vart') = toSSAListCFGNot lcom vart
                                in (Block n lcomSSA, vart')
toSSANode _ _ = error "toSSANode: Invalid kind of Node"

toSSAListCFGNot :: [CFGNot] -> VarTrack -> ([CFGNot], VarTrack)
toSSAListCFGNot [] vt = ([],  vt)
toSSAListCFGNot [h] vt = let (c, vt') = toSSACFGNot h vt
                         in ([c], vt')
toSSAListCFGNot (h:t) vt = let (c, vt') = toSSACFGNot h vt
                               (t', vt'') = toSSAListCFGNot t vt'
                           in (c:t', vt'')

toSSACFGNot :: CFGNot -> VarTrack -> (CFGNot, VarTrack)
toSSACFGNot SSkip vart = (SSkip, vart)
toSSACFGNot (SAsg x ae) vart = if (null $ (filter (\(var, lvar) -> var==x)) vart) == False
                                  then let ae' = replaceVarAExp ae vart
                                           id = length $ snd $ head $ filter (\(var, lvar) -> var==x) vart
                                           newx = x ++ show(id)
                                           vart' = map (\(var, lvar) -> if var==x then (var, newx:lvar) else (var, lvar)) vart
                                       in (SAsg newx ae', vart')
                                  else let newx = x++show(0)
                                           vart' = (x,[newx]):vart
                                           ae' = replaceVarAExp ae vart'
                                       in (SAsg newx ae', vart')
toSSACFGNot (BE bexp) vart = let ssa_bexp = replaceVarBExp bexp vart
                             in (BE ssa_bexp, vart)


---Auxiliary functions---
replaceVarAExp :: AExp -> VarTrack -> AExp
replaceVarAExp (Var x) vart = let newx = head $ snd $ head $ filter (\(var, lvar) -> var==x) vart
                              in (Var newx)
replaceVarAExp (Num num) _ = Num num
replaceVarAExp (Plus ae1 ae2) vart = let ae1' = replaceVarAExp ae1 vart
                                         ae2' = replaceVarAExp ae2 vart
                                     in Plus ae1' ae2'
replaceVarAExp (Minus ae1 ae2) vart = let ae1' = replaceVarAExp ae1 vart
                                          ae2' = replaceVarAExp ae2 vart
                                      in Minus ae1' ae2'
replaceVarAExp (Mult ae1 ae2) vart = let ae1' = replaceVarAExp ae1 vart
                                         ae2' = replaceVarAExp ae2 vart
                                     in Mult ae1' ae2'
replaceVarAExp (Div ae1 ae2) vart = let ae1' = replaceVarAExp ae1 vart
                                        ae2' = replaceVarAExp ae2 vart
                                    in Div ae1' ae2'
replaceVarAExp (Negate ae) vart = let ae' = replaceVarAExp ae vart
                                  in Negate ae'


replaceVarBE :: CFGNot -> VarTrack -> [CFGNot]
replaceVarBE (BE b) vart = let b' = replaceVarBExp b vart
                           in [BE b']
replaceVarBE _ _ = error "replaceVarBE: not a Boolean expression"                              

replaceVarBExp :: BExp -> VarTrack -> BExp
replaceVarBExp BTrue _ = BTrue
replaceVarBExp BFalse _ = BFalse
replaceVarBExp (Not bexp) vart = let bexp' = replaceVarBExp bexp vart
                                 in Not bexp'
replaceVarBExp (And be1 be2) vart = let be1' = replaceVarBExp be1 vart
                                        be2' = replaceVarBExp be2 vart
                                    in And be1' be2'
replaceVarBExp (OrB be1 be2) vart = let be1' = replaceVarBExp be1 vart
                                        be2' = replaceVarBExp be2 vart
                                    in OrB be1' be2'
replaceVarBExp (Equ be1 be2) vart = let be1' = replaceVarAExp be1 vart
                                        be2' = replaceVarAExp be2 vart
                                    in Equ be1' be2'
replaceVarBExp (Leq be1 be2) vart = let be1' = replaceVarAExp be1 vart
                                        be2' = replaceVarAExp be2 vart
                                    in Leq be1' be2'
replaceVarBExp (Geq be1 be2) vart = let be1' = replaceVarAExp be1 vart
                                        be2' = replaceVarAExp be2 vart
                                    in Geq be1' be2'
replaceVarBExp (Less be1 be2) vart = let be1' = replaceVarAExp be1 vart
                                         be2' = replaceVarAExp be2 vart
                                     in Less be1' be2'
replaceVarBExp (Gre be1 be2) vart = let be1' = replaceVarAExp be1 vart
                                        be2' = replaceVarAExp be2 vart
                                    in Gre be1' be2'

getVarNode :: Graph -> NodeId -> [Var]
getVarNode [] _ = []
getVarNode (h:t) id = if (getNodeId h == id)
                      then getVar h
                      else getVarNode t id

getVar :: Node -> [Var]
getVar (Block id lcmds) = getVarListCFGNot lcmds
getVar (Test id lcmds) = getVarListCFGNot lcmds
getVar _ = error "getVar: node that allowed"

getVarListCFGNot :: [CFGNot] -> [Var]
getVarListCFGNot lcmds = concat $ map getVarCFGNot lcmds

getVarCFGNot :: CFGNot -> [Var]
getVarCFGNot SSkip = []
getVarCFGNot (SAsg x aexp) = x : getVarAExp aexp
getVarCFGNot (Phi x _) = [x]
getVarCFGNot (BE bexp) = getVarBExp bexp

getVarAExp :: AExp -> [Var]
getVarAExp (Var x) = [x]
getVarAExp (Num _) = []
getVarAExp (Negate aexp) = getVarAExp aexp
getVarAExp (Plus ae1 ae2) = (getVarAExp ae1) ++ (getVarAExp ae2)
getVarAExp (Minus ae1 ae2) = (getVarAExp ae1) ++ (getVarAExp ae2)
getVarAExp (Mult ae1 ae2) = (getVarAExp ae1) ++ (getVarAExp ae2)
getVarAExp (Div ae1 ae2) = (getVarAExp ae1) ++ (getVarAExp ae2)

getVarBExp :: BExp -> [Var]
getVarBExp (Not be) = getVarBExp be
getVarBExp (And be1 be2) = (getVarBExp be1) ++ (getVarBExp be2)
getVarBExp (OrB be1 be2) = (getVarBExp be1) ++ (getVarBExp be2)
getVarBExp (Equ ae1 ae2) = (getVarAExp ae1) ++ (getVarAExp ae2)
getVarBExp (Leq ae1 ae2) = (getVarAExp ae1) ++ (getVarAExp ae2)
getVarBExp (Geq ae1 ae2) = (getVarAExp ae1) ++ (getVarAExp ae2)
getVarBExp (Less ae1 ae2) = (getVarAExp ae1) ++ (getVarAExp ae2)
getVarBExp (Gre ae1 ae2) = (getVarAExp ae1) ++ (getVarAExp ae2)

getNodeId :: Node -> NodeId
getNodeId (Block id _) = id
getNodeId (Test id _) = id
getNodeId (Empty id) = id
getNodeId _ = error "getNodeId: node not allowed"
---Auxiliary functions---
