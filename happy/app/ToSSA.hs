module ToSSA where

{-
This file aims to transform blocks in CFG into SSA
We only consider ToCFG2, hence from data Node we only need to deal with Block, Test, and Empty
-}

{-
Problems:
The main problem is that NodeVarTrack is not representing propagated SSA environments at CFG
points. Since information is not passed from predecessors to successors, the code tries to recover
it by looking at ancestors, which is both unnecessary and wrong. The right fix is to propagate
VarTrack along CFG edges and compute each node’s environment from its immediate predecessors.

---------
1) x:=1;if (tt) then {x:=x+2} else {x:=x+1};skip
2) phi functions must have a mistake, bcs it is taking too much time to compute
-}

import ToCFG2
import Syntax
import Com

import Data.List

type VarTrack = [(Var, [Var])]
type SplitNode = [(NodeId, [NodeId])]
type MergingNode = [([NodeId], NodeId)]
type NodeVarTrack = [(NodeId, VarTrack)]

type VarLog = [(Var, Int)]


-- runSSA :: String -> IO()
-- runSSA s = do
--   let c = testCom s
--       (g, es, entry, exit, next) = toCFG c 0
--       (g_ssa, nvar, varlog) = toSSA g es
--   putStrLn (drawCFG g_ssa es)
  
-- runGraph :: String -> Graph
-- runGraph s = let c = testCom s
--                  (g, es, entry, exit, next) = toCFG c 0
--                  join_nodes = endDiv es []              
--                  (g', vart) = toSSAGraph g [] join_nodes
--              in g'

test_toSSA :: String -> IO()
test_toSSA s = do
  let c = testCom s
      (g, es, entry, exit, next) = toCFG c 0
      ((g_ssa, nvart, varlog), trav_order) = toSSA g es
  putStrLn (drawCFG g_ssa es)
  putStrLn "NodeVarTrack:"
  putStrLn $ show(nvart)
  putStrLn "\n VarLog:"
  putStrLn $ show(varlog)
  putStrLn "\n Traverse:"
  putStrLn $ show trav_order
  putStrLn "\n"  

test_traverseGraph :: String -> IO()
test_traverseGraph s = do
  let c = testCom s
      (g, es, entry, exit, next) = toCFG c 0
      (g', es') = rmvEmptyNode (g, es)
      traverse = lastOcc $ traverseGraph g' es' (head entry)
  putStrLn $ drawCFG g' es'
  putStrLn $ show traverse


toSSA :: Graph -> Edges -> ((Graph, NodeVarTrack, VarLog), [(NodeId, [NodeId])])
toSSA [] _ = (([], [], []),[])
toSSA graph eds = let init_node = getNodeId $ head graph
                      trav_order = lastOcc $ traverseGraph graph eds init_node -- [(NodeId, [NodeId])]
                  in (toSSAGraph graph eds [] [] trav_order, trav_order)


toSSAGraph :: Graph -> Edges -> NodeVarTrack -> VarLog -> [(NodeId, [NodeId])] -> (Graph, NodeVarTrack, VarLog)
toSSAGraph g eds nvart varlog [] = (g, nvart, varlog)
toSSAGraph g eds nvart varlog ((n_id, pred):t) = let node = getNode g n_id
                                                     (node_phi, upd_nvart, upd_varlog) = addPhi node nvart varlog (n_id, pred)
                                                     (node_ssa, nvart', varlog') = toSSANode node_phi upd_nvart upd_varlog
                                                     upd_g = updNodeInGraph g node_ssa n_id    
                                                 in toSSAGraph upd_g eds nvart' varlog' t


addPhi :: Node -> NodeVarTrack -> VarLog -> (NodeId, [NodeId]) -> (Node, NodeVarTrack, VarLog)
addPhi node nvart varlog (n_id, pred) = case length pred of
  0 -> (node, nvart, varlog)
  1 -> let n_id_pred = head pred
           vart_pred = retrieveVarTrack n_id_pred nvart
           upd_vart = updNodeVarTrack nvart n_id vart_pred
       in (node, upd_vart, varlog)
  otherwise -> let vart_preds = map (\n -> retrieveVarTrack n nvart) pred -- [(Var, [Var])]
                   --vars_preds = map fst vart_preds -- [Var]
                   (node_phi, vart_phi, varlog_phi) = addReallyPhi node vart_preds varlog
                   upd_nvart = updNodeVarTrack nvart n_id vart_phi
               in (node_phi, upd_nvart, varlog_phi)


addReallyPhi :: Node -> [VarTrack] -> VarLog -> (Node, VarTrack, VarLog)
addReallyPhi node pred_varts varlog =
  let lcom      = retrieveLCFGNotFromNode node
      all_vars  = nub $ concatMap (map fst) pred_varts -- [Var]
      fix_vart  = mergePredVarts pred_varts -- [(Var, [Var])]

      -- For each variable, collect one incoming value per predecessor.
      -- If the variable is absent from that predecessor, use its initial version.
      var_inputs :: [(Var, [Var])]
      var_inputs =
        [ (v, map (incomingVersion v) pred_varts)
        | v <- all_vars
        ]

      phi_info :: [(Var, Var, [Var])]
      phi_info =
        [ (oldv, freshPhiName oldv, ins)
        | (oldv, ins) <- var_inputs
        , needsPhi ins
        ]

      phi_cfgcnot :: [CFGNot]
      phi_cfgcnot =
        [ Phi newv ins
        | (_, newv, ins) <- phi_info
        ]

      node_phi :: Node
      node_phi = updNodeLCFGNot node (phi_cfgcnot ++ lcom)

      upd_vart :: VarTrack
      upd_vart =
        foldl (\vt (oldv, newv, _) -> updVarTrack vt (oldv, newv)) fix_vart phi_info

      upd_varlog :: VarLog
      upd_varlog =
        map (\(var,n) ->
              if var `elem` [oldv | (oldv,_,_) <- phi_info]
              then (var, n+1)
              else (var, n))
            varlog

  in (node_phi, upd_vart, upd_varlog)

  where
    needsPhi :: [Var] -> Bool
    needsPhi []     = False
    needsPhi [_]    = False
    needsPhi (h:t)  = not (all (== h) t)

    incomingVersion :: Var -> VarTrack -> Var
    incomingVersion v vt =
      case [head lv | (x, lv) <- vt, x == v, not (null lv)] of
        h:_ -> h
        []  -> initVar v

    freshPhiName :: Var -> Var
    freshPhiName v =
      case [n | (x,n) <- varlog, x == v] of
        n:_ -> v ++ show n
        []  -> v ++ show 0

    -- Distinguished initial SSA name for variables that are absent on a path.
    -- Semantically, this stands for value 0.
    initVar :: Var -> Var
    initVar v = v ++ "_init"


mergePredVarts :: [VarTrack] -> VarTrack
mergePredVarts [] = []
mergePredVarts vts =
  [ (v, nub $ concat [lv | vt <- vts, (x, lv) <- vt, x == v])
  | v <- nub $ concatMap (map fst) vts
  ]

-- addReallyPhi :: Node -> [VarTrack] -> [Var] -> VarLog -> (Node, VarTrack, VarLog)
-- addReallyPhi node vart_preds vars_preds varlog =
--   let lcom = retrieveLCFGNotFromNode node
--       fix_vart = mergingVart vart_preds (nub vars_preds)
--       (vars_preds_repeat, vars_preds_solo) = splitVars vars_preds
--       var_phi_rep = [(var, [head lv | (v, lv) <- vart_preds, v == var]) | var <- vars_preds_repeat]
--       --var_phi_solo = [(var ++ show(0), [])]
--       phi_info = [ (oldv, oldv ++ show(snd $ head $ filter (\(var, _) -> var == oldv) varlog), lv) | (oldv, lv) <- var_phi_rep, notAllEqualLv lv]
--       phi_cfgcnot = [Phi newv lv | (_, newv, lv) <- phi_info]
--       node_phi = updNodeLCFGNot node (phi_cfgcnot ++ lcom)
--       upd_vart = foldl (\vt (oldv, newv, _) -> updVarTrack vt (oldv, newv)) fix_vart phi_info
--       upd_varlog = map (\(var,n) -> if var `elem` [oldv | (oldv,_,_) <- phi_info]
--                                     then (var,n+1)
--                                     else (var,n)) varlog
--   in (node_phi, upd_vart, upd_varlog)
--   where
--     notAllEqualLv :: [Var] -> Bool
--     notAllEqualLv (h:t) = not $ and [h == e | e <- t]
--     notAllEqualLv [] = False

--     splitVars :: [Var] -> ([Var],[Var])
--     splitVars vars_preds = let rep_vars = nub $ repeatedVars vars_preds
--                                solo_vars = soloVars vars_preds
--                            in (rep_vars, solo_vars)

--     soloVars :: [Var] -> [Var]
--     soloVars [] = []
--     soloVars (h:t) = if elem h t
--                      then soloVars (rmvVar t h)
--                      else h : soloVars t

--     rmvVar :: [Var] -> Var -> [Var]
--     rmvVar [] _ = []
--     rmvVar (h:t) v = if h==v then rmvVar t v else h : rmvVar t v

                       
mergingVart :: VarTrack -> [Var] -> VarTrack
mergingVart [] _ = []
mergingVart ((var, lvar):t) common_vars = if elem var common_vars == False
                                          then (var, lvar) : mergingVart t common_vars
                                          else (var, nub $ lvar ++ (concat [lv | (v,lv) <- t, var==v])) : mergingVart (rmvVar t var) common_vars
  where rmvVar :: VarTrack -> Var -> VarTrack
        rmvVar [] _ = []
        rmvVar ((var, lvar):t) v = if var==v then rmvVar t v else (var, lvar) : rmvVar t v

repeatedVars :: [Var] -> [Var]
repeatedVars [] = []
repeatedVars (h:t) = if elem h t
                     then h : repeatedVars t
                     else repeatedVars t
  
    

toSSANode :: Node -> NodeVarTrack -> VarLog -> (Node, NodeVarTrack, VarLog)
toSSANode (Empty n) nvart varlog = ((Empty n), nvart, varlog)
toSSANode (Test n lcom) nvart varlog = let vart = retrieveVarTrack n nvart
                                           (lcom', vart', varlog') = toSSAListCFGNot lcom vart varlog
                                           nvart' = updNodeVarTrack nvart n vart'
                                       in (Test n lcom', nvart', varlog')
toSSANode (Block n lcom) nvart varlog = let vart = retrieveVarTrack n nvart
                                            (lcom', vart', varlog') = toSSAListCFGNot lcom vart varlog
                                            nvart' = updNodeVarTrack nvart n vart'
                                        in (Block n lcom', nvart', varlog')
toSSANode _ _ _ = error "toSSANode: Invalid kind of Node"


toSSAListCFGNot :: [CFGNot] -> VarTrack -> VarLog -> ([CFGNot], VarTrack, VarLog)
toSSAListCFGNot [] vart varlog = ([],  vart, varlog)
toSSAListCFGNot [h] vart varlog = let (c, vart', varlog') = toSSACFGNot h vart varlog
                                  in ([c], vart', varlog')
toSSAListCFGNot (h:t) vart varlog = let (c, vart', varlog') = toSSACFGNot h vart varlog
                                        (t', vart'', varlog'') = toSSAListCFGNot t vart' varlog'
                                    in (c:t', vart'', varlog'')

toSSACFGNot :: CFGNot -> VarTrack -> VarLog -> (CFGNot, VarTrack, VarLog)
toSSACFGNot SSkip vart varlog = (SSkip, vart, varlog)
toSSACFGNot (SAsg x ae) vart varlog = if isVarInVarLog x varlog == True
                                      then
                                        let ae' = replaceVarAExp ae vart varlog
                                            id = snd $ head $ filter (\(var, _) -> var==x) varlog
                                            newx = x ++ show(id)
                                            vart' = updVarTrack vart (x, newx) 
                                            varlog' = map (\(var,n) -> if var==x then (var, n+1) else (var, n)) varlog
                                        in (SAsg newx ae', vart', varlog')
                                      else
                                        let ae' = replaceVarAExp ae vart ((x,0):varlog)
                                            newx = x++show(0)
                                            vart' = (x,[newx]):vart
                                            varlog' = (x,1):varlog
                                        in (SAsg newx ae', vart', varlog')
toSSACFGNot (BE bexp) vart varlog = let ssa_bexp = replaceVarBExp bexp vart varlog 
                                    in (BE ssa_bexp, vart, varlog)
toSSACFGNot (Phi var lvar) vart varlog = (Phi var lvar, vart, varlog)


---Auxiliary functions---
updNodeLCFGNot :: Node -> [CFGNot] -> Node
updNodeLCFGNot (Empty n) _ = Empty n
updNodeLCFGNot (Block n lcom) upd_lcom = Block n upd_lcom
updNodeLCFGNot (Test n lcom) upd_lcom = Test n upd_lcom
updNodeLCFGNot _ _ = error "updNodeLCFGNot: Invalid Node"

retrieveLCFGNotFromNode :: Node -> [CFGNot]
retrieveLCFGNotFromNode (Empty n) = []
retrieveLCFGNotFromNode (Block _ lcom) = lcom
retrieveLCFGNotFromNode (Test _ lcom) = lcom
retrieveLCFGNotFromNode _ = error "retrieveLCFGNotFromNode: Invalid Node"

updNodeInGraph :: Graph -> Node -> NodeId -> Graph
updNodeInGraph [] _ _ = []
updNodeInGraph (h:t) n nid = let n_id = getNodeId h
                             in if n_id==nid then (n:t) else h : updNodeInGraph t n nid

isVarInVarLog :: Var -> VarLog -> Bool
isVarInVarLog x vart = or $ map (\(var, _) -> if x==var then True else False) vart


lastOcc :: [(NodeId, [NodeId])] -> [(NodeId, [NodeId])]
lastOcc [] = []
lastOcc (h:t) = if elem h t
                then lastOcc t
                else h : lastOcc t

--traverse a graph given an initial node
-- [(NodeId, [NodeId])] --> [(NodeId, list of predecessors)]
traverseGraph :: Graph -> Edges -> NodeId -> [(NodeId, [NodeId])]
traverseGraph g eds nid = let succ = [n_out | (n, _, n_out) <- eds, nid==n]
                              pred = [n | (n, _, n_out) <- eds, nid==n_out] 
                              next = concat $ map (\n -> traverseGraph g eds n) succ
                           in (nid, pred):next

getNode :: Graph -> NodeId -> Node
getNode [] _ = error "node not found"
getNode (h:t) n = let nid = getNodeId h
                   in if nid==n then h else getNode t n

retrieveVarTrack :: NodeId -> NodeVarTrack -> VarTrack
retrieveVarTrack nid [] = []
retrieveVarTrack nid ((n_id,vart):t) = if nid==n_id then vart else retrieveVarTrack nid t

updVarTrack :: VarTrack -> (Var, Var) -> VarTrack
updVarTrack [] (x, x_n) = [(x, [x_n])]
updVarTrack ((var, lvar):t) (x,x_n) = if var==x
                                      then (var, x_n:lvar) : t
                                      else (var, lvar) : updVarTrack t (x,x_n) 

updNodeVarTrack :: NodeVarTrack -> NodeId -> VarTrack -> NodeVarTrack
updNodeVarTrack [] nid vart = [(nid, vart)]
updNodeVarTrack ((n_id, vart_id):t) nid vart = if n_id==nid
                                               then (n_id, vart) : t
                                               else (n_id, vart_id) : updNodeVarTrack t nid vart


-- In replaceVarAExp:
-- x in vart     => use current SSA version
-- x not in vart but in varlog => x is being introduced; use initial value 0
-- x not in vart and not in varlog => undefined variable
replaceVarAExp :: AExp -> VarTrack -> VarLog -> AExp
replaceVarAExp (Var x) vart varlog = if (null $ filter (\(var, _) -> var==x) vart) == False
                                     then let newx = head $ snd $ head $ filter (\(var, _) -> var==x) vart
                                          in Var newx
                                     else if null $ filter (\(var, _) -> var==x) varlog
                                          then error ("replaceVarAExp: Variable " ++ x ++ " is not defined")
                                          else Num 0
replaceVarAExp (Num num) _ _ = Num num
replaceVarAExp (Plus ae1 ae2) vart varlog = let ae1' = replaceVarAExp ae1 vart varlog
                                                ae2' = replaceVarAExp ae2 vart varlog
                                            in Plus ae1' ae2'
replaceVarAExp (Minus ae1 ae2) vart varlog = let ae1' = replaceVarAExp ae1 vart varlog
                                                 ae2' = replaceVarAExp ae2 vart varlog
                                             in Minus ae1' ae2'
replaceVarAExp (Mult ae1 ae2) vart varlog = let ae1' = replaceVarAExp ae1 vart varlog
                                                ae2' = replaceVarAExp ae2 vart varlog
                                            in Mult ae1' ae2'
replaceVarAExp (Div ae1 ae2) vart varlog = let ae1' = replaceVarAExp ae1 vart varlog
                                               ae2' = replaceVarAExp ae2 vart varlog
                                           in Div ae1' ae2'
replaceVarAExp (Negate ae) vart varlog = let ae' = replaceVarAExp ae vart varlog
                                         in Negate ae'


replaceVarBE :: CFGNot -> VarTrack -> VarLog -> [CFGNot]
replaceVarBE (BE b) vart varlog = let b' = replaceVarBExp b vart varlog
                                  in [BE b']
replaceVarBE _ _ _ = error "replaceVarBE: not a Boolean expression"                              

replaceVarBExp :: BExp -> VarTrack -> VarLog -> BExp
replaceVarBExp BTrue _ _ = BTrue
replaceVarBExp BFalse _ _ = BFalse
replaceVarBExp (Not bexp) vart varlog = let bexp' = replaceVarBExp bexp vart varlog
                                        in Not bexp'
replaceVarBExp (And be1 be2) vart varlog = let be1' = replaceVarBExp be1 vart varlog
                                               be2' = replaceVarBExp be2 vart varlog
                                           in And be1' be2'
replaceVarBExp (OrB be1 be2) vart varlog = let be1' = replaceVarBExp be1 vart varlog
                                               be2' = replaceVarBExp be2 vart varlog
                                           in OrB be1' be2'
replaceVarBExp (Equ be1 be2) vart varlog = let be1' = replaceVarAExp be1 vart varlog
                                               be2' = replaceVarAExp be2 vart varlog
                                    in Equ be1' be2'
replaceVarBExp (Leq be1 be2) vart varlog = let be1' = replaceVarAExp be1 vart varlog
                                               be2' = replaceVarAExp be2 vart varlog
                                           in Leq be1' be2'
replaceVarBExp (Geq be1 be2) vart varlog = let be1' = replaceVarAExp be1 vart varlog
                                               be2' = replaceVarAExp be2 vart varlog
                                           in Geq be1' be2'
replaceVarBExp (Less be1 be2) vart varlog = let be1' = replaceVarAExp be1 vart varlog
                                                be2' = replaceVarAExp be2 vart varlog
                                            in Less be1' be2'
replaceVarBExp (Gre be1 be2) vart varlog = let be1' = replaceVarAExp be1 vart varlog
                                               be2' = replaceVarAExp be2 vart varlog
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
