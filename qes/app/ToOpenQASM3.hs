module ToOpenQASM3 where

import Data.List

import Syntax
import Com
import ToCFG

{-
It is assumed that:
  1) the CFG has only one initial node
  2) The number of such node is always 0
  3) The CFG is not empty
  4) There are no empty nodes
-}


-- from CFG to OpenQASM3
toOpenQASM3 :: Graph -> Edges -> IO ()
toOpenQASM3 g eds = do
  putStrLn "OPENQASM 3.0;"
  putStrLn "include \"stdgates.inc\";\n"
  let trav = traverseG g eds 0
      vars = collectVars g eds
  putStrLn $ declareVars vars
  putStrLn $ printG 0 g eds trav vars  

-- declare variables
declareVars :: [(QVar, CVar)] -> String
declareVars vars = let qvars = map fst vars
                       cvars = map snd vars
                   in printQVars qvars ++ "\n" ++ printCVars cvars

printQVars :: [String] -> String
printQVars [] = ""
printQVars (h:t) = "qubit " ++ h ++ ";\n" ++ printQVars t

printCVars :: [String] -> String
printCVars [] = ""
printCVars (h:t) = if h==""
                   then printCVars t
                   else "bit " ++ h ++ ";\n" ++ printCVars t

--Given the traversal of a graph, print its OpenQASM3 instructions (prettier ChatGPT)
printG :: Int -> Graph -> Edges -> [NodeId] -> [(QVar, CVar)] -> String
printG _ _ _ [] _ = ""
printG n g eds (h:t) vars =
  let node = getNode g h
      (str, trav) = fromNode n g eds node t vars
  in str ++ printG n g eds trav vars

-- g -> eds -> node -> traverse_order -> q_c_vars -> (openqasm3 text, updated_traverse_order) (prettier ChatGPT)
fromNode :: Int -> Graph -> Edges -> Node -> [NodeId] -> [(QVar, CVar)] -> (String, [NodeId])
fromNode n g eds (Block _ list_cfg) trav vars =
  (fromListCFGNot n 2 list_cfg vars, trav)
fromNode n g eds (MeasBlock nid list_cfg) next_trav vars =
  let init_text = fromListCFGNot n 0 list_cfg vars
      (true_trav, false_trav) = measTrav g eds nid
      then_text = printG (n + 1) g eds true_trav vars ++ line n "}"
      else_text = line n "else {" ++ printG (n + 1) g eds false_trav vars ++ line n "}"
      upd_trav = filter (`notElem` (true_trav ++ false_trav)) next_trav
  in (init_text ++ then_text ++ else_text, upd_trav)
fromNode n g eds (WhlBlock nid list_cfg) next_trav vars =
  let init_text = fromListCFGNot n 1 list_cfg vars
      true_trav = whlTrav g eds nid
      body_text = printG (n + 1) g eds true_trav vars
      end_text = fromListCFGNot (n + 1) (-1) list_cfg vars ++ line n "}"
      upd_trav = filter (`notElem` true_trav) next_trav
  in (init_text ++ body_text ++ end_text, upd_trav)

indent :: Int -> String
indent n = replicate (2 * n) ' '

line :: Int -> String -> String
line n s = indent n ++ s ++ "\n"

whlTrav :: Graph -> Edges -> NodeId -> [NodeId]
whlTrav g eds nid =
  let tt_nodeid = head $ [n_out | (n_in, edge_label, n_out) <- eds, n_in==nid, edge_label==TrueEdge]
      tt_trav = traverseNids eds tt_nodeid nid 
  in init tt_trav
  
-- g -> eds -> nodeid -> (true_traverse_order_nodeid, false_traverse_order_nodeid)
measTrav :: Graph -> Edges -> NodeId -> ([NodeId], [NodeId])
measTrav g eds nid = 
  let tt_nodeid = head $ [n_out | (n_in, edge_label, n_out) <- eds, n_in==nid, edge_label==TrueEdge]
      ff_nodeid = head $ [n_out | (n_in, edge_label, n_out) <- eds, n_in==nid, edge_label==FalseEdge]
      merging_node = findMergingNode eds tt_nodeid ff_nodeid
  in case merging_node of
    Just nid -> let tt_trav = traverseNids eds tt_nodeid nid
                    ff_trav = traverseNids eds ff_nodeid nid
                in  (init tt_trav, init ff_trav)
    Nothing -> let tt_trav = traverseG g eds tt_nodeid
                   ff_trav = traverseG g eds ff_nodeid
               in (tt_trav, ff_trav)

-- g -> eds -> node_true -> node_false -> merge_node
findMergingNode :: Edges -> NodeId -> NodeId -> Maybe NodeId
findMergingNode eds nid_tt nid_ff 
  | nid_tt==nid_ff = Just nid_tt
  | otherwise =
      let succs_tt = [n_out | (n_in,_,n_out) <- eds, n_in==nid_tt]
      in findMergingNodeTruePath eds succs_tt nid_ff []

findMergingNodeTruePath :: Edges -> [NodeId] -> NodeId -> [NodeId] -> Maybe NodeId
findMergingNodeTruePath _ [] _ _ = Nothing
findMergingNodeTruePath eds (h:t) nid_ff visited_true
  | elem h visited_true = findMergingNodeTruePath eds t nid_ff visited_true
  | otherwise = if h==nid_ff
                then Just h
                else let succs_ff = [n_out | (n_in, _, n_out) <- eds, n_in==nid_ff]
                         succs_h = [n_out | (n_in, _, n_out) <- eds, n_in==h]
                     in case findMergingNodeAux eds h succs_ff [] of
                          Nothing -> findMergingNodeTruePath eds (t++succs_h) nid_ff (h:visited_true)
                          Just nid -> Just nid
      

-- eds -> fixed_node -> dynamic_nodes -> visited_nodes
findMergingNodeAux :: Edges -> NodeId -> [NodeId] -> [NodeId] -> Maybe NodeId
findMergingNodeAux eds _ [] _ = Nothing
findMergingNodeAux eds nid (h:t) visited_false = if nid==h
                                                 then Just nid
                                                 else let succ = [n_out | (n_in,_,n_out) <- eds, n_in==h]
                                                      in if elem h visited_false
                                                         then findMergingNodeAux eds nid t visited_false
                                                         else findMergingNodeAux eds nid (t++succ) (h:visited_false)

--need to change this to a traverse between two nodes and not find a path between two nodes
traverseNids :: Edges -> NodeId -> NodeId -> [NodeId]
traverseNids eds start target = bfs [start] target []
  where
    bfs [] _ visited = reverse visited
    bfs (nid:t) target visited
      | elem nid visited = bfs t target visited
      | otherwise = if nid==target
                    then bfs t target (nid:visited)
                    else let succs = [n_out | (n_in, _, n_out) <- eds, nid == n_in]
                             newSuccs = filter (`notElem` visited) succs
                         in bfs (t ++ newSuccs) target (nid : visited)


existsPath :: Edges -> NodeId -> NodeId -> [NodeId] -> Bool
existsPath [] n_start n_finish _ = n_start==n_finish
existsPath eds n_start n_finish visited_nid =
  let next_n_start = [n_out | (n_in, _, n_out) <- eds, n_in==n_start]
      res_next = elem n_finish next_n_start
      res = or $ concat $ map (\n -> if elem n visited_nid then [] else [existsPath eds n n_finish (n:visited_nid)]) next_n_start
  in res_next || res


-- from a list of CFGNot to a sequence of OpenQASM3 instructions
-- if_or_whl: 0 -> if; 1 -> init while; -1 -> end while; 2 -> block
fromListCFGNot :: Int -> Int -> [CFGNot] -> [(QVar, CVar)] -> String
fromListCFGNot _ _ [] _ = ""
fromListCFGNot n if_or_whl (h:t) vars =
  case h of
    SSkip -> line n "nop;" ++ fromListCFGNot n if_or_whl t vars
    _ -> line n (fromCFGNot if_or_whl h vars) ++ fromListCFGNot n if_or_whl t vars

-- from CFGNot to OpenQASM3
fromCFGNot :: Int -> CFGNot -> [(QVar, CVar)] -> String
fromCFGNot _ (UU g qvars) _ = gatesToOpenQASM3 g ++ qvarsToString qvars ++ ";"
fromCFGNot if_or_whl (MeasQ qvar) vars =
  let cvar = findCVar qvar vars
      meas = cvar ++ " = measure " ++ qvar ++ ";"
  in case if_or_whl of
       0  -> meas ++ "\n" ++ "if (" ++ cvar ++ " == 1) {"
       1  -> meas ++ "\n" ++ "while (" ++ cvar ++ " == 1) {"
       -1 -> meas
       _  -> error "fromCFGNot: invalid context for measurement"
       

findCVar :: QVar -> [(QVar, CVar)] -> CVar
findCVar q [] = error ("findCVar: the classical variable associated to qubit " ++ q ++ " was not found")
findCVar q ((qvar, cvar):t) = if q==qvar
                              then cvar
                              else findCVar q t

qvarsToString :: QVarList -> String
qvarsToString [] = ""
qvarsToString [h] = h
qvarsToString (e1:e2:[]) = e1 ++ ", " ++ e2
qvarsToString (h:t) = h ++ ", " ++ qvarsToString t

gatesToOpenQASM3 :: G -> String
gatesToOpenQASM3 I = "p(0) "
gatesToOpenQASM3 X = "x "
gatesToOpenQASM3 Y = "y "
gatesToOpenQASM3 Z = "z "
gatesToOpenQASM3 H = "h "
gatesToOpenQASM3 S = "s "
gatesToOpenQASM3 T = "t "
gatesToOpenQASM3 SWAP = "swap "
gatesToOpenQASM3 CNOT = "cx "
gatesToOpenQASM3 CZ = "cz "
gatesToOpenQASM3 TOF = "ccx "
gatesToOpenQASM3 _ = undefined


collectVars :: Graph -> Edges -> [(QVar, CVar)]
collectVars g eds = collectVarsAux g (traverseG g eds 0) 0 []

collectVarsAux :: Graph -> [NodeId] -> Int -> [(QVar, CVar)] -> [(QVar, CVar)]
collectVarsAux _ [] _ vars = vars
collectVarsAux g (h:t) i_cvar vars = case getNode g h of
  (Block nid list_cfgnot) -> let qvars_nid = nub $ concat $ map retrieveqVar list_cfgnot
                                 qvars = map fst vars
                                 not_in_vars = filter (`notElem` qvars) qvars_nid
                                 upd_vars = [(qvar, "") | qvar <- not_in_vars] ++ vars
                             in collectVarsAux g t i_cvar upd_vars
  node ->  let (h_vars, h_i_cvar) = createCVar node i_cvar vars
           in collectVarsAux g t h_i_cvar h_vars

createCVar :: Node -> Int -> [(QVar, CVar)] -> ([(QVar, CVar)], Int)
createCVar (Block _ _) i_cvar vars = (vars, i_cvar)
createCVar measwhl_node i_cvar vars = let q_measwhl = head $ retrieveqVar $ head $ retrieveListCFGNot measwhl_node -- QVar
                                          qvars = map fst vars
                                      in if elem q_measwhl qvars
                                         then let upd_vars = map (\(qvar, cvar) -> if (qvar==q_measwhl) && (cvar=="") then (qvar, 'c' : show(i_cvar)) else (qvar, cvar)) vars
                                              in (upd_vars, i_cvar+1)
                                         else (( (q_measwhl, 'c' : show(i_cvar)) : vars), i_cvar+1)


retrieveListCFGNot :: Node -> [CFGNot]
retrieveListCFGNot (Block _ l) = l
retrieveListCFGNot (MeasBlock _ l) = l
retrieveListCFGNot (WhlBlock _ l) = l
                                          
retrieveqVar :: CFGNot -> QVarList
retrieveqVar SSkip = []
retrieveqVar (UU _ qvarlist) = qvarlist
retrieveqVar (MeasQ qvar) = [qvar]


isMeasWhlBlock :: Node -> Bool
isMeasWhlBlock (MeasBlock _ _) = True
isMeasWhlBlock (WhlBlock _ _) = True
isMeasWhlBlock _ = False

-- g -> eds -> current_node -> visited_nodes
traverseG :: Graph -> Edges -> NodeId -> [NodeId]
traverseG _ eds start = bfs [start] []
  where
    bfs [] visited = reverse visited
    bfs (nid:queue) visited
      | nid `elem` visited = bfs queue visited
      | otherwise = let succs = [n_out | (n_in, _, n_out) <- eds, nid == n_in]
                        newSuccs = filter (`notElem` visited) succs
                    in bfs (queue ++ newSuccs) (nid : visited)


getNode :: Graph -> NodeId -> Node
getNode [] _ = error "getNode: Node was not found"
getNode (h:t) nid = if n_id==nid
                    then h
                    else getNode t nid
  where n_id = getNodeId h

getNodeId :: Node -> NodeId
getNodeId (Block nid _) = nid
getNodeId (MeasBlock nid _) = nid
getNodeId (WhlBlock nid _ ) = nid
getNodeId (Empty nid) = nid


--- Test functions
test_com :: [String]
test_com = [
  "skip",
  "H[q]",
  "X[q1];H[q2]",
  "H[q1]; CNOT[q1,q2]; TOF[q1,q2,q3]",
  "Meas(q, skip, skip)",
  "Meas(q, skip, skip); H[q]",
  "Meas(q, skip, Meas(q, X[q], Z[q])); H[q]",
  "H[q];Meas(q, skip, Meas(q, X[q], Z[q])); H[q]",
  "H[q];Meas(q, skip, Meas(q, X[q], skip;Z[q])); H[q]",
  "H[q1];H[q2];Meas(q1, H[q2], H[q2]); Meas(q2, skip, skip); X[q1]",
  "H[q1]; H[q2]; Meas(q2, X[q1], Meas(q1, X[q2], skip))",
  "while q do {skip}",
  "while q do {skip};X[q]",
  "H[q1]; while q1 do {X[q2]};X[q1]",  
  "H[q1]; while q1 do {H[q2]; H[q1]}; Meas(q2, skip, X[q2])",
  "Meas(q, while q do {skip}; skip, Meas(q, X[q], Z[q]); skip); H[q]",
  "while q do {skip}; while q do {H[q]}",
  "while q1 do {H[q2]; while q2 do {skip}}"
           ]

test_func :: (String -> IO ()) -> IO ()
test_func f = test_func_aux f test_com

test_func_aux :: (String -> IO ()) -> [String] -> IO ()
test_func_aux _ [] = putStrLn ""
test_func_aux f (h:t) = do
  putStrLn "---------------"
  putStrLn $ show h
  x <- f h
  putStrLn "---------------\n"
  y <- test_func_aux f t
  return ()

test_toOpenQASM3 :: String -> IO ()
test_toOpenQASM3 s = do
  let c = testC s
      (g, eds, entry, exit, next) = toCFG c 0
      (g', eds') = rmvEmptyNode (g, eds)
  --putStrLn $ drawCFG g' eds'
  toOpenQASM3 g' eds'

test_collectVars :: String -> IO ()
test_collectVars s = do
  let c = testC s
      (g, eds, entry, exit, next) = toCFG c 0
      (g', eds') = rmvEmptyNode (g, eds)
      vars = collectVars g' eds'
  putStrLn $ show vars

-- test_collectCVar :: String -> IO ()
-- test_collectCVar s = do
--   let c = testC s
--       (g, eds, entry, exit, next) = toCFG c 0
--       (g', eds') = rmvEmptyNode (g, eds)
--       vars = collectCVar g' eds' 0 []
--   putStrLn $ show vars

test_traverseG :: String -> IO ()
test_traverseG s = do
  let c = testC s
      (g, eds, entry, exit, next) = toCFG c 0
      (g', eds') = rmvEmptyNode (g, eds)
      traverse = traverseG g' eds' (head entry)
  putStrLn $ drawCFG g' eds'
  putStrLn $ show traverse


test_existsPath :: String -> IO ()
test_existsPath s = do
  let c = testC s
      (g, eds, entry, exit, next) = toCFG c 0
      (g', eds') = rmvEmptyNode (g, eds)
  putStrLn $ drawCFG g' eds'
  putStrLn "starting nid"
  n_start <- getLine
  putStrLn "ending nid"
  n_finish <- getLine
  putStrLn $ show $ existsPath eds' (read n_start :: Int) (read n_finish :: Int) []


test_nidPathDFS :: String -> IO ()
test_nidPathDFS s = do
  let c = testC s
      (g, eds, entry, exit, next) = toCFG c 0
      (g', eds') = rmvEmptyNode (g, eds)
  putStrLn $ drawCFG g' eds'
  putStrLn "starting nid"
  n_start <- getLine
  putStrLn "ending nid"
  n_finish <- getLine
  let result = nidPathDFS eds' (read n_start :: Int) (read n_finish :: Int) []
  case result of
    Nothing -> putStrLn "No path found"
    Just path -> putStrLn $ show path

test_nidPathBFS :: String -> IO ()
test_nidPathBFS s = do
  let c = testC s
      (g, eds, entry, exit, next) = toCFG c 0
      (g', eds') = rmvEmptyNode (g, eds)
  putStrLn $ drawCFG g' eds'
  putStrLn "starting nid"
  n_start <- getLine
  putStrLn "ending nid"
  n_finish <- getLine
  let result = nidPathBFS eds' (read n_start :: Int) (read n_finish :: Int)
  case result of
    Nothing -> putStrLn "No path found"
    Just path -> putStrLn $ show path

test_traverseNids :: String -> IO ()
test_traverseNids s = do
  let c = testC s
      (g, eds, entry, exit, next) = toCFG c 0
      (g', eds') = rmvEmptyNode (g, eds)
  putStrLn $ drawCFG g' eds'
  putStrLn "starting nid"
  n_start <- getLine
  putStrLn "ending nid"
  n_finish <- getLine
  let path = traverseNids eds' (read n_start :: Int) (read n_finish :: Int)
  putStrLn $ show path    


test_findMergingNode :: String -> IO ()
test_findMergingNode s = do
  let c = testC s
      (g, eds, entry, exit, next) = toCFG c 0
      (g', eds') = rmvEmptyNode (g, eds)
  putStrLn $ drawCFG g' eds'
  putStrLn "starting nid"
  n_start <- getLine
  putStrLn "ending nid"
  n_finish <- getLine
  let result = findMergingNode eds' (read n_start :: Int) (read n_finish :: Int)
  case result of
    Nothing -> putStrLn "There is no merging node"
    Just path -> putStrLn $ show path
  
-- collectCVar :: Graph -> Edges -> Int -> [(QVar, CVar)] -> [(QVar, CVar)]
-- collectCVar g eds i_cvar vars = collectCVarAux g (traverseG g eds 0) i_cvar vars

-- collectCVarAux :: Graph -> [NodeId] -> Int -> [(QVar, CVar)] -> [(QVar, CVar)]
-- collectCVarAux _ [] _ vars = vars
-- collectCVarAux g (h:t) i_cvar vars = let node = getNode g h
--                                          (h_vars, h_i_cvar) = createCVar node i_cvar vars
--                                      in collectCVarAux g t h_i_cvar h_vars



---Random functions about graphs
-- Depth-first search to find a path between two nodes -- ChatGPT
nidPathDFS :: Edges -> NodeId -> NodeId -> [NodeId] -> Maybe [NodeId]
nidPathDFS eds current target visited
  | current == target = Just [current]
  | current `elem` visited = Nothing
  | otherwise =
      let nexts = [n_out | (n_in, _, n_out) <- eds, n_in == current]
          try [] = Nothing
          try (n:ns) =
            case nidPathDFS eds n target (current : visited) of
              Just path -> Just (current : path)
              Nothing   -> try ns
      in try nexts

-- Breadth-first search to find a path between two nodes -- ChatGPT
nidPathBFS :: Edges -> NodeId -> NodeId -> Maybe [NodeId]
nidPathBFS eds start target = go [[start]] []
  where
    go [] _ = Nothing
    go (path:queue) visited =
      let current = last path
      in if current == target
         then Just path
         else if current `elem` visited
              then go queue visited
              else
                let nexts = [n_out | (n_in, _, n_out) <- eds, n_in == current]
                    newPaths = [path ++ [n] | n <- nexts]
                in go (queue ++ newPaths) (current : visited)

--to debug
-- printG :: Graph -> Edges -> [NodeId] -> [(QVar, CVar)] -> String
-- printG _ _ [] _ = ""
-- printG g eds (h:t) vars = let node = getNode g h
--                               (str, (tt_path, ff_path), trav) = fromNode g eds node t vars
--                           in "printG argument:" ++ show(h:t) ++
--                              "\ntt_path:" ++ show(tt_path) ++ " ff_path:" ++ show(ff_path) ++
--                              "\nafter printing a node:" ++ show(trav) ++ "\n" ++ printG g eds trav vars


--debug
-- fromNode :: Graph -> Edges -> Node -> [NodeId] -> [(QVar, CVar)] -> (String, ([NodeId],[NodeId]), [NodeId])
-- fromNode g eds (Block nid list_cfg) trav vars = (fromListCFGNot g eds list_cfg vars, ([],[]), trav)
-- fromNode g eds (MeasBlock nid list_cfg) next_trav vars = let init_text = fromListCFGNot g eds list_cfg vars
--                                                              (true_path, false_path) = measTrav g eds nid
--                                                              then_text =  "  " ++ printG g eds true_path vars ++ "}\n"
--                                                              else_text = "else{\n  " ++ printG g eds false_path vars ++ "}\n"
--                                                              rmv_path = true_path++false_path
--                                                              upd_trav = filter (`notElem` rmv_path) next_trav
--                                                          in (init_text ++ then_text ++ else_text, (true_path,false_path), upd_trav)


--- functions before ChatGPT print prettier outputs
-- --Given the traversal of a graph, print its OpenQASM3 instructions
-- printG :: Graph -> Edges -> [NodeId] -> [(QVar, CVar)] -> String
-- printG _ _ [] _ = ""
-- printG g eds (h:t) vars = let node = getNode g h
--                               (str, trav) = fromNode g eds node t vars
--                           in str ++ printG g eds trav vars


-- -- from Node to OpenQASM3
-- -- g -> eds -> node -> traverse_order -> q_c_vars -> (openqasm3 text, updated_traverse_order)
-- fromNode :: Graph -> Edges -> Node -> [NodeId] -> [(QVar, CVar)] -> (String, [NodeId])
-- fromNode g eds (Block nid list_cfg) trav vars = (fromListCFGNot 2 list_cfg vars, trav)
-- fromNode g eds (MeasBlock nid list_cfg) next_trav vars = let init_text = fromListCFGNot 0 list_cfg vars
--                                                              (true_trav, false_trav) = measTrav g eds nid
--                                                              then_text =  "  " ++ printG g eds true_trav vars ++ "}\n"
--                                                              else_text = "else{\n  " ++ printG g eds false_trav vars ++ "}\n"
--                                                              upd_trav = filter (`notElem` (true_trav++false_trav)) next_trav
--                                                          in (init_text ++ then_text ++ else_text, upd_trav)
-- fromNode g eds (WhlBlock nid list_cfg) next_trav vars = let init_text = fromListCFGNot 1 list_cfg vars 
--                                                             true_trav = whlTrav g eds nid
--                                                             do_text = "  " ++ printG g eds true_trav vars 
--                                                             end_text = (fromListCFGNot (-1) list_cfg vars) ++ "}\n"
--                                                             upd_trav = filter (`notElem` true_trav) next_trav
--                                                         in (init_text ++ do_text ++ end_text, upd_trav)


-- fromListCFGNot :: Int -> [CFGNot] -> [(QVar, CVar)] -> String
-- fromListCFGNot _ [] _ = ""
-- fromListCFGNot if_or_whl (h:t) vars = case h of
--   SSkip -> "nop;\n" ++ fromListCFGNot if_or_whl t vars
--   otherwise -> fromCFGNot if_or_whl h vars ++ "\n" ++ fromListCFGNot if_or_whl t vars


-- fromCFGNot :: Int -> CFGNot -> [(QVar, CVar)] -> String
-- --fromCFGNot SSkip = ""
-- fromCFGNot _ (UU g qvars) _ = let g_opq3 = gatesToOpenQASM3 g
--                                   qvars_opq3 = qvarsToString qvars
--                               in g_opq3 ++ qvars_opq3 ++ ";"
-- fromCFGNot if_or_whl (MeasQ qvar) vars = let cvar = findCVar qvar vars 
--                                              meas = cvar ++ " = measure " ++ qvar ++ ";\n"
--                                              if_text = "if (" ++ cvar ++ "==1){"
--                                              whl_text = "while (" ++ cvar ++ "==1){"
--                                          in case if_or_whl of
--                                               0 -> meas ++ if_text
--                                               1 -> meas ++ whl_text
--                                               -1 -> takeWhile (\e -> e /= '\n') meas

