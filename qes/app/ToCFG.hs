module ToCFG where

{-
Given a command, build its respective Control-Flow Graph (CFG)
The nodes have one or more possible entry points and/or exit points
-}

import Data.List (intercalate, sortOn)

import Com
import Syntax

-- C -> Current node -> (Graph, edges, entry node, exit node, next fresh node)
toCFG :: C -> NodeId -> (Graph, Edges, [NodeId], [NodeId], NodeId)
toCFG Skip n = ([Block n (flatCmd Skip)], [], [n], [n], n+1)
toCFG (U g qvars) n = ([Block n (flatCmd (U g qvars))], [], [n], [n], n+1)
toCFG (Seq c1 c2) n = if (hasBranches (Seq c1 c2)) == False
                      then let g = [Block n (flatCmd (Seq c1 c2))]
                               eds = []
                           in (g, eds, [n], [n], n+1)
                      else let (g1, eds1, entry1, exit1, next1) = toCFG c1 n
                               (g2, eds2, entry2, exit2, next2) = toCFG c2 next1
                               g = g1++g2
                               eds = eds1++[(out1, Uncond, in2) | out1 <- exit1, in2 <- entry2]++eds2
                           in (g, eds, entry1, exit2, next2)
toCFG (Meas qvar c1 c2) n = let (g1, eds1, entry1, exit1, next1) = toCFG c1 (n+1)
                                (g2, eds2, entry2, exit2, next2) = toCFG c2 next1
                                g = (MeasBlock n [MeasQ qvar]):(g1++g2)
                                eds = [(n, TrueEdge, entry) | entry <- entry1] ++ [(n, FalseEdge, entry) | entry <- entry2] ++ eds1++eds2
                           in (g, eds, [n], exit1++exit2, next2)
toCFG (Whl qvar c) n = let (gT, edsT, entryT, exitT, nextT) = toCFG c (n+1)
                           g = (WhlBlock n [MeasQ qvar]):(Empty nextT):gT
                           eds = (n, FalseEdge, nextT):([(n, TrueEdge, entry) | entry <- entryT] ++ [(exit, Uncond, n) | exit <- exitT] ++ edsT)
                       in (g, eds, [n], [nextT], nextT+1)

rmvEmptyNode :: (Graph,Edges) -> (Graph,Edges)
rmvEmptyNode ([],eds) = ([],eds)
rmvEmptyNode (((Empty n):t),eds) = let new_eds = renewEmptyEdges n eds
                                   in rmvEmptyNode (t, new_eds)
rmvEmptyNode (h:t,eds) = let (nds, new_eds) = rmvEmptyNode (t,eds)
                         in (h:nds, new_eds)

renewEmptyEdges :: NodeId -> Edges -> Edges
renewEmptyEdges _ [] = []
renewEmptyEdges n eds = let enter_n = filter (\(_,_,out) -> out==n) eds -- nodes that enter n, correspond to exit nodes
                            exit_n = filter (\(enter,_,_) -> enter==n) eds --nodes that exit n, correspond to enter nodes
                            bypass = byPass n enter_n exit_n
                            en_ex_n = enter_n++exit_n
                            upd_eds = [edges | edges <- eds, notElem edges en_ex_n]
                        in upd_eds++bypass

byPass :: NodeId -> Edges -> Edges -> Edges
byPass n enter_n exit_n = [(in1,l1,out2) | (in1,l1,out1) <- enter_n, (in2, _, out2) <- exit_n, (in2==n && out1==n)]
  
                                                           
rmvNumber :: Node -> Int
rmvNumber (Block n _) = n
rmvNumber (MeasBlock n _) = n
rmvNumber (WhlBlock n _) = n
rmvNumber (Empty n) = n


hasBranches :: C -> Bool
hasBranches Skip = False
hasBranches (U _ _) = False
hasBranches (Seq c1 c2) = hasBranches c1 || hasBranches c2
hasBranches _ = True

flatCmd :: C -> [CFGNot]
flatCmd Skip = [SSkip]
flatCmd (U g qvars) = [UU g qvars]
flatCmd (Seq c1 c2) = if hasBranches (Seq c1 c2) == True
                      then error "sequential composition contains branching terms"
                      else (flatCmd c1) ++ (flatCmd c2)
flatCmd _ = error "the command contains branching terms"

---Print
nodeId :: Node -> NodeId
nodeId (Block n _)   = n
nodeId (MeasBlock n _)    = n
nodeId (WhlBlock n _)    = n
nodeId (Empty n)      = n

showNode :: Node -> String
showNode (Block n cmds) = "[" ++ show n ++ "] Block: " ++ showCFGNot cmds
showNode (MeasBlock n cmds) =  "[" ++ show n ++ "] MeasBlock: " ++ showCFGNot cmds
showNode (WhlBlock n cmds) =  "[" ++ show n ++ "] WhlBlock: " ++ showCFGNot cmds
showNode (Empty n) = "[" ++ show n ++ "] Empty"

showCFGNot :: [CFGNot] -> String
showCFGNot [] = "{}"
showCFGNot xs = intercalate "\n\t   " (map showCFGNotAux xs)

showCFGNotAux :: CFGNot -> String
showCFGNotAux SSkip       = "skip"
showCFGNotAux (UU g qvars)  = show g ++ show qvars
showCFGNotAux (MeasQ qvar) = show qvar

showEdge :: (NodeId, EdgeLabel, NodeId) -> String
showEdge (a, Uncond, b)   = "[" ++ show a ++ "] --> [" ++ show b ++ "]"
showEdge (a, TrueEdge, b) = "[" ++ show a ++ "] -T-> [" ++ show b ++ "]"
showEdge (a, FalseEdge, b)= "[" ++ show a ++ "] -F-> [" ++ show b ++ "]"

drawCFG :: Graph -> Edges -> String
drawCFG g es =
  unlines $
    ["Nodes:"] ++
    map showNode (sortOn nodeId g) ++
    ["", "Edges:"] ++
    (if null es then ["(none)"] else map showEdge es)


-- A small wrapper for testing from a command
mainCFG :: C -> IO ()
mainCFG c = do
  let (g, es, entry, exit, next) = toCFG c 0
  putStrLn "=== CFG with EmptyNodes==="
  putStrLn (drawCFG g es)
  putStrLn ("Entry node: " ++ show entry)
  putStrLn ("Exit node: "  ++ show exit)
  putStrLn ("Next fresh node: " ++ show next)
  putStrLn ""
  let (g',es') = rmvEmptyNode (g,es)
  putStrLn "=== CFG without EmptyNodes==="
  putStrLn (drawCFG g' es')
  putStrLn ("Entry node: " ++ show entry)
  putStrLn ("Exit node: "  ++ show exit)
  putStrLn ("Next fresh node: " ++ show next)
  putStrLn ""

---Integrate with Parser
runCFG :: String -> IO()
runCFG s = mainCFG $ testC s


---testing examples

