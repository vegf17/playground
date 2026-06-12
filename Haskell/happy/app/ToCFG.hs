module ToCFG where

{-
Given a command, build its respective Control-Flow Graph (CFG)
All the nodes are composed by a entrance and exit node
If clauses demand the insertion of a join node, which unifies the exit nodes
In while loops, the exit node is represented by the false branch
-}

import Data.List (intercalate, sortOn)

import Com
import Syntax

-- C -> Current node -> (Graph, edges, entry node, exit node, next fresh node)
toCFG :: C -> NodeId -> (Graph, Edges, NodeId, NodeId, NodeId)
toCFG Skip n = ([Block n (flatCmd Skip)], [], n, n, n+1)
toCFG (Asg var e) n = ([Block n (flatCmd (Asg var e))], [], n, n, n+1)
toCFG (Seq c1 c2) n = if (hasBranches (Seq c1 c2)) == False
                      then let g = [Block n (flatCmd (Seq c1 c2))]
                               eds = []
                           in (g, eds, n, n, n+1)
                      else let (g1, eds1, entry1, exit1, next1) = toCFG c1 n
                               (g2, eds2, entry2, exit2, next2) = toCFG c2 next1
                           in (g1++g2, (exit1, Uncond, entry2):(eds1++eds2), entry1, exit2, next2)
toCFG (IfC bexp c1 c2) n = let (g1, eds1, entry1, exit1, next1) = toCFG c1 (n+1)
                               (g2, eds2, entry2, exit2, next2) = toCFG c2 next1
                               g = (Join next2):(g1++g2)
                               eds = (exit1, Uncond, next2):(exit2, Uncond, next2):(eds1++eds2)
                           in ((Test n bexp):g, (n, TrueEdge, entry1):(n, FalseEdge, entry2):eds, n, next2, next2+1)
toCFG (Whl bexp c) n = let (gT, edsT, entryT, exitT, nextT) = toCFG c (n+1)
                           g = (Test n bexp):(Join nextT):gT
                           eds = (n, TrueEdge, entryT):(exitT, Uncond, n):(n, FalseEdge, nextT):edsT
                       in (g, eds, n, nextT, nextT+1)

hasBranches :: C -> Bool
hasBranches Skip = False
hasBranches (Asg var e) = False
hasBranches (Seq c1 c2) = hasBranches c1 || hasBranches c2
hasBranches _ = True

flatCmd :: C -> [SimpleCmd]
flatCmd Skip = [SSkip]
flatCmd (Asg var e) = [SAsg var e]
flatCmd (Seq c1 c2) = if hasBranches (Seq c1 c2) == True
                      then error "sequential composition contains branching terms"
                      else (flatCmd c1) ++ (flatCmd c2)
flatCmd _ = error "the command contains branching terms"

---Integrate with Parser
runCFG :: String -> IO()
runCFG s = mainCFG $ testCom s


---Print
nodeId :: Node -> NodeId
nodeId (Entry n)     = n
nodeId (Exit n)      = n
nodeId (Join n)      = n
nodeId (Block n _)   = n
nodeId (Test n _)    = n

showNode :: Node -> String
showNode (Entry n) =
  "[" ++ show n ++ "] Entry"
showNode (Exit n) =
  "[" ++ show n ++ "] Exit"
showNode (Join n) =
  "[" ++ show n ++ "] Join"
showNode (Block n cmds) =
  "[" ++ show n ++ "] Block: " ++ showCmds cmds
showNode (Test n b) =
  "[" ++ show n ++ "] Test: " ++ show b

showCmds :: [SimpleCmd] -> String
showCmds [] = "{}"
showCmds xs = intercalate "; " (map showSimpleCmd xs)

showSimpleCmd :: SimpleCmd -> String
showSimpleCmd SSkip       = "skip"
showSimpleCmd (SAsg x e)  = x ++ " := " ++ show e

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
  putStrLn "=== CFG ==="
  putStrLn (drawCFG g es)
  putStrLn ("Entry node: " ++ show entry)
  putStrLn ("Exit node: "  ++ show exit)
  putStrLn ("Next fresh node: " ++ show next)
  putStrLn ""
