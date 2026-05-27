import Data.List

{-
take a quantum circuit
list of pairs
each pair is the name of the gate and the second is the qubits
[(G, [Qbit])]

goal: execute the circuit efficiently (verify if it exists disjoint qubits on the gates, and put them into parallel layouts)
-}


{-

example:
H[0]; CX[0,1]; H[2] --> [[H[0]; CX[0,1]], [H[2]]]
H[0]; CX[0,1]; H[1] --> [[H[0]; CX[0,1]; H[1]]]

CX[0,1];CX[1,2]
CX[0,1];CX[2,3];CX[1,2]
-}

type Qbit = Integer
type G = String
type QCirc = [(G, [Qbit])]

runC :: QCirc -> [QCirc]
runC [] = [[]]
runC (h@(g,qbits):t) = let l_qbits = nub $ collectQubits qbits [qbits' | (_,qbits')<-t]
                           (qc, rest) = runCAux h t l_qbits
                       in qc : runC rest

runCAux :: (G, [Qbit]) -> QCirc -> [Qbit] -> (QCirc, QCirc)
runCAux (g, qbits) [] l_qbits = if (any (\q -> elem q l_qbits) qbits) == True
                                then ([(g, qbits)], [])
                                else ([], [(g, qbits)])
runCAux (g, qbits) (h:t) l_qbits = if (any (\q -> elem q l_qbits) qbits) == True
                                   then let (qc, rest) = runCAux h t l_qbits
                                        in ((g, qbits):qc, rest)
                                   else let (qc, rest) = runCAux h t l_qbits
                                        in (qc, (g, qbits):rest)

collectQubits :: [Qbit] -> [[Qbit]] -> [Qbit]
collectQubits l [] = l
collectQubits l (h:t) = if (any (\q -> elem q l) h)
                        then collectQubits (l++h) t
                        else if hasOverlap t l==False
                             then l
                             else collectQubits l (t++[h])

hasOverlap :: [[Qbit]] -> [Qbit] -> Bool
hasOverlap l_qbits l = any (\q -> elem q l) (concat l_qbits)



--tests
qc1 = [("H",[0]), ("CX", [0,1]), ("H", [2])]
qc2 = [("H",[0]), ("CX", [0,1]), ("H", [1])]
qc3 = [("CX", [0,1]), ("CX", [1,2])]
qc4 = [("CX", [0,1]), ("CX", [2,3]), ("CX", [1,2])]
qc5 = [("CX", [0,1]), ("CX", [2,3]), ("CX", [1,2]), ("CX", [4,5]), ("CX", [6,7])]
qc6 = [("A",[0]), ("B",[2]), ("C",[0,2])]
qc7 = [("A",[0]), ("C",[0,2]), ("B",[2])]


--Interview--
-- runCirc :: qCirc -> [qCirc]
-- runCirc [] = [[]]
-- runCirc qcirc = let all_qubits = map snd qcirc -- [[Qbit]]
--                     clean_single_qubits = filter (\e -> length e > 1) all_qubits -- list of lists of two or more qubits
--                 in runCircAux qcirc clean_single_qubits

--group_qubtis :: all_qubits -> list_to_build -> return the built list
-- group_qubits :: [[Qbit]] -> [Qbit] -> [Qbit]
-- group_qubits [] l = l
-- group_qubits (h:t) l = if any (\q -> elem q h) l
--                        then group_qubits t (nub $ h++l)
--                        else group_qubits (t++[h]) (nub $ l)


{-
CX[0,1];CX[2,3];CX[1,2]

clean_single_qubits = [[0,1],[2,3],[1,2]]
group_qubits [[2,3],[1,2]] [0,1]
group_qubits [[1,2],[2,3]] [0,1]
group_qubits [[2,3]] [0,1,2] --> [0,1,2,3]

CX[0,1];CX[2,3];CX[1,2];CX[4,5];CX[6,7]
clean_single_qubits = [[0,1],[2,3],[1,2],[4,5]]
group_qubits [[2,3],[1,2],[4,5]] [0,1]
group_qubits [[1,2],[4,5],[2,3]] [0,1]
group_qubits [[2,3],[4,5]] [0,1,2]
group_qubits [[4,5],[6,7]] [0,1,2,3]
-}

-- runCircAux :: qCirc -> [[Qbit]] -> [qCirc]
-- runCircAux [] _ = [[]]
-- runCircAux ((g, qubits):t) l_qubits = 

-- belongsTo :: [Qbit] -> [[Qbit]] -> Bool
-- belongsTo _ [] = False
-- belongsTo qubits (h:t) = if or $ map (\q -> if elem q h then True else False) qubits ==True
--                          then True
--                          else belongsTo qubits t
