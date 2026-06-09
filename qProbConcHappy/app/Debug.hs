module Debug where

{-
This module has the goal to "debug", i.e. to give a trace of the states occurred during the computation
We do this resorting to the history of the computation, that the scheduler uses and produces
-}

--Haskell imports--
import Control.Monad.State
import Control.Monad.Trans 
import Control.Monad.Trans.Except
import Control.Monad.Writer
--import Control.Monad.Trans.Maybe
--import Control.Monad
--import Control.Monad.Identity
--import System.Random
import Data.Either
import Data.List
import Data.Ord
import Data.Complex
import Data.Ratio
import Data.Char
import Data.Fixed
import Data.Matrix
--Haskell imports--

--my imports--
import Syntax
import SemAEBE
import SmallStep
import Examples
import DistTMonad
import Beautify
import KStep
import Gates
import User_Gates
import QuantumCalc
--my imports--

type NodeId = Int
type Lvl = Int

data Node = Block NodeId Lvl String
          | Init Lvl String

type Graph = [Node]
type Edges = [(NodeId, NodeId)]



type Hist = [(String, LMem)]
type Debug a = StateT LMem (ExceptT LMem (WriterT Hist (DistT []))) a --small for debug
type SchDebug = ProbPath -> Maybe [([((Either LMem (C, LMem), Hist), Double)], Double)]

debugSmall :: C -> Debug C
debugSmall Skip = StateT $ \s -> ExceptT $ WriterT $ DistT $ [[((Left s, [(comToStr Skip, s)]), 1)]]
debugSmall (Asg var e) = do
  (sc,l,sq) <- get
  let sc' = changeSt var (bigStepExp e sc) sc
      lmem = (sc',l,sq)
      strC = comToStr (Asg var e)
  StateT $ \_ -> ExceptT $ WriterT $ DistT $ [[((Left lmem, [(strC, lmem)]), 1)]]
debugSmall (Reset q) = do
  (sc,l,sq) <- get
  let sq' = resetOpDen (qNumsAux q l) sq
      lmem = (sc,l,sq')
      strC = comToStr (Reset q)
  StateT $ \_ -> ExceptT $ WriterT $ DistT $ [[((Left lmem, [(strC, lmem)]), 1)]]      
debugSmall (U g qvar) = do
  (sc,l,sq) <- get
  let sq' = appGateOpDen g (qNums qvar l) sq
      lmem = (sc,l,sq')
      strC = comToStr (U g qvar)
  StateT $ \_ -> ExceptT $ WriterT $ DistT $ [[((Left lmem, [(strC, lmem)]), 1)]]      
debugSmall (Meas (x,q)) = do
  (sc,l,sqt) <- get
  let sq = zeroIfSmallS sqt
      p0 = probOpDen 0 ((qNumsAux q l)) sq -- probability of measuring qubit q to be in state |0>
      p1 = probOpDen 1 ((qNumsAux q l)) sq -- probability of measuring qubit q to be in state |1>
      sq0 = stateMeasOpDen 0 ((qNumsAux q l)) sq -- state of the system of qubits after measuring qubit q in state |0>
      sq1 = stateMeasOpDen 1 ((qNumsAux q l)) sq -- state of the system of qubits after measuring qubit q in state |1>
      sc0 = changeSt x 0 sc -- assigning the value 0 to the variable x
      sc1 = changeSt x 1 sc -- assigning the value 1 to the variable x
      lmem0 = (sc0,l,sq0)
      lmem1 = (sc1,l,sq1)
      str0 = "P0(" ++ x ++ "<-" ++ q ++ ")"
      str1 = "P1(" ++ x ++ "<-" ++ q ++ ")"      
  if (p0==0.0)
    then StateT $ \_ -> ExceptT $ WriterT $ DistT $ [[((Left lmem1, [(str1, lmem1)]), p1)]]
    else if (p1==0.0)
    then StateT $ \_ -> ExceptT $ WriterT $ DistT $ [[((Left lmem0, [(str0, lmem0)]), p0)]]
    else StateT $ \_ -> ExceptT $ WriterT $ DistT $ [[((Left lmem0, [(str0, lmem0)]),p0), ((Left lmem1, [(str1, lmem1)]),p1)]]
debugSmall (P prob c1 c2) = do
  s <- get
  let p = fromRational prob
      cc1 = runDistT $ runWriterT $ runExceptT $ runStateT (debugSmall c1) s  -- :: [[((Either S (Com, S), Hist), Prob)]]
      cc2 = runDistT $ runWriterT $ runExceptT $ runStateT (debugSmall c2) s  -- :: [[((Either S (Com, S), Hist), Prob)]]
      pc1 = [[(c,p*p')| (c,p') <- lcc1] | lcc1 <- cc1]
      pc2 = [[(c,(1-p)*p')| (c,p') <- lcc2] | lcc2 <- cc2]
      pc1c2 = concat [[ec1++ec2 | ec2 <- pc2] | ec1 <- pc1]
  StateT $ \_ -> ExceptT $ WriterT $ DistT $ pc1c2          
debugSmall (Seq c1 c2) = do
  s <- get
  let cp = runDistT $ runWriterT $ runExceptT $ runStateT (debugSmall c1) s -- :: [((Either LMem (C, LMem), Hist), Double)]
      seqC = [compSeqDebug dist c2 | dist <- cp]
  StateT $ \_ -> ExceptT $ WriterT $ DistT $ seqC
debugSmall (Or c1 c2) = do 
    s <- get 
    let cp1 = runDistT $ runWriterT $ runExceptT $ runStateT (debugSmall c1) s -- :: [[(Either LMem (Com, LMem), Prob)]]
        cp2 = runDistT $ runWriterT $ runExceptT $ runStateT (debugSmall c2) s -- :: [[(Either LMem (Com, LMem), Prob)]]
    StateT $ \_ -> ExceptT $ WriterT $ DistT $ (cp1++cp2) 
debugSmall (Par c1 c2) = do 
    s <- get 
    let cp1 = runDistT $ runWriterT $ runExceptT $ runStateT (debugSmall c1) s -- :: [[(Either LMem (Com, LMem), Prob)]]
        cp2 = runDistT $ runWriterT $ runExceptT $ runStateT (debugSmall c2) s -- :: [[(Either LMem (Com, LMem), Prob)]]
        par1 = [compParRDebug dist c2| dist <- cp1]
        par2 = [compParLDebug dist c1| dist <- cp2]
    StateT $ \_ -> ExceptT $ WriterT $ DistT $ par1++par2
debugSmall (IfC bExp c1 c2) = do
  (sc,l,sq) <- get
  let b = bigStepBExp bExp sc
      lmem = (sc,l,sq)
  if b == True
    then StateT $ \_ -> ExceptT $ WriterT $ DistT $ [[((Right (c1, lmem), [("If-TT", lmem)]), 1.0)]]
    else StateT $ \_ -> ExceptT $ WriterT $ DistT $ [[((Right (c2, lmem), [("If-FF", lmem)]),1.0)]]
debugSmall (Whl bExp c) = do
  (sc,l,sq) <- get
  let b = bigStepBExp bExp sc
      lmem = (sc,l,sq)
  if b == True
    then StateT $ \_ -> ExceptT $ WriterT $ DistT $ [[((Right (Seq c (Whl bExp c), lmem), [("Whl-TT", lmem)]), 1.0)]]
    else StateT $ \_ -> ExceptT $ WriterT $ DistT $ [[((Left lmem, [("Whl-FF", lmem)]), 1.0)]]
debugSmall (Await bExp c) = do
  (sc,l,sq) <- get
  let b = bigStepBExp bExp sc
      lmem = (sc,l,sq)
  if b == False
    then StateT $ \_ -> ExceptT $ WriterT $ DistT $ [[((Right (Await bExp c, (sc,l,sq)), [("Aw-FF", lmem)]), 1.0)]]
    else StateT $ \_ -> ExceptT $ WriterT $ DistT $ [[((Right (Atom c, (sc,l,sq)), [("Aw-TT", lmem)]), 1.0)]]
debugSmall (Atom c) = do
  s <- get
  let cp = runDistT $ runWriterT $ runExceptT $ runStateT (debugSmall c) s
      atom = [inAtomDebug dist | dist <- cp]
  StateT $ \_ -> ExceptT $ WriterT $ DistT $ atom



--debug for the big step semantics
debug_kStepSch :: (SchDebug, ProbPath, Hist, Int) -> Dist (LMem, Hist)
debug_kStepSch (_, path, hist, 0) = Dist [((snd $ snd path, hist), 0)]
debug_kStepSch (sch_d, l@(path, (c, s)), hist, k) =
  case (sch_d l) of
    Nothing -> error "Scheduler undefined"
    Just convDist -> do
      let ppL = [[ ((s,hist++h), p*q) | ((s,h), p) <- (projLDebug dist)] | (dist, q) <- convDist] -- [([((S, Hist), Double)], Double)]
          next_eval = [
            [
              ((sch_d, (path ++ [((c, s), rmvHist dist)], cs), hist++h, k-1), p*q)
            | ((cs, h), p) <- (projRDebug dist)
            ]
            | (dist, q) <- convDist
            ] -- 
          transStep = (Dist $ concat next_eval) >>= debug_kStepSch
      addDist transStep (Dist $ concat ppL)


run_debug_KStepSch :: SchDebug -> C -> LMem -> Int -> [((Mem, Hist), Double)]
run_debug_KStepSch sch_d c lmem k = cleanL $ getDist $ debug_kStepSch (sch_d, ([],(c,lmem)), [], k)
  where cleanL = map (\(((sc,l,sq),h),p) -> (((sc,sq),h),p))

show_debug_KStepSch :: SchDebug -> C -> LMem -> Int -> IO()
show_debug_KStepSch sch_d c lmem k = let res = run_debug_KStepSch sch_d c lmem k
                                     in putStrLn $ print_debug res


print_debug :: [((Mem, Hist), Double)] -> String
print_debug [] = ""
print_debug l = intercalate "\n\n" $ zipWith showBranch [1 :: Int ..] l

showBranch :: Int -> ((Mem, Hist), Double) -> String
showBranch i (((stc, stq), hist), p) =
  "Branch " ++ show i ++ "\n"
  ++ "Probability: " ++ show p ++ "\n"
  ++ "Classical memory:\n"
  ++ indent (showStC stc) ++ "\n"
  ++ "Quantum state:\n"
  ++ indent (showStQ stq) ++ "\n"
  ++ "History:\n"
  ++ indent (showHist hist)

showStQ :: StQ -> String
showStQ sq = rmvPlus $ denOpToKetBraComplex sq

indent :: String -> String
indent = unlines . map ("  " ++) . lines

showHist :: Hist -> String
showHist [] = ""
showHist l = intercalate "\n" $ zipWith showHistStep [0 :: Int ..] l

showHistStep :: Int -> (String, LMem) -> String
showHistStep i (label, _) =  "Step " ++ show i ++ ": "  ++ label


--START: priority for Atom--
--prioritize the evaluation of Atom commands
debug_evalAtom :: C -> LMem -> [[((Either LMem (C, LMem), Hist), Double)]]
debug_evalAtom (Atom c) s = runDistT $ runWriterT $ runExceptT $ runStateT (debugSmall (Atom c)) s
debug_evalAtom (P prob c1 c2) s = let p = fromRational prob
                                      listDist1 = debug_evalAtom c1 s
                                      listDist2 = debug_evalAtom c2 s
                                      pc1 = [[(c,p*p')| (c,p') <- lcc1] | lcc1 <- listDist1]
                                      pc2 = [[(c,(1-p)*p')| (c,p') <- lcc2] | lcc2 <- listDist2]
                                  in  concat [[ec1++ec2 | ec2 <- pc2] | ec1 <- pc1]
debug_evalAtom (Seq c1 c2) s =
  case (nextAtom c1) of
    True -> let listDist = debug_evalAtom c1 s
            in [compSeqDebug dist c2 | dist <- listDist]
    False -> runDistT $ runWriterT $ runExceptT $ runStateT (debugSmall (Seq c1 c2)) s
debug_evalAtom (Or c1 c2) s =
  case (nextAtom c1, nextAtom c2) of
    (True, False) -> debug_evalAtom c1 s
    (False, True) -> debug_evalAtom c2 s
    otherwise -> runDistT $ runWriterT $ runExceptT $ runStateT (debugSmall (Or c1 c2)) s
debug_evalAtom (Par c1 c2) s =
  case (nextAtom c1, nextAtom c2) of
    (True, False) ->  let listDist = debug_evalAtom c1 s
                      in [compParRDebug dist c2 | dist <- listDist]
    (False, True) -> let listDist = debug_evalAtom c2 s
                     in [compParLDebug dist c1 | dist <- listDist]
    otherwise -> runDistT $ runWriterT $ runExceptT $ runStateT (debugSmall (Par c1 c2)) s
debug_evalAtom c s = runDistT $ runWriterT $ runExceptT $ runStateT (debugSmall c) s
--END: priority for Atom--
      
      

--START: auxiliary functions (complementary to the ones in SmallStep.hs)--
compSeqDebug :: [((Either LMem (C, LMem), Hist), Double)] -> C -> [((Either LMem (C, LMem), Hist), Double)]
compSeqDebug [] _ = []
compSeqDebug (((Left s, hist), p) : t) c = ((Right (c,s), hist), p) : compSeqDebug t c
compSeqDebug (((Right (cc, s), hist),p) : t) c = ((Right (Seq cc c, s), hist), p) : compSeqDebug t c

compParRDebug :: [((Either LMem (C, LMem), Hist), Double)] -> C -> [((Either LMem (C, LMem), Hist), Double)]
compParRDebug [] _ = []
compParRDebug (((Left s, hist), p) : t) c = ((Right (c,s), hist), p) : compParRDebug t c
compParRDebug (((Right (cc, s), hist),p) : t) c = ((Right (Par cc c, s), hist), p) : compParRDebug t c

compParLDebug :: [((Either LMem (C, LMem), Hist), Double)] -> C -> [((Either LMem (C, LMem), Hist), Double)]
compParLDebug [] _ = []
compParLDebug (((Left s, hist), p) : t) c = ((Right (c,s), hist), p) : compParLDebug t  c
compParLDebug (((Right (cc, s), hist),p) : t) c = ((Right (Par c cc, s), hist), p) : compParLDebug t c

inAtomDebug :: [((Either LMem (C, LMem), Hist), Double)] -> [((Either LMem (C, LMem), Hist), Double)]
inAtomDebug [] = []
inAtomDebug (((Left s, hist), p) : t) = ((Left s, hist), p) : inAtomDebug t
inAtomDebug (((Right (c, s), hist),p) : t) = ((Right (Atom c, s), hist), p) : inAtomDebug t


projLDebug :: [((Either a b, Hist), Double)] -> [((a, Hist), Double)]
projLDebug [] = []
projLDebug (((Left a, h),p):t) = ((a,h),p) : projLDebug t
projLDebug (((Right b, _) ,p):t) = projLDebug t

projRDebug :: [((Either a b, Hist), Double)] -> [((b, Hist), Double)]
projRDebug [] = []
projRDebug (((Left a, _),p):t) = projRDebug t
projRDebug (((Right b, h) ,p):t) = ((b,h),p) : projRDebug t

rmvLAuxII :: [((Either LMem (a, LMem), Hist), Double)] -> [((Either Mem (a, Mem), Hist), Double)]
rmvLAuxII [] = []
rmvLAuxII (((Left (sc,l,sq), hist), p):t) = ((Left (sc,sq), hist), p) : rmvLAuxII t
rmvLAuxII (((Right (c, (sc,l,sq)), hist), p) :t) = ((Right (c, (sc,sq)), hist), p) : rmvLAuxII t


rmvHist :: [((Either LMem (C, LMem), Hist), Double)] -> [((Either LMem (C, LMem)), Double)]
rmvHist [] = []
rmvHist (((Left lmem, hist), p):t) = (Left lmem, p) : rmvHist t
rmvHist (((Right (c, lmem), hist), p):t) = (Right (c, lmem), p) : rmvHist t
--END: auxiliary functions (complementary to the ones in SmallStep.hs)--


--START: functions to beautify the history--
comToStr :: C -> String
comToStr Skip = "skip"
comToStr (Asg x a) = x ++ " := " ++ aexpToStr a
comToStr (Reset q) = "reset " ++ q
comToStr (U g qs) = gateToStr g ++ " " ++ qvarListToStr qs
comToStr (Meas (x, q)) = x ++ " := meas " ++ q
comToStr (Seq c1 c2) = comToStr c1 ++ ";\n" ++ comToStr c2
comToStr (Or c1 c2) = "(" ++ comToStr c1 ++ ") or (" ++ comToStr c2 ++ ")"
comToStr (Par c1 c2) = "(" ++ comToStr c1 ++ ") || (" ++ comToStr c2 ++ ")"
comToStr (P p c1 c2) = "(" ++ comToStr c1 ++ ") [" ++ rationalToStr p ++ "] (" ++ comToStr c2 ++ ")"
comToStr (IfC b c1 c2) = "if " ++ bexpToStr b ++ " then {\n" ++ comToStr c1 ++ "\n} else {\n" ++ comToStr c2 ++ "\n}"
comToStr (Whl b c) = "while " ++ bexpToStr b ++ " do {\n" ++ comToStr c ++ "\n}"
comToStr (Await b c) = "await " ++ bexpToStr b ++ " do {\n" ++ comToStr c ++ "\n}"
comToStr (Atom c) = "atomic {\n" ++ comToStr c ++ "\n}"

aexpToStr :: AExp -> String
aexpToStr (Num n) = show n
aexpToStr (Var x) = x
aexpToStr Pi = "pi"
aexpToStr (Plus a1 a2) = "(" ++ aexpToStr a1 ++ " + " ++ aexpToStr a2 ++ ")"
aexpToStr (Minus a1 a2) = "(" ++ aexpToStr a1 ++ " - " ++ aexpToStr a2 ++ ")"
aexpToStr (Mult a1 a2) = "(" ++ aexpToStr a1 ++ " * " ++ aexpToStr a2 ++ ")"
aexpToStr (Div a1 a2) = "(" ++ aexpToStr a1 ++ " / " ++ aexpToStr a2 ++ ")"
aexpToStr (Negate a) = "(-" ++ aexpToStr a ++ ")"
aexpToStr (Sqrt a) = "sqrt(" ++ aexpToStr a ++ ")"

bexpToStr :: BExp -> String
bexpToStr BTrue = "true"
bexpToStr BFalse ="false"
bexpToStr (Not b) = "not (" ++ bexpToStr b ++ ")"
bexpToStr (And b1 b2) = "(" ++ bexpToStr b1 ++ " && " ++ bexpToStr b2 ++ ")"
bexpToStr (OrB b1 b2) = "(" ++ bexpToStr b1 ++ " || " ++ bexpToStr b2 ++ ")"
bexpToStr (Equ a1 a2) = "(" ++ aexpToStr a1 ++ " == " ++ aexpToStr a2 ++ ")"
bexpToStr (Leq a1 a2) = "(" ++ aexpToStr a1 ++ " <= " ++ aexpToStr a2 ++ ")"
bexpToStr (Geq a1 a2) = "(" ++ aexpToStr a1 ++ " >= " ++ aexpToStr a2 ++ ")"
bexpToStr (Less a1 a2) = "(" ++ aexpToStr a1 ++ " < " ++ aexpToStr a2 ++ ")"
bexpToStr (Gre a1 a2) = "(" ++ aexpToStr a1 ++ " > " ++ aexpToStr a2 ++ ")"

gateToStr :: G -> String
gateToStr I = "I"
gateToStr X = "X"
gateToStr Y = "Y"
gateToStr Z = "Z"
gateToStr H = "H"
gateToStr S = "S"
gateToStr T = "T"
gateToStr SWAP = "SWAP"
gateToStr CNOT = "CNOT"
gateToStr CZ = "CZ"
gateToStr TOF = "TOF"
gateToStr (Ph a) = "Ph(" ++ aexpToStr a ++ ")"
gateToStr (CPh a) = "CPh(" ++ aexpToStr a ++ ")"
gateToStr Umag2 = "Umag2"
gateToStr Vmag3 = "Vmag3"
gateToStr (UD name) = name


qvarListToStr :: QVarList -> String
qvarListToStr qs = "[" ++ intercalate ", " qs ++ "]"

rationalToStr :: Rational -> String
rationalToStr r
  | denominator r == 1 = show (numerator r)
  | otherwise          = show (numerator r) ++ "/" ++ show (denominator r)  
--END: functions to beautify the history--



--START: definition of schedulers adapted to Debug--
--undefined scheduler
undSch :: SchDebug
undSch _ = Nothing

--scheduler that chooses always the first element in the list of the possible transitions
initSchDebug :: SchDebug
initSchDebug (path,(c,s)) =
  --let listDist = runDistT $ runExceptT $ runStateT (small c) s -- [[(Either LMem (C,LMem), Double)]]
  let listDist = debug_evalAtom c s -- [[(Either LMem (C,LMem), Double)]]
  in Just $ [(head listDist, 1)]

--scheduler that chooses always the last element in the list of the possible transitions
lastSchDebug :: SchDebug
lastSchDebug (path,(c,s)) =
  --let listDist = runDistT $ runExceptT $ runStateT (small c) s -- [[(Either LMem (C,LMem), Double)]]
  let listDist = debug_evalAtom c s
  in Just $ [(last listDist, 1)]

--scheduler that chooses always the middle element in the list of the possible transitions
middleSchDebug :: SchDebug
middleSchDebug (path,(c,s)) =
  --let listDist = runDistT $ runExceptT $ runStateT (small c) s -- [[(Either LMem (C,LMem), Double)]]
  let listDist = debug_evalAtom c s
      ind = fromIntegral $ floor ((fromIntegral $ length listDist)/2)
  in Just $ [(listDist!!ind, 1)]  

--uniform scheduler, which attributes the same probability to all the elements in the list of the
--possible transitions
uniSchDebug :: SchDebug
uniSchDebug (path,(c,s)) =
  --let listDist = runDistT $ runExceptT $ runStateT (small c) s -- [[(Either LMem (C,LMem), Double)]]
  let listDist = debug_evalAtom c s
      len = fromIntegral $ length listDist
  in Just $ [(dist, 1/len) | dist <- listDist]


-- --non blocking scheduler that checks the history path to see if the current command and the classical state
-- --already occurred; if yes, then the scheduler tries another option
nonBlockSchDebug :: SchDebug
nonBlockSchDebug hist@(path,(c,s)) = 
  --let listDist = runDistT $ runExceptT $ runStateT (small c) s -- [[(Either LMem (C,LMem), Double)]]
  let listDist = debug_evalAtom c s -- [[((Either LMem (C, LMem), Hist), Double)]]
      new_listDist = map rmvHist listDist
      head_listDist = head new_listDist -- [((Either LMem (C, LMem), Hist), Double)]
      tail_listDist = tail new_listDist -- [[((Either LMem (C, LMem), Hist), Double)]]
      (nextDist, i) = notRepeatedDebug hist (head_listDist, tail_listDist) 0
  in Just $ [(listDist!!i, 1)]


--Receives a probabilistic path, a tuple composed of the scheduled next step and all the unscheduled
--next steps, and returns a next step that has not occur yet; in the case where all the possible
--next steps, scheduled or unscheduled, already appeared in the probabilistic path, the next step
--chosen is the last one unscheduled
notRepeatedDebug :: ProbPath -> ([(Either LMem (C,LMem), Double)],[[(Either LMem (C,LMem), Double)]]) -> Int -> ([(Either LMem (C,LMem), Double)], Int)
notRepeatedDebug _ (next,[]) i = (next, i)
notRepeatedDebug (path,(c,s)) (next,(h:t)) i = if (allLeft || null intersection ) -- if next is composed only by final states or it did not appear in the history --  || allIn listComProbPath nextR
                                          then (next, i) -- then return next
                                          else notRepeatedDebug (path,(c,s)) (h,t) (i+1)-- else repeat the procedure by giving as next step the head of the list of the unscheduled next steps
  where allLeft = and $ map (\x -> isLeft x) (map fst next) --verifies if all the elements of next are final states
        nextL = lefts (map fst next) --collects the final states from next
        nextR = rights (map fst next) --collects the computations that did not finish
        nextRClassic = map (\(c,(sc,l,sq)) -> (c,sc)) nextR --collects the computations that did not finish only considering the command and the classical state
        listComProbPath = map fst path --list with all the commands and states that occurred during the computation
        listComProbPathClassic = map (\(c,(sc,l,sq)) -> (c,sc)) listComProbPath --list with all the commands and states that occurred during the computation only considering the command and the classical state
        intersection = intersect listComProbPathClassic nextRClassic --checks if the unfinished computations in nextR are present in the history


-- --limited fair scheduler, that keeps in memory the last 10 steps of the computation
limitnonBlockSchDebug :: SchDebug
limitnonBlockSchDebug hist@(path,(c,s)) = if (length path == 50)
                                 then nonBlockSchDebug (tail path, (c,s))
                                 else nonBlockSchDebug hist


-- --"Random" scheduler
-- --Uses the length of the possible steps to decide if the next configuration is the initial, the
-- --final, or the intermediate in the list of possible choices
-- --To do that, we do the following with lenListDist being the length of the possible steps:
-- --mod lenListDist 3 = 0 --> initial
-- --mod lenListDist 3 = 1 --> intermediate
-- --mod lenListDist 3 = 2 --> final
randSchDebug :: SchDebug
randSchDebug hist@(path,(c,s)) =
  let listDist = debug_evalAtom c s
      res = mod (length listDist) 3
  in case res of
    0 -> Just $ [(head listDist, 1)]
    1 -> let ind = fromIntegral $ floor ((fromIntegral $ length listDist)/2)
         in Just $ [(listDist!!ind, 1)]  
    2 -> Just $ [(last listDist, 1)]


-- --"Random" fair scheduler
-- --Mixes the definitions of randSchDebug and fairSchDebug
randNonBlockSchDebug :: SchDebug
randNonBlockSchDebug hist@(path,(c,s)) =
  let listDist = debug_evalAtom c s
      lenListDist = length listDist
      res = mod lenListDist 3
      new_listDist = map rmvHist listDist
  in case res of
    0 -> let head_listDist = head new_listDist
             tail_listDist = tail new_listDist
             (nextDist, i) = notRepeatedDebug hist (head_listDist, tail_listDist) 0
        in Just $ [(listDist!!i, 1)]
    1 -> let ind = fromIntegral $ floor ((fromIntegral $ length listDist)/2)
             ind_listDist = new_listDist!!ind
             rmv_ind_listDist = rmvInd ind new_listDist
             (nextDist,i) = notRepeatedDebug hist (ind_listDist, rmv_ind_listDist) ind
         in Just $ [(listDist!!i, 1)]  
    2 -> let last_listDist = last new_listDist
             init_listDist = take (lenListDist-1) new_listDist
             (nextDist, i) = notRepeatedDebug hist (last_listDist, init_listDist) (lenListDist-1)
         in Just $ [(listDist!!i, 1)]


randPickDebug :: ProbPath -> [[(Either LMem (C, LMem), Double)]] -> (([(Either LMem (C,LMem), Double)],[[(Either LMem (C,LMem), Double)]]), Int)
randPickDebug path [] = error "The list of possible steps should not be empty"
randPickDebug path listDist =
  let lenListDist = length listDist
      res = mod lenListDist 3
  in case res of
    0 -> ((head listDist, tail listDist), 0)
    1 -> let ind = fromIntegral $ floor ((fromIntegral $ length listDist)/2)
         in ((listDist!!ind, rmvInd ind listDist), ind)
    2 -> ((last listDist, take (lenListDist-1) listDist), lenListDist-1)

randNonBlockSchDebugII :: SchDebug
randNonBlockSchDebugII hist@(path,(c,s)) = do
  let listDist = debug_evalAtom c s
      new_listDist = map rmvHist listDist
      ((selected, rest), ind) = randPickDebug hist new_listDist
      (nextDist, i) = randNotRepeatedDebug hist ((selected, rest), ind)
    in Just $ [(listDist!!i, 1)]

-- --Receives a probabilistic path, a tuple composed of the scheduled next step and all the unscheduled
-- --next steps, and returns a next step that has not occur yet; in the case where all the possible
-- --next steps, scheduled or unscheduled, already appeared in the probabilistic path, the next step
-- --chosen is the last one unscheduled
randNotRepeatedDebug :: ProbPath -> (([(Either LMem (C,LMem), Double)],[[(Either LMem (C,LMem), Double)]]), Int) -> ([(Either LMem (C,LMem), Double)], Int)
randNotRepeatedDebug _ ((next,[]), i) = (next,i)
randNotRepeatedDebug hist@(path,(c,s)) ((next, rest), i) = if (allLeft || null intersection) -- if next is composed only by final states or it did not appear in the history --  || allIn listComProbPath nextR
                                                           then (next, i) -- then return next
                                                           else randNotRepeatedDebug (path,(c,s)) (randPickDebug hist rest) -- else repeat the procedure by giving as next step the head of the list of the unscheduled next steps
  where allLeft = and $ map (\x -> isLeft x) (map fst next) --verifies if all the elements of next are final states
        nextL = lefts (map fst next) --collects the final states from next
        nextR = rights (map fst next) --collects the computations that did not finish
        nextRClassic = map (\(c,(sc,l,sq)) -> (c,sc)) nextR --collects the computations that did not finish only considering the command and the classical state
        listComProbPath = map fst path --list with all the commands and states that occurred during the computation
        listComProbPathClassic = map (\(c,(sc,l,sq)) -> (c,sc)) listComProbPath --list with all the commands and states that occurred during the computation only considering the command and the classical state
        intersection = intersect listComProbPathClassic nextRClassic --checks if the unfinished computations in nextR are present in the history



-- --Round-Robin scheduler that uses the length of "path" and the lenListDist to pick the next
-- --distribution to be evaluated
roundRobinSchDebug :: SchDebug
roundRobinSchDebug hist@(path,(c,s)) =
  let listDist = debug_evalAtom c s
      lenListDist = length listDist
      aux = (map (\(c,(sc,l,sq)) -> (c,sc)) . map fst) path
      lenAux = length aux
      ind = mod lenAux lenListDist
      nextDist = listDist !! ind
  in Just [(nextDist, 1.0)]
--END: definition of schedulers adapted to Debug--

