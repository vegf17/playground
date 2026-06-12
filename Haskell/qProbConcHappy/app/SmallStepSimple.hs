module SmallStepSimple where

--Haskell imports--
import Control.Monad.State
import Control.Monad.Trans 
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer
--import System.Random
import Data.Either
import Data.List
import Data.Ord
import Data.Complex
import Data.Ratio
import Data.Char
import Data.Fixed
import Data.Matrix
--import Data.Random.Normal --cabal install normaldistribution
--import Data.Random -- :set -package random-fu
--import Data.Random.Distribution.Normal
--import Numeric (showFFloat)
--import Numeric.Probability.Distribution hiding (map, lift, filter)
  --for the use we make of probabilities, what we have defined is sufficient
  --later versions can make use of this package
--Haskell imports--

--import for graphics--
--import HistogramSem
--import Numeric.Probability.Game.Event
--import System.Exit
--import for graphics--

--my imports--
import Syntax
import SemAEBE
import Examples
import DistTMonad
import QuantumCalc
import Gates
import User_Gates
import Beautify
--my imports--


--START: small-step semantics--

-- StTQC C '=' S -> [[(Either S (C,S), Prob)]]
type StTQC a = StateT LMem (ExceptT LMem Dist) a --small

--Codifies the behavior of the small-step operational semantics
small :: C -> StTQC C
small Skip = StateT $ \s -> throwE s
small (Asg var e) = StateT $ \(sc,l,sq) -> throwE $ (changeSt var (bigStepExp e sc) sc, l, sq)
small (Reset q) = StateT $ \(sc,l,sq) -> throwE $ (sc, l, resetOpDen (qNumsAux q l) sq) 
small (U g qvar) = StateT $ \(sc,l,sq) -> throwE $ (sc, l, appGateOpDen g (qNums qvar l) sq) 
small (Meas (x,q)) = do
  (sc,l,sqt) <- get
  let sq = zeroIfSmallS sqt
      p0 = probOpDen 0 ((qNumsAux q l)) sq -- probability of measuring qubit q to be in state |0>
      p1 = probOpDen 1 ((qNumsAux q l)) sq -- probability of measuring qubit q to be in state |1>
      sq0 = stateMeasOpDen 0 ((qNumsAux q l)) sq -- state of the system of qubits after measuring qubit q in state |0>
      sq1 = stateMeasOpDen 1 ((qNumsAux q l)) sq -- state of the system of qubits after measuring qubit q in state |1>
      sc0 = changeSt x 0 sc -- assigning the value 0 to the variable x
      sc1 = changeSt x 1 sc -- assigning the value 1 to the variable x
  if (p0==0.0)
    then StateT $ \_ -> ExceptT $ Dist [(Left (sc1,l,sq1),p1)]
    else if (p1==0.0)
    then StateT $ \_ -> ExceptT $ Dist [(Left (sc0,l,sq0),p0)]
    else StateT $ \_ -> ExceptT $ Dist [(Left (sc0,l,sq0),p0), (Left (sc1,l,sq1),p1)]
small (Seq c1 c2) = do 
    s <- get 
    let cp = getDist $ runExceptT $ runStateT (small c1) s  -- :: [(Either LMem (Com, LMem), Prob)]
        seqC = compSeq cp c2
    StateT $ \_ -> ExceptT $ Dist seqC
small (IfC bExp c1 c2) = do
  (sc,l,sq) <- get
  let b = bigStepBExp bExp sc
  if b == True
    then StateT $ \_ -> ExceptT $ Dist [(Right (c1,(sc,l,sq)),1.0)]
    else StateT $ \_ -> ExceptT $ Dist [(Right (c2,(sc,l,sq)),1.0)]
small (Whl bExp c) = do
  (sc,l,sq) <- get
  let b = bigStepBExp bExp sc
  if b == True
    then StateT $ \_ -> ExceptT $ Dist [(Right (Seq c (Whl bExp c),(sc,l,sq)),1)]
    else StateT $ \_ -> ExceptT $ Dist [(Left (sc,l,sq),1)]


--Evaluates the results of the small-step operational semantics for a given command C and state s
runSmall :: C -> LMem -> [(Either Mem (C,Mem), Double)]
runSmall c s = rmvLAux $ getDist $ runExceptT $ runStateT (small c) s
--END: small-step semantics--


--START: debugger--
type Hist = [(String, LMem)]
type Debug a = StateT LMem (ExceptT LMem (WriterT Hist Dist)) a

debugSmall :: C -> Debug C
debugSmall Skip = StateT $ \s -> ExceptT $ WriterT $ Dist [((Left s, [(comToStr Skip, s)]), 1)]
debugSmall (Asg var e) = do
  (sc,l,sq) <- get
  let sc' = changeSt var (bigStepExp e sc) sc
      lmem = (sc',l,sq)
      strC = comToStr (Asg var e)
  StateT $ \_ -> ExceptT $ WriterT $ Dist [((Left lmem, [(strC, lmem)]), 1)]
debugSmall (Reset q) = do
  (sc,l,sq) <- get
  let sq' = resetOpDen (qNumsAux q l) sq
      lmem = (sc,l,sq')
      strC = comToStr (Reset q)
  StateT $ \_ -> ExceptT $ WriterT $ Dist [((Left lmem, [(strC, lmem)]), 1)]      
debugSmall (U g qvar) = do
  (sc,l,sq) <- get
  let sq' = appGateOpDen g (qNums qvar l) sq
      lmem = (sc,l,sq')
      strC = comToStr (U g qvar)
  StateT $ \_ -> ExceptT $ WriterT $ Dist [((Left lmem, [(strC, lmem)]), 1)]      
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
    then StateT $ \_ -> ExceptT $ WriterT $ Dist [((Left lmem1, [(str1, lmem1)]), p1)]
    else if (p1==0.0)
    then StateT $ \_ -> ExceptT $ WriterT $ Dist [((Left lmem0, [(str0, lmem0)]), p0)]
    else StateT $ \_ -> ExceptT $ WriterT $ Dist [((Left lmem0, [(str0, lmem0)]),p0), ((Left lmem1, [(str1, lmem1)]),p1)]  
debugSmall (Seq c1 c2) = do
  s <- get
  let cp = getDist $ runWriterT $ runExceptT $ runStateT (debugSmall c1) s -- :: [((Either LMem (C, LMem), Hist), Double)]
      seqC = compSeqDebug cp c2
  StateT $ \_ -> ExceptT $ WriterT $ Dist seqC
debugSmall (IfC bExp c1 c2) = do
  (sc,l,sq) <- get
  let b = bigStepBExp bExp sc
      lmem = (sc,l,sq)
  if b == True
    then StateT $ \_ -> ExceptT $ WriterT $ Dist [((Right (c1, lmem), [("If-TT", lmem)]), 1.0)]
    else StateT $ \_ -> ExceptT $ WriterT $ Dist [((Right (c2, lmem), [("If-FF", lmem)]),1.0)]
debugSmall (Whl bExp c) = do
  (sc,l,sq) <- get
  let b = bigStepBExp bExp sc
      lmem = (sc,l,sq)
  if b == True
    then StateT $ \_ -> ExceptT $ WriterT $ Dist [((Right (Seq c (Whl bExp c), lmem), [("Whl-TT", lmem)]), 1)]
    else StateT $ \_ -> ExceptT $ WriterT $ Dist [((Left lmem, [("Whl-FF", lmem)]), 1)]

run_debugSmall :: C -> LMem -> [((Either Mem (C, Mem), Hist), Double)]
run_debugSmall c lmem = rmvLAuxII $ getDist $ runWriterT $ runExceptT $ runStateT (debugSmall c) lmem


debug :: (C, LMem, Hist, Int) -> Dist (LMem, Hist)
debug (_, lmem, hist, 0) = Dist [((lmem, hist), 0)]
debug (c, lmem, hist, k) = do
  let cp = getDist $ runWriterT $ runExceptT $ runStateT (debugSmall c) lmem
      pL = [ ((s,hist++h),p) | ((s,h),p) <- projLDebug cp]
      pR = [ ((c, s, hist++h, k-1),p) | (((c,s),h),p) <- projRDebug cp]
      transStep = (Dist pR) >>= debug
  addDist transStep (Dist pL)


run_debug :: C -> LMem -> Int -> [((Mem, Hist), Double)]
run_debug c lmem k = f $ getDist $ debug (c, lmem, [], k)
  where f = map (\(((sc,l,sq),h),p) -> (((sc,sq),h),p))

show_debug :: C -> LMem -> Int -> String
show_debug c lmem k = print_debug $ run_debug c lmem k

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

--START: auxiliary functions--
--auxiliary functions for sequential and parallel composition
compSeq :: [(Either LMem (C,LMem), Double)] -> C -> [(Either LMem (C,LMem), Double)]
compSeq [] _ = []
compSeq ((Left s, p) : t) c = (Right (c,s), p) : compSeq t c
compSeq ((Right (cc, s),p) : t) c = (Right (Seq cc c, s), p) : compSeq t c

compSeqDebug :: [((Either LMem (C, LMem), Hist), Double)] -> C -> [((Either LMem (C, LMem), Hist), Double)]
compSeqDebug [] _ = []
compSeqDebug (((Left s, hist), p) : t) c = ((Right (c,s), hist), p) : compSeqDebug t c
compSeqDebug (((Right (cc, s), hist),p) : t) c = ((Right (Seq cc c, s), hist), p) : compSeqDebug t c


compParR :: [(Either LMem (C,LMem), Double)] -> C -> [(Either LMem (C,LMem), Double)]
compParR [] _ = []
compParR ((Left s, p) : t) c = (Right (c,s), p) : compParR t c
compParR ((Right (cc, s),p) : t) c = (Right (Par cc c, s), p) : compParR t c

compParL :: [(Either LMem (C,LMem), Double)] -> C -> [(Either LMem (C,LMem), Double)]
compParL [] _ = []
compParL ((Left s, p) : t) c = (Right (c,s), p) : compParL t  c
compParL ((Right (cc, s),p) : t) c = (Right (Par c cc, s), p) : compParL t c

inAtom :: [(Either LMem (C,LMem), Double)] -> [(Either LMem (C,LMem), Double)]
inAtom [] = []
inAtom ((Left s, p) : t) = (Left s, p) : inAtom t
inAtom ((Right (c, s),p) : t) = (Right (Atom c, s), p) : inAtom t

--auxiliary functions for small
projL :: [((Either a b), Double)] -> [(a, Double)]
projL [] = []
projL ((Left a,p):t) = (a,p) : projL t
projL ((Right b ,p):t) = projL t

projLDebug :: [((Either a b, Hist), Double)] -> [((a, Hist), Double)]
projLDebug [] = []
projLDebug (((Left a, h),p):t) = ((a,h),p) : projLDebug t
projLDebug (((Right b, _) ,p):t) = projLDebug t

projR :: [((Either a b), Double)] -> [(b, Double)]
projR [] = []
projR ((Left a, p):t) = projR t
projR ((Right b, p):t) = (b,p) : projR t

projRDebug :: [((Either a b, Hist), Double)] -> [((b, Hist), Double)]
projRDebug [] = []
projRDebug (((Left a, _),p):t) = projRDebug t
projRDebug (((Right b, h) ,p):t) = ((b,h),p) : projRDebug t

rmvL :: [[(Either LMem (a,LMem), Double)]] -> [[(Either Mem (a,Mem), Double)]]
rmvL [] = []
rmvL (h:t) = rmvLAux h : rmvL t

rmvLAux :: [(Either LMem (a,LMem), Double)] -> [(Either Mem (a,Mem), Double)]
rmvLAux [] = []
rmvLAux (((Left (sc,l,sq)),p) :t) = ((Left (sc,sq)),p) : rmvLAux t
rmvLAux (((Right(c, (sc,l,sq))),p) :t) = ((Right (c,(sc,sq))),p) : rmvLAux t

rmvLAuxII :: [((Either LMem (a, LMem), Hist), Double)] -> [((Either Mem (a, Mem), Hist), Double)]
rmvLAuxII [] = []
rmvLAuxII (((Left (sc,l,sq), hist), p):t) = ((Left (sc,sq), hist), p) : rmvLAuxII t
rmvLAuxII (((Right (c, (sc,l,sq)), hist), p) :t) = ((Right (c, (sc,sq)), hist), p) : rmvLAuxII t


--auxiliary function for runNStepSch
--Remove the linking function
-- rmvLII :: [([(LMem, Double)],Double)] -> [([(Mem, Double)],Double)]
-- rmvLII ll = [([ ((sc,sq),p) | ((sc,l,sq),p) <- dist],q) | (dist,q) <- ll]

rmvIOLII :: IO [([(LMem, Double)],Double)] -> IO [([(Mem, Double)],Double)]
rmvIOLII ioll = do
  ll <- ioll
  let l = [([ ((sc,sq),p) | ((sc,l,sq),p) <- dist],q) | (dist,q) <- ll]
  return l

--Remove the linking function 
rmvIOL :: IO [(LMem,Double)] -> IO [(Mem,Double)]
rmvIOL iol = do
  l <- iol -- [(LMem,Double)]
  let l' = map (\((a,b,c),p) -> ((a,c),p) ) l
  return l'

--auxiliary functions to add elements of DistT IO x
addDist :: (Eq x) => Dist x -> Dist x -> Dist x
addDist (Dist psi) (Dist phi) = Dist (addDistG psi phi)

addDistTIOG :: (Eq x) => DistT IO x -> DistT IO x -> DistT IO x
addDistTIOG (DistT iopsi) (DistT iophi) = do
  psi <- lift $ iopsi -- [(x,Double)]
  phi <- lift $ iophi -- [(x,Double)]
  DistT $ return (addDistG psi phi)

addDistG :: (Eq x) => [(x,Double)] -> [(x,Double)] -> [(x,Double)]
addDistG psi [] = psi
addDistG [] phi = phi
addDistG ((v1,p1):t1) phi = addDistAuxG (v1,p1) phi : addDistG t1 (rmvG v1 phi)

addDistAuxG :: (Eq x) => (x,Double) -> [(x,Double)] -> (x,Double)
addDistAuxG (v,p) [] = (v,p)
addDistAuxG (v1,p1) ((v2,p2):t) = if v1==v2
                                 then (v1,p1+p2)
                                 else addDistAuxG (v1,p1) t

rmvG :: (Eq x) => x -> [(x,Double)] -> [(x,Double)]
rmvG x [] = []
rmvG x ((y,p):t) = if x==y
                  then t
                  else (y,p) : rmvG x t

--auxiliary functions for sequential composition inside await
-- compSeqA :: [(Either LMem (CAwait,LMem), Double)] -> CAwait -> [(Either LMem (CAwait,LMem), Double)]
-- compSeqA [] _ = []
-- compSeqA ((Left s, p) : t) c = (Right (c,s), p) : compSeqA t c
-- compSeqA ((Right (cc, s),p) : t) c = (Right (SeqA cc c, s), p) : compSeqA t c    
--END: auxiliary functions--



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
