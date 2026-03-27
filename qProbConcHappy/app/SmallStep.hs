module SmallStep where

--Haskell imports--
import Control.Monad.State
import Control.Monad.Trans 
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Identity
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
type StTQC a = StateT LMem (ExceptT LMem (DistT [])) a --small

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
    then StateT $ \_ -> ExceptT $ DistT $ [[(Left (sc1,l,sq1),p1)]]
    else if (p1==0.0)
    then StateT $ \_ -> ExceptT $ DistT $ [[(Left (sc0,l,sq0),p0)]]
    else StateT $ \_ -> ExceptT $ DistT $ [[(Left (sc0,l,sq0),p0), (Left (sc1,l,sq1),p1)]]
small (P prob c1 c2) = do
  s <- get
  let p = fromRational prob
      cc1 = runDistT $ runExceptT $ runStateT (small c1) s  -- :: [[(Either S (Com, S), Prob)]]
      cc2 = runDistT $ runExceptT $ runStateT (small c2) s  -- :: [[(Either S (Com, S), Prob)]]
      pc1 = [[(c,p*p')| (c,p') <- lcc1] | lcc1 <- cc1]
      pc2 = [[(c,(1-p)*p')| (c,p') <- lcc2] | lcc2 <- cc2]
      pc1c2 = concat [[ec1++ec2 | ec2 <- pc2] | ec1 <- pc1]
  StateT $ \_ -> ExceptT $ DistT $ pc1c2    
small (Seq c1 c2) = do 
    s <- get 
    let cp = runDistT $ runExceptT $ runStateT (small c1) s  -- :: [[(Either LMem (Com, LMem), Prob)]]
        seqC = [compSeq dist c2| dist <- cp]
    StateT $ \_ -> ExceptT $ DistT $ seqC
small (Or c1 c2) = do 
    s <- get 
    let cp1 = runDistT $ runExceptT $ runStateT (small c1) s -- :: [[(Either LMem (Com, LMem), Prob)]]
        cp2 = runDistT $ runExceptT $ runStateT (small c2) s -- :: [[(Either LMem (Com, LMem), Prob)]]
    StateT $ \_ -> ExceptT $ DistT $ (cp1++cp2) 
small (Par c1 c2) = do 
    s <- get 
    let cp1 = runDistT $ runExceptT $ runStateT (small c1) s -- :: [[(Either LMem (Com, LMem), Prob)]]
        cp2 = runDistT $ runExceptT $ runStateT (small c2) s -- :: [[(Either LMem (Com, LMem), Prob)]]
        par1 = [compParR dist c2| dist <- cp1]
        par2 = [compParL dist c1| dist <- cp2]
    StateT $ \_ -> ExceptT $ DistT $ par1++par2
small (IfC bExp c1 c2) = do
  (sc,l,sq) <- get
  let b = bigStepBExp bExp sc
  if b == True
    then StateT $ \_ -> ExceptT $ DistT $ [[(Right (c1,(sc,l,sq)),1.0)]]
    else StateT $ \_ -> ExceptT $ DistT $ [[(Right (c2,(sc,l,sq)),1.0)]]
small (Whl bExp c) = do
  (sc,l,sq) <- get
  let b = bigStepBExp bExp sc
  if b == True
    then StateT $ \_ -> ExceptT $ DistT $ [[(Right (Seq c (Whl bExp c),(sc,l,sq)),1)]]
    else StateT $ \_ -> ExceptT $ DistT $ [[(Left (sc,l,sq),1)]]
small (Await bExp c) = do
  (sc,l,sq) <- get
  let b = bigStepBExp bExp sc
  if b == False
    then StateT $ \_ -> ExceptT $ DistT $ [[(Right (Await bExp c, (sc,l,sq)),1)]]
    else StateT $ \_ -> ExceptT $ DistT $ [[(Right (Atom c, (sc,l,sq)), 1)]]
small (Atom c) = do
  s <- get
  let cp = runDistT $ runExceptT $ runStateT (small c) s
      atom = [inAtom dist | dist <- cp]
  StateT $ \_ -> ExceptT $ DistT $ atom


--Evaluates the results of the small-step operational semantics for a given command C and state s
runSmall :: C -> LMem -> [[(Either Mem (C,Mem), Double)]]
runSmall c s = rmvL $ runDistT $ runExceptT $ runStateT (small c) s
--END: small-step semantics--




-- --START: semantics for the await command--

-- --Type definition for the semantics of the await command
-- --StTP C '=' S -> [(Either S (C,S), Prob)]
-- type StTP a = StateT LMem (ExceptT LMem Prob) a

-- --Codifies the behavior of the small-step operational semantics for commands inside await
-- smallAwait :: CAwait -> StTP CAwait
-- smallAwait SkipA = StateT $ \s -> throwE s
-- smallAwait (AsgA var e) = StateT $ \(sc,l,sq) -> throwE $ (changeSt var (bigStepExp e sc) sc, l, sq)
-- smallAwait (ResetA q) = StateT $ \(sc,l,sq) -> throwE $ (sc, l, resetOpDen (qNumsAux q l) sq) 
-- smallAwait (UA g qvar) = StateT $ \(sc,l,sq) -> throwE $ (sc, l, appGateOpDen g (qNums qvar l) sq)
-- smallAwait (MeasA (x,q)) = do
--   (sc,l,sqt) <- get
--   let sq = zeroIfSmallS sqt
--       p0 = probOpDen 0 ((qNumsAux q l)) sq -- probability of measuring qubit q to be in state |0>
--       p1 = probOpDen 1 ((qNumsAux q l)) sq -- probability of measuring qubit q to be in state |1>
--       sq0 = stateMeasOpDen 0 ((qNumsAux q l)) sq -- state of the system of qubits after measuring qubit q in state |0>
--       sq1 = stateMeasOpDen 1 ((qNumsAux q l)) sq -- state of the system of qubits after measuring qubit q in state |1>
--       sc0 = changeSt x 0 sc -- assigning the value 0 to the variable x
--       sc1 = changeSt x 1 sc -- assigning the value 1 to the variable x
--   if (p0==0.0)
--     then StateT $ \_ -> ExceptT $ Dist $ [(Left (sc1,l,sq1),p1)]
--     else if (p1==0.0)
--     then StateT $ \_ -> ExceptT $ Dist $ [(Left (sc0,l,sq0),p0)]
--     else StateT $ \_ -> ExceptT $ Dist $ [(Left (sc0,l,sq0),p0), (Left (sc1,l,sq1),p1)]
-- smallAwait (SeqA c1 c2) = do 
--     s <- get 
--     let cp = getDist $ runExceptT $ runStateT (smallAwait c1) s  -- :: [(Either LMem (Com, LMem), Rational)]
--         seqC = compSeqA cp c2
--     StateT $ \_ -> ExceptT $ Dist $ seqC
-- smallAwait (IfCA bExp c1 c2) = do
--   (sc,l,sq) <- get
--   let b = bigStepBExp bExp sc
--   if b == True
--     then StateT $ \_ -> ExceptT $ Dist $ [(Right (c1,(sc,l,sq)),1.0)]
--     else StateT $ \_ -> ExceptT $ Dist $ [(Right (c2,(sc,l,sq)),1.0)]

-- --Codifies the behavior of the big-step semantics for await commands
-- bigAwait :: (CAwait,LMem) -> Dist LMem
-- bigAwait (c,s) = do
--   let small = getDist $ runExceptT $ runStateT (smallAwait c) s --[(Either LMem (CAwait, LMem), Double)]
--       sts = projL small -- [(LMem, Double)]
--       confs = projR small -- [((CAwait, LMem), Double)]
--       result = (Dist $ confs) >>= bigAwait
--   Dist $ addDistG sts (getDist result)

-- --improved display of the results of bigAwait
-- --for example quantum states are shown in bra-ket notation
-- showBigAwait :: CAwait -> LMem -> IO()
-- showBigAwait c s = putStrLn $ showProbMemList $ f $ getDist $ bigAwait (c,s)
--   where f :: [((LMem, Double))] -> [(Mem, Double)]
--         f l = [((sc,sq),p) | ((sc,l,sq),p) <- l]
-- --END: semantics for the await command--


--START: auxiliary functions--
--auxiliary functions for sequential and parallel composition
compSeq :: [(Either LMem (C,LMem), Double)] -> C -> [(Either LMem (C,LMem), Double)]
compSeq [] _ = []
compSeq ((Left s, p) : t) c = (Right (c,s), p) : compSeq t c
compSeq ((Right (cc, s),p) : t) c = (Right (Seq cc c, s), p) : compSeq t c

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

projR :: [((Either a b), Double)] -> [(b, Double)]
projR [] = []
projR ((Left a, p):t) = projR t
projR ((Right b, p):t) = (b,p) : projR t

rmvL :: [[(Either LMem (a,LMem), Double)]] -> [[(Either Mem (a,Mem), Double)]]
rmvL [] = []
rmvL (h:t) = rmvLAux h : rmvL t

rmvLAux :: [(Either LMem (a,LMem), Double)] -> [(Either Mem (a,Mem), Double)]
rmvLAux [] = []
rmvLAux (((Left (sc,l,sq)),p) :t) = ((Left (sc,sq)),p) : rmvLAux t
rmvLAux (((Right(c, (sc,l,sq))),p) :t) = ((Right (c,(sc,sq))),p) : rmvLAux t

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



