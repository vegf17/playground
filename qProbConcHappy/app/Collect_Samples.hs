module Collect_Samples where

import Control.Monad.State
import Control.Monad.Trans 
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Identity
import Data.Complex -- module for complex numbers
import Data.Matrix -- module for matrix datatype and operations
import Data.Char -- intToDigit function
import Data.List -- nub function
import Data.Ord -- sortBy, comparing functions
import Data.Maybe
-- import Graphics.Histogram
-- import qualified Graphics.Gnuplot.Frame.OptionSet as Opt
-- import qualified Graphics.Gnuplot.Frame.Option as Option
import System.Exit -- in order to get type ExitCode 
import System.Random.MWC.Probability -- module to help sampling from a discrete probability
                                     -- distribution

import Syntax
import SmallStep
import Beautify
import DistTMonad
import KStep
import Examples
import Com


-- ((ProgramName, NumberSamples, NumberKStep), (C,StC,L,StQ))
type ProgInfoFile = ((String, Int, Int), (C,StC,L,StQ))
type ListProgInfoFile = [ProgInfoFile]

---START: Collecting Samples considering incomplete computation---
--(ProgramName,[(SampleNumber, (StC, FinalComp ,StQ))])
-- FinalComp = True (represents the state of a finished computation)
-- FinalComp = False (represents the state of an unfinished computation)
type SampleCollection = (String, L, [(Int, Bool, Mem)])
type ListSampleCollection = [SampleCollection]


--START: sampling from the k-step--
-- The strategy is to run the k-step semantics, obtain the valuation and sample from it
-- By doing this, the same scheduler is used in the histogram generation and in the k-step semantic;
-- this way, both outputs are more related to each other
collectListSamples :: ListProgInfoFile -> Sch -> IO ListSampleCollection
collectListSamples [] _ = return []
collectListSamples (h:t) sch = do
  sample_prog <- collectSamples h sch
  sample_prog' <- collectListSamples t sch
  return (sample_prog : sample_prog')

-- -- ProgInfoFile  -> Sch -> SampleCollection
collectSamples :: ProgInfoFile -> Sch -> IO SampleCollection
collectSamples ((name, 0, k),(c, sc, l, sq)) sch = return (name, l, [])
collectSamples ((name, n_samples, k),(c, sc, l, sq)) sch = do
  maybe_st <- sampleMem (c, (sc,l,sq), k) sch -- Maybe (Mem,Double)
  (_, _, samples) <- collectSamples ((name, n_samples-1, k),(c, sc, l, sq)) sch
  if maybe_st == Nothing
    then return (name, l, samples)
    else do
    let (b, (sc', sq')) = fromJust maybe_st
    return (name, l, (n_samples, b, (sc', limitPrecisionS 5 sq')): samples)


--executes kStepSch and samples from the obtained valuation
sampleMem :: (C,LMem,Int) -> Sch -> IO (Maybe (Bool, Mem))
sampleMem (c, (sc,l,sq), 0) _ = return Nothing
sampleMem (c, s, k) sch = do
  gen <- createSystemRandom
  let dist_valL = kStepSch (sch, ([], (c, s)), k) -- Dist LMem
      valL = getDist dist_valL -- [(LMem, Double)]
      val = map (\((sc,l,sq),p) -> (p,(sc,sq))) valL -- [(Double, Mem)]
      fill_val = fillVal val -- [(Double, (Bool,Mem))]
      dist_val = discrete fill_val -- Prob m (Bool, Mem)
  st <- sample dist_val gen -- (Bool,Mem)
  return $ Just st

--if the sum of the weights do not add up to one, then we fill the gap
--the gap is caused by unfinished computations
fillVal :: [(Double, Mem)] -> [(Double, (Bool, Mem))]
fillVal val_mem = let weights = map fst val_mem
                      sum_weights = sum weights
                  in if sum_weights == 1
                     then map (\(p, mem) -> (p, (True, mem))) val_mem
                     else let zeros = filter (\(p,_) -> p==0) val_mem
                              val_mem_non_zeros = filter (\e -> if elem e zeros then False else True) val_mem
                              mem = snd $ head zeros
                              p = 1-sum_weights
                              val_mem_non_zeros_true = map (\(p, mem) -> (p, (True, mem))) val_mem_non_zeros
                          in (p, (False, mem)) : val_mem_non_zeros_true
  

showCollectSamples :: SampleCollection -> String
showCollectSamples (name_prog, l, samples) = name_prog ++ ":\n " ++ show(l) ++ "\n " ++ showSamples samples ++ "\n "

showSamples :: [(Int, Bool, (StC, StQ))] -> String
showSamples [] = ""
showSamples [(n_smp, _, (sc, sq))] = show(n_smp) ++ "\n " ++ (memToString (sc, sq)) ++ "\n "
showSamples ((n_smp, _, (sc, sq)):t) = (showSamples t) ++ show(n_smp) ++ "\n " ++ (memToString (sc, sq)) ++ "\n "


-- testCollectSamples :: String -> String -> String -> Sch -> Int -> Int -> IO ()
testCollectSamples str_c str_sc str_sq sch n_samples k = do
  let prog_name = "Test"
      c = testC str_c
      sc = testStC str_sc
      (l,sq) = testStQ str_sq
  (_, _, lres) <- collectSamples ((prog_name, n_samples, k),(c,sc,l,sq)) sch
  putStrLn $ prog_name ++ "\n " ++ show(l) ++ "\n  " ++ (showSamples lres)

testsampling :: String -> String -> String -> Sch -> Int -> IO (Maybe (Bool, Mem))
testsampling str_c str_stc str_lstq sch k = do
  let c = testC str_c
      sc = testStC str_stc
      (l,sq) = testStQ str_lstq
  sampleMem (c, (sc, l, sq), k) sch

prettyTesting :: String -> String -> String -> Sch -> Int -> IO ()
prettyTesting str_c str_stc str_lstq sch k = do
  res <- testsampling str_c str_stc str_lstq sch k -- Maybe (Bool, Mem)
  if isJust res
    then putStrLn $ memToString $ snd $ fromJust res
    else putStrLn $ show res
---END: Collecting Samples considering incomplete computation---



-- ---START: Collecting Samples discarding incomplete computation---
-- --(ProgramName,[(SampleNumber, (StC, StQ))])
-- type SampleCollection = (String, L, [(Int, Mem)])
-- type ListSampleCollection = [SampleCollection]


-- --START: sampling from the k-step--
-- -- The strategy is to run the k-step semantics, obtain the valuation and sample from it
-- -- By doing this, the same scheduler is used in the histogram generation and in the k-step semantic;
-- -- this way, both outputs are more related to each other
-- collectListSamples :: ListProgInfoFile -> Sch -> IO ListSampleCollection
-- collectListSamples [] _ = return []
-- collectListSamples (h:t) sch = do
--   sample_prog <- collectSamples h sch
--   sample_prog' <- collectListSamples t sch
--   return (sample_prog : sample_prog')

-- -- -- ProgInfoFile  -> Sch -> SampleCollection
-- collectSamples :: ProgInfoFile -> Sch -> IO SampleCollection
-- collectSamples ((name, 0, k),(c, sc, l, sq)) sch = return (name, l, [])
-- collectSamples ((name, n_samples, k),(c, sc, l, sq)) sch = do
--   maybe_st <- sampleMem (c, (sc,l,sq), k) sch -- Maybe (Mem,Double)
--   (_, _, samples) <- collectSamples ((name, n_samples-1, k),(c, sc, l, sq)) sch
--   if maybe_st == Nothing
--     then return (name, l, samples)
--     else do
--     let (sc', sq') = fromJust maybe_st
--     return (name, l, (n_samples, (sc', limitPrecisionS 5 sq')): samples)


-- --nstep where after a small-step a configuration is chosen to be executed next
-- sampleMem :: (C,LMem,Int) -> Sch -> IO (Maybe Mem)
-- sampleMem (c, (sc,l,sq), 0) _ = return Nothing
-- sampleMem (c, s, k) sch = do
--   gen <- createSystemRandom
--   let (dist_valL) = kStepSch (sch, ([], (c, s)), k) -- Dist LMem
--       valL = getDist dist_valL -- [(LMem, Double)]
--       val = discrete $ map (\((sc,l,sq),p) -> (p,(sc,sq))) valL -- [(Mem, Double)]
--   st <- sample val gen
--   return $ Just st

-- showCollectSamples :: SampleCollection -> String
-- showCollectSamples (name_prog, l, samples) = name_prog ++ ":\n " ++ show(l) ++ "\n " ++ showSamples samples ++ "\n "

-- showSamples :: [(Int, (StC, StQ))] -> String
-- showSamples [] = ""
-- showSamples [(n_smp, (sc, sq))] = show(n_smp) ++ "\n " ++ (memToString (sc, sq)) ++ "\n "
-- showSamples ((n_smp, (sc, sq)):t) = (showSamples t) ++ show(n_smp) ++ "\n " ++ (memToString (sc, sq)) ++ "\n "


-- testCollectSamples :: String -> String -> String -> Sch -> Int -> Int -> IO ()
-- testCollectSamples str_c str_sc str_sq sch n_samples k = do
--   let prog_name = "Test"
--       c = testC str_c
--       sc = testStC str_sc
--       (l,sq) = testStQ str_sq
--   (_, _, lres) <- collectSamples ((prog_name, n_samples, k),(c,sc,l,sq)) sch
--   putStrLn $ prog_name ++ "\n " ++ show(l) ++ "\n  " ++ (showSamples lres)

-- testsampling :: String -> String -> String -> Sch -> Int -> IO (Maybe Mem)
-- testsampling str_c str_stc str_lstq sch k = do
--   let c = testC str_c
--       sc = testStC str_stc
--       (l,sq) = testStQ str_lstq
--   sampleMem (c, (sc, l, sq), k) sch

-- prettyTesting :: String -> String -> String -> Sch -> Int -> IO ()
-- prettyTesting str_c str_stc str_lstq sch k = do
--   res <- testsampling str_c str_stc str_lstq sch k -- Maybe Mem
--   if isJust res
--     then putStrLn $ memToString $ fromJust res
--     else putStrLn $ show res
-- ---END: Collecting Samples discarding incomplete computation---
