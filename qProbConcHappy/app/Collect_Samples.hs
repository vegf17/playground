module Collect_Samples where

import Control.Monad.State
import Control.Monad.Trans 
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Identity
import System.Random
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
import Numeric.Probability.Game.Event

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

--(ProgramName,[(SampleNumber, (StC, StQ))])
type SampleCollection = (String, [(Int, (StC, StQ))])
type ListSampleCollection = [SampleCollection]


--START: sampling from the k-step--
-- The strategy is to run the k-step semantics, obtain the valuation and sample from it
-- By doing this, the same scheduler is used in the histogram generation and in the k-step semantic;
-- this way, both outputs are more related to each other
-- collectListSamples :: ListProgInfoFile -> Sch -> IO ListSampleCollection
-- collectListSamples [] _ = return []
-- collectListSamples (h:t) sch = do
--   sample_prog <- collectSamples h sch
--   sample_prog' <- collectListSamples t sch
--   return (sample_prog : sample_prog')

-- -- ProgInfoFile  -> Sch -> SampleCollection
collectSamples :: ProgInfoFile -> Sch -> IO SampleCollection
collectSamples ((name, 0, k),(c, sc, l, sq)) sch = return (name, [])
collectSamples ((name, n_samples, k),(c, sc, l, sq)) sch = do
  maybe_st <- sample (c, (sc,l,sq), k) sch -- Maybe (Mem,Double)
  (_, samples) <- collectSamples ((name, n_samples-1, k),(c, sc, l, sq)) sch
  if maybe_st == Nothing
    then return (name, samples)
    else do
    let (sc,sq) = fromJust maybe_st
    return (name, (n_samples, (sc, limitPrecisionS 5 sq)): samples)


--nstep where after a small-step a configuration is chosen to be executed next
sample :: (C,LMem,Int) -> Sch -> IO (Maybe Mem)
sample (c, (sc,l,sq), 0) _ = return Nothing
sample (c, s, k) sch = do
  let (dist_valL) = kStepSch (sch, ([], (c, s)), k) -- Dist LMem
      valL = getDist dist_valL
      val = map (\((sc,l,sq),p) -> ((sc,sq),p)) valL
      list = zip [i | i <-[0 .. length val]] [p | (a,p) <- val]
      st = map fst val
  n <- enact $ makeEventProb list
  return $ Just (st!!n)


testsampling :: String -> String -> String -> Sch -> Int -> IO (Maybe Mem)
testsampling str_c str_stc str_lstq sch k = do
  let c = testC str_c
      sc = testStC str_stc
      (l,sq) = testStQ str_lstq
  sample (c, (sc, l, sq), k) sch 

prettyTesting :: String -> String -> String -> Sch -> Int -> IO ()
prettyTesting str_c str_stc str_lstq sch k = do
  res <- testsampling str_c str_stc str_lstq sch k -- Maybe Mem
  if isJust res
    then putStrLn $ memToString $ fromJust res
    else putStrLn $ show res
  
