module Debug where

{-
This module has the goal to "debug", i.e. to give a trace of the states occurred during the computation
We do this resorting to the history of the computation, that the scheduler uses and produces
-}

--Haskell imports--
import Control.Monad.State
import Control.Monad.Trans 
import Control.Monad.Trans.Except
--import Control.Monad.Trans.Maybe
--import Control.Monad
--import Control.Monad.Identity
--import System.Random
import Data.Either
import Data.List
--import Data.Ord
--import Data.Complex
--import Data.Ratio
--import Data.Char
--import Data.Fixed
--import Data.Matrix
--Haskell imports--

--my imports--
import Syntax
import SmallStep
import Examples
import DistTMonad
import Beautify
import KStep
--my imports--


-- runDebugSch :: Sch -> C -> LMem -> Int -> [(ProbPath, [(Mem, Double)])]
-- runDebugSch sch c lmem k = let result = debugSch (sch,([],(c,lmem)),k)
--                            in map (\(prob_path, lmem) -> (prob_path, cleanL $ getDist lmem) ) result
--   where cleanL = map (\((sc,l,sq),p) -> ((sc,sq),p))

debugSch :: (Sch, ProbPath, Int) -> [Dist LMem]
debugSch (_, path, 0) = [Dist [(snd $ snd path, 0)]]   -- or whatever base case makes sense
debugSch (sch, l@(path, (c, s)), k) =
  case sch l of
    Nothing -> error "Scheduler undefined"
    Just convDist ->
      let ppL = [(s', p*q) | (dist, q) <- convDist, (s', p)   <- projL dist]
          next_eval = [((sch, (path ++ [((c, s), dist)], cs), k-1), p*q) | (dist, q) <- convDist, (cs, p)   <- projR dist]
          trans_step = concatMap (\(st, _) -> debugSch st) next_eval
      in trans_step
