module KStep where

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
--my imports--


--START: k-step--
--Scheduler: ProbPath --> V(V(S + (C x S))) + {*}  ==> ProbPath --> Maybe V(V(S + (C x S)))
type Sch = ProbPath -> Maybe [([(Either LMem (C, LMem), Double)], Double)]

--Returns the result of the k-step operational semantics for a given scheduler Sch, command C, state
--s, and empty history
runKStepSch :: Sch -> C -> LMem -> Int -> [(Mem, Double)]
runKStepSch sch c lmem k = cleanL $ getDist $ kStepSch (sch,([],(c,lmem)),k)
  where cleanL = map (\((sc,l,sq),p) -> ((sc,sq),p))

--improved display of the results of runNStepSch
--for example quantum states are shown in bra-ket notation
showKStepSch :: Sch -> C -> LMem -> Int -> IO()
showKStepSch sch c s n = let s' = runKStepSch sch c s n -- [(Mem, Double)]
                         in putStrLn $ showProbMemList s'
                         --in putStrLn $ showProbMemList $ (limitPrecAux 5 s')

--codifies the k-step semantics
kStepSch :: (Sch, ProbPath, Int) -> Dist LMem
kStepSch (_, path, 0) = Dist [(snd $ snd path, 0)]
kStepSch (sch, l@(path, (c, s)), k) =
  case (sch l) of
    Nothing -> error "Scheduler undefined"
    Just convDist -> do 
      let ppL = [ [ (s, p*q) | (s, p) <- (projL dist)] | (dist, q) <- convDist] -- [([(S, Double)], Double)]
          next_eval = [[((sch, (path ++ [((c, s), dist)], cs), k-1), p*q) | (cs, p) <- (projR dist)] | (dist, q) <- convDist]
          transStep = (Dist $ concat next_eval) >>= kStepSch
      addDist transStep (Dist $ concat ppL)
--END: k-step--


--START: priority for Atom--
--prioritize the evaluation of Atom commands
evalAtom :: C -> LMem -> [[(Either LMem (C, LMem), Double)]]
evalAtom (Atom c) s = runDistT $ runExceptT $ runStateT (small (Atom c)) s
evalAtom (P prob c1 c2) s = let p = fromRational prob
                                listDist1 = evalAtom c1 s
                                listDist2 = evalAtom c2 s
                                pc1 = [[(c,p*p')| (c,p') <- lcc1] | lcc1 <- listDist1]
                                pc2 = [[(c,(1-p)*p')| (c,p') <- lcc2] | lcc2 <- listDist2]
                            in  concat [[ec1++ec2 | ec2 <- pc2] | ec1 <- pc1]
evalAtom (Seq c1 c2) s =
  case (nextAtom c1) of
    True -> let listDist = evalAtom c1 s
            in [compSeq dist c2 | dist <- listDist]
    False -> runDistT $ runExceptT $ runStateT (small (Seq c1 c2)) s
evalAtom (Or c1 c2) s =
  case (nextAtom c1, nextAtom c2) of
    (True, False) -> evalAtom c1 s
    (False, True) -> evalAtom c2 s
    otherwise -> runDistT $ runExceptT $ runStateT (small (Or c1 c2)) s
evalAtom (Paral c1 c2) s =
  case (nextAtom c1, nextAtom c2) of
    (True, False) ->  let listDist = evalAtom c1 s
                      in [compParR dist c2 | dist <- listDist]
    (False, True) -> let listDist = evalAtom c2 s
                     in [compParL dist c1 | dist <- listDist]
    otherwise -> runDistT $ runExceptT $ runStateT (small (Paral c1 c2)) s
evalAtom c s = runDistT $ runExceptT $ runStateT (small c) s

{-
evalAtom :: C -> LMem -> [[(Either LMem (C, LMem), Double)]]
evalAtom (Atom c) s = runDistT $ runExceptT $ runStateT (small (Atom c)) s
evalAtom (P prob c1 c2) s = let p = fromRational prob
                                listDist1 = evalAtom c1 s
                                listDist2 = evalAtom c2 s
                                pc1 = [[(c,p*p')| (c,p') <- lcc1] | lcc1 <- listDist1]
                                pc2 = [[(c,(1-p)*p')| (c,p') <- lcc2] | lcc2 <- listDist2]
                            in  concat [[ec1++ec2 | ec2 <- pc2] | ec1 <- pc1]
evalAtom (Seq c1 c2) s = let listDist = evalAtom c1 s
                         in [compSeq dist c2 | dist <- listDist]
evalAtom (Or c1 c2) s =
  case (nextAtom c1) of
    False -> evalAtom c2 s
    True -> if (nextAtom c2 == False)
            then evalAtom c1 s
            else (evalAtom c1 s) ++ (evalAtom c2 s)
evalAtom (Paral c1 c2) s =
  case (nextAtom c1) of
    False -> let listDist = evalAtom c2 s
             in [compParL dist c1 | dist <- listDist]
    True -> if (nextAtom c2 == False)
            then let listDist = evalAtom c1 s
                 in [compParR dist c2 | dist <- listDist]
            else runDistT $ runExceptT $ runStateT (small (Paral c1 c2)) s
evalAtom c s = runDistT $ runExceptT $ runStateT (small c) s                 

-}

--see if the next command to be evaluated is an atom
nextAtom :: C -> Bool
nextAtom Skip = False
nextAtom (Asg var e) = False
nextAtom (Reset q) = False
nextAtom (U g qvar) = False
nextAtom (Meas (x,q)) = False
nextAtom (P prob c1 c2) = nextAtom c1 || nextAtom c2
nextAtom (Seq c1 c2) = nextAtom c1
nextAtom (Or c1 c2) = nextAtom c1 || nextAtom c2
nextAtom (Paral c1 c2) = nextAtom c1 || nextAtom c2
nextAtom (IfC bExp c1 c2) = False
nextAtom (Whl bExp c) = False
nextAtom (Await bExp c) = False
nextAtom (Atom c) = True
--END: priority for Atom--

--START: definition of schedulers without IO--
--undefined scheduler
undSch :: Sch
undSch _ = Nothing

--scheduler that chooses always the first element in the list of the possible transitions
initSch :: Sch
initSch (path,(c,s)) =
  --let listDist = runDistT $ runExceptT $ runStateT (small c) s -- [[(Either LMem (C,LMem), Double)]]
  let listDist = evalAtom c s -- [[(Either LMem (C,LMem), Double)]]
  in Just $ [(head listDist, 1)]

--scheduler that chooses always the last element in the list of the possible transitions
lastSch :: Sch
lastSch (path,(c,s)) =
  --let listDist = runDistT $ runExceptT $ runStateT (small c) s -- [[(Either LMem (C,LMem), Double)]]
  let listDist = evalAtom c s
  in Just $ [(last listDist, 1)]

--scheduler that chooses always the middle element in the list of the possible transitions
middleSch :: Sch
middleSch (path,(c,s)) =
  --let listDist = runDistT $ runExceptT $ runStateT (small c) s -- [[(Either LMem (C,LMem), Double)]]
  let listDist = evalAtom c s
      ind = fromIntegral $ floor ((fromIntegral $ length listDist)/2)
  in Just $ [(listDist!!ind, 1)]  

--uniform scheduler, which attributes the same probability to all the elements in the list of the
--possible transitions
uniSch :: Sch
uniSch (path,(c,s)) =
  --let listDist = runDistT $ runExceptT $ runStateT (small c) s -- [[(Either LMem (C,LMem), Double)]]
  let listDist = evalAtom c s
      len = fromIntegral $ length listDist
  in Just $ [(dist, 1/len) | dist <- listDist]


--non blocking scheduler that checks the history path to see if the current command and the classical state
--already occurred; if yes, then the scheduler tries another option
nonBlockSch :: Sch
nonBlockSch hist@(path,(c,s)) = 
  --let listDist = runDistT $ runExceptT $ runStateT (small c) s -- [[(Either LMem (C,LMem), Double)]]
  let listDist = evalAtom c s
      nextDist = notRepeated hist (head listDist, tail listDist)
  in Just $ [(nextDist, 1)]

--Receives a probabilistic path, a tuple composed of the scheduled next step and all the unscheduled
--next steps, and returns a next step that has not occur yet; in the case where all the possible
--next steps, scheduled or unscheduled, already appeared in the probabilistic path, the next step
--chosen is the last one unscheduled
notRepeated :: ProbPath -> ([(Either LMem (C,LMem), Double)],[[(Either LMem (C,LMem), Double)]]) -> [(Either LMem (C,LMem), Double)]
notRepeated _ (next,[]) = next
notRepeated (path,(c,s)) (next,(h:t)) = if (allLeft || null intersection ) -- if next is composed only by final states or it did not appear in the history --  || allIn listComProbPath nextR
                                        then next -- then return next
                                        else notRepeated (path,(c,s)) (h,t) -- else repeat the procedure by giving as next step the head of the list of the unscheduled next steps
  where allLeft = and $ map (\x -> isLeft x) (map fst next) --verifies if all the elements of next are final states
        nextL = lefts (map fst next) --collects the final states from next
        nextR = rights (map fst next) --collects the computations that did not finish
        nextRClassic = map (\(c,(sc,l,sq)) -> (c,sc)) nextR --collects the computations that did not finish only considering the command and the classical state
        listComProbPath = map fst path --list with all the commands and states that occurred during the computation
        listComProbPathClassic = map (\(c,(sc,l,sq)) -> (c,sc)) listComProbPath --list with all the commands and states that occurred during the computation only considering the command and the classical state
        intersection = intersect listComProbPathClassic nextRClassic --checks if the unfinished computations in nextR are present in the history


--limited fair scheduler, that keeps in memory the last 10 steps of the computation
limitnonBlockSch :: Sch
limitnonBlockSch hist@(path,(c,s)) = if (length path == 50)
                                 then nonBlockSch (tail path, (c,s))
                                 else nonBlockSch hist


--"Random" scheduler
--Uses the length of the possible steps to decide if the next configuration is the initial, the
--final, or the intermediate in the list of possible choices
--To do that, we do the following with lenListDist being the length of the possible steps:
--mod lenListDist 3 = 0 --> initial
--mod lenListDist 3 = 1 --> intermediate
--mod lenListDist 3 = 2 --> final
randSch :: Sch
randSch hist@(path,(c,s)) =
  let listDist = evalAtom c s
      res = mod (length listDist) 3
  in case res of
    0 -> Just $ [(head listDist, 1)]
    1 -> let ind = fromIntegral $ floor ((fromIntegral $ length listDist)/2)
         in Just $ [(listDist!!ind, 1)]  
    2 -> Just $ [(last listDist, 1)]


--"Random" fair scheduler
--Mixes the definitions of randSch and fairSch
randNonBlockSch :: Sch
randNonBlockSch hist@(path,(c,s)) =
  let listDist = evalAtom c s
      lenListDist = length listDist
      res = mod lenListDist 3
  in case res of
    0 -> let nextDist = notRepeated hist (head listDist, tail listDist)
        in Just $ [(nextDist, 1)]
    1 -> let ind = fromIntegral $ floor ((fromIntegral $ length listDist)/2)
             nextDist = notRepeated hist (listDist!!ind, rmvInd ind listDist)
         in Just $ [(nextDist, 1)]  
    2 -> let nextDist = notRepeated hist (last listDist, take (lenListDist-1) listDist)
         in Just $ [(nextDist, 1)]

rmvInd :: Int -> [a] -> [a]
rmvInd _ [] = []
rmvInd ind (h:t)
  | ind==0 = t
  | otherwise = h : rmvInd (ind-1) t


randPick :: ProbPath -> [[(Either LMem (C, LMem), Double)]] -> ([(Either LMem (C,LMem), Double)],[[(Either LMem (C,LMem), Double)]])
randPick path [] = error "The list of possible steps should not be empty"
randPick path listDist =
  let lenListDist = length listDist
      res = mod lenListDist 3
  in case res of
    0 -> (head listDist, tail listDist)
    1 -> let ind = fromIntegral $ floor ((fromIntegral $ length listDist)/2)
         in (listDist!!ind, rmvInd ind listDist)
    2 -> (last listDist, take (lenListDist-1) listDist)

randNonBlockSchII :: Sch
randNonBlockSchII hist@(path,(c,s)) = do
  let listDist = evalAtom c s
      (selected, rest) = randPick hist listDist
      nextDist = randNotRepeated hist (selected, rest)
    in Just $ [(nextDist, 1)]

--Receives a probabilistic path, a tuple composed of the scheduled next step and all the unscheduled
--next steps, and returns a next step that has not occur yet; in the case where all the possible
--next steps, scheduled or unscheduled, already appeared in the probabilistic path, the next step
--chosen is the last one unscheduled
randNotRepeated :: ProbPath -> ([(Either LMem (C,LMem), Double)],[[(Either LMem (C,LMem), Double)]]) -> [(Either LMem (C,LMem), Double)]
randNotRepeated _ (next,[]) = next
randNotRepeated hist@(path,(c,s)) (next, rest) = if (allLeft || null intersection ) -- if next is composed only by final states or it did not appear in the history --  || allIn listComProbPath nextR
                                            then next -- then return next
                                            else randNotRepeated (path,(c,s)) (randPick hist rest) -- else repeat the procedure by giving as next step the head of the list of the unscheduled next steps
  where allLeft = and $ map (\x -> isLeft x) (map fst next) --verifies if all the elements of next are final states
        nextL = lefts (map fst next) --collects the final states from next
        nextR = rights (map fst next) --collects the computations that did not finish
        nextRClassic = map (\(c,(sc,l,sq)) -> (c,sc)) nextR --collects the computations that did not finish only considering the command and the classical state
        listComProbPath = map fst path --list with all the commands and states that occurred during the computation
        listComProbPathClassic = map (\(c,(sc,l,sq)) -> (c,sc)) listComProbPath --list with all the commands and states that occurred during the computation only considering the command and the classical state
        intersection = intersect listComProbPathClassic nextRClassic --checks if the unfinished computations in nextR are present in the history



--Round-Robin scheduler that uses the length of "path" and the lenListDist to pick the next
--distribution to be evaluated
roundRobinSch :: Sch
roundRobinSch hist@(path,(c,s)) =
  let listDist = evalAtom c s
      lenListDist = length listDist
      aux = (map (\(c,(sc,l,sq)) -> (c,sc)) . map fst) path
      lenAux = length aux
      ind = mod lenAux lenListDist
      nextDist = listDist !! ind
  in Just [(nextDist, 1.0)]


-- roundRobinSch :: Sch
-- roundRobinSch hist@(path,(c,s)) =
--   let listDist = evalAtom c s
--       atomList = atomPrio listDist
--   in  case (null atomList) of
--         True ->  let lenListDist = length listDist
--                      aux = (map (\(c,(sc,l,sq)) -> (c,sc)) . map fst) path
--                      lenAux = length aux
--                      ind = mod lenAux lenListDist
--                      nextDist = listDist !! ind
--                  in Just [(nextDist, 1.0)]
--         False -> Just [(head $ atomList, 1.0)]

-- --Priority to atom
-- atomPrio :: [[(Either LMem (C, LMem), Double)]] -> [[(Either LMem (C, LMem), Double)]]
-- atomPrio listDist = [dist | dist <- listDist, atomPrioAux dist == True]

-- atomPrioAux :: [(Either LMem (C, LMem), Double)] -> Bool
-- atomPrioAux dist = let listC =  map fst $ rights $ map fst dist
--                    in or [nextAtom c | c <- listC]
--END: definition of schedulers without IO--
