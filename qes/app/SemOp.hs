module SemOp where

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

import Data.Monoid as Monoid
import QioMonad.QIO.QioSyn
import QioMonad.QIO.Qdata
import QioMonad.QIO.QioClass
import QioMonad.QIO.Qio
import QioMonad.QIO.Heap
import QioMonad.QIO.Qdata
import QioMonad.QIO.Qio
import QioMonad.QIO.QioClass
import QioMonad.QIO.QioSyn
import QioMonad.QIO.Vec
import QioMonad.QIO.VecEq
import QioMonad.QIO.QArith


import Syntax
import Com

type LL = [(QVar, Qbit)]
type StQIO a = StateT LL (ExceptT (QIO ()) QIO) a

-- given a concurrent program, it returns a sequential program based on a defined scheduling strategy 
type Sch = C -> C


initSch :: Sch
initSch (Skip) = Skip
initSch (Syntax.U g qvars) = Syntax.U g qvars
initSch (Seq c1 c2) = let c1' = initSch c1
                          c2' = initSch c2
                      in Seq c1' c2'
initSch (Par c1 c2) = let c1' = initSch c1
                          c2' = initSch c2
                      in Seq c1' c2'
initSch (Syntax.Meas q c1 c2) = let c1' = initSch c1
                                    c2' = initSch c2
                                in Syntax.Meas q c1' c2'
initSch (Whl q c) = let c' = initSch c
                    in Whl q c'                       


lastSch :: Sch
lastSch (Skip) = Skip
lastSch (Syntax.U g qvars) = Syntax.U g qvars
lastSch (Seq c1 c2) = let c1' = lastSch c1
                          c2' = lastSch c2
                      in Seq c1' c2'
lastSch (Par c1 c2) = let c1' = lastSch c1
                          c2' = lastSch c2
                      in Seq c2' c1'
lastSch (Syntax.Meas q c1 c2) = let c1' = lastSch c1
                                    c2' = lastSch c2
                                in Syntax.Meas q c1' c2'
lastSch (Whl q c) = let c' = lastSch c
                    in Whl q c'                       

-- this definition has a problem with programs that demand the verification of "Boolean conditions",
-- e.g.: Meas(q, sk, X[q]) || H[q]
fairSch :: Int -> Sch
fairSch _ Skip = Skip
fairSch _ (Syntax.U g qvars) = Syntax.U g qvars
fairSch n (Seq c1 c2) = let c1' = fairSch n c1
                            c2' = fairSch n c2
                        in Seq c1' c2'
fairSch n c@(Par c1 c2) = let n_par = howManyParC c
                              i = mod n n_par
                              list_c = parC c
                              sel_c = list_c!!i
                              (sel_inst, rest_c) = pickInst sel_c
                              list_c' = updList list_c sel_c rest_c
                              upd_parC = toParC list_c'
                          in Seq sel_inst (fairSch (n+1) upd_parC)
fairSch n (Syntax.Meas q c1 c2) = let c1' = fairSch n c1
                                      c2' = fairSch n c2
                                  in Syntax.Meas q c1' c2'
fairSch n (Whl q c) = let c' = fairSch n c
                      in Whl q c'


updList :: [C] -> C -> Maybe C -> [C]
updList [] _ _ = []
updList (h:t) sel_c rest_c = if h==sel_c
                             then case rest_c of
                                    Nothing -> t
                                    Just c' -> c':t
                             else h : updList t sel_c rest_c

--given a non-concurrent command, returns the first instruction and the remainder
pickInst :: C -> (C, Maybe C)
pickInst (Seq c1 c2) = let (inst, rest) = pickInst c1
                       in case rest of
                            Nothing -> (inst, Just c2)
                            Just c1' -> (inst, Just $ Seq c1' c2)
pickInst c = (c, Nothing)                            

--given a list of programs that were composed concurrently, compose the programs concurrently
toParC :: [C] -> C
toParC (c:[]) = c
toParC (h:t) = Par h (toParC t)

--creates a list of the programs that are composed concurrently
parC :: C -> [C]
parC (Par c1 c2) = let has_par_c1 = hasPar c1
                       has_par_c2 = hasPar c2
                   in if has_par_c1 == False
                      then c1 : parC c2
                      else if has_par_c2 == False
                           then let l1 = parC c1
                                in l1 ++ [c2]
                           else let l1 = parC c1
                                    l2 = parC c2
                                in l1++l2
parC c = [c]
                         

--given a list of programs composed concurrently, returns the n-th program
getC :: [C] -> Int -> Maybe C
getC [] _ = Nothing
getC (h:t) n = if n==1 then Just h else getC t (n-1)


hasPar :: C -> Bool
hasPar Skip = False
hasPar (Syntax.U _ _) = False
hasPar (Seq c1 c2) = hasPar c1 || hasPar c2
hasPar (Par c1 c2) = True
hasPar (Syntax.Meas q c1 c2) = hasPar c1 || hasPar c2
hasPar (Whl q c) = hasPar c

-- counts the number of programs concurrently composed
howManyParC :: C -> Int
howManyParC Skip = 0
howManyParC (Syntax.U _ _) = 0
howManyParC (Seq c1 c2) = howManyParC c1 + howManyParC c2
howManyParC (Par c1 c2) = howManyParC c1 + howManyParC c2 + 2
howManyParC (Syntax.Meas _ c1 c2) = howManyParC c1 + howManyParC c2
howManyParC (Whl _ c) = howManyParC c


small :: C -> LL -> QIO (Either U ((C,LL),U))
small Skip _ = return (Left mempty)
small (Syntax.U g qvars) l = let qbits = map snd $ filter (\(q,_) -> elem q qvars) l
                             in return (Left $ applyUQvars g qbits)                                
small (Syntax.Meas qvar c1 c2) l = do
  let q = snd $ head $ filter (\(qq,_) -> qq==qvar) l
  meas <- measQ q -- Bool
  case meas of
    False -> return $ Right ((c1,l), mempty) -- False = |0>
    True -> return $ Right ((c2,l), mempty) -- True = |1>
small (Seq c1 c2) l = do
  eval_c1 <- small c1 l
  case eval_c1  of
    Left u -> return $ Right ((c2,l), u)
    Right ((c1',l'), u) -> return $ Right ((Seq c1' c2, l'), u)
small (Whl qvar c) l = do
  let q = snd $ head $ filter (\(qq,_) -> qq==qvar) l
  meas <- measQ q -- Bool
  case meas of
    True -> return $ Right ((Seq c (Whl qvar c), l), mempty)
    False -> return (Left mempty)

big :: (C, LL) -> QIO U
big (c,l) = do
  eval_c <- small c l -- Either U ((C,LL),U)
  case eval_c of
    Left u -> return u
    Right ((c',l'),u) -> do
      u' <- big (c',l')
      return (u <> u')


init_qbits :: [QVar] -> QIO [Qbit]
init_qbits [] = return []
init_qbits (h:t) = do
  q <- mkQ False
  qs <- init_qbits t
  return (q:qs)



prog1 = Seq (Syntax.U H ["q"]) (Syntax.Meas "q" Skip (Syntax.U X ["q"]))
l = [("q", Qbit 0)] 

test_small :: C -> QVarList -> QIO (Either U ((C,LL), U))
test_small c qvar = do
  qbits <- init_qbits qvar --[Qbit]
  let l = zip qvar qbits
  small c l

test_big :: C -> QVarList -> QIO [Bool]
test_big c qvar = do
  qbits <- init_qbits qvar -- [Qbit]
  let l = zip qvar qbits
  u <- big (c,l) -- U
  applyU u -- QIO ()
  measQ qbits


  
  

applyUQvars :: G -> [Qbit] -> U
applyUQvars _ [] = mempty
applyUQvars g (h:t) = mappend (gateQIO g h) (applyUQvars g t)

gateQIO :: G -> Qbit -> U
gateQIO I q = rot q rid
gateQIO X q = unot q
gateQIO H q = uhad q

st0 :: QIO Qbit
st0 = mkQbit False

creatQ :: Int -> QIO [Qbit]
creatQ 0 = return []
creatQ n = do
  h <- mkQbit False
  t <- creatQ (n-1)
  return (h:t)

appH :: Qbit -> QIO ()
appH q = applyU (uhad q)
  
p1 :: QIO Bool
p1 = do
  q1 <- st0
  appH q1
  measQ q1

p2 :: QIO Bool
p2 = do
  q1 <- st0
  applyU (unot q1)
  measQ q1
  

hadamards :: [Qbit] -> U
hadamards [] = mempty
hadamards (h:t) = (uhad h) `mappend` (hadamards t)

prog :: Int -> QIO [Bool]
prog n = do
  qbits <- creatQ n
  let stH = hadamards qbits
  applyU stH
  measQ qbits

testIf :: Bool -> QIO Bool
testIf b = do
  q1 <- mkQbit b
  q2 <- mkQbit (not b)
  appH q1
  applyU (ifElseQ q1 (uhad q2) (unot q2))
  measQ q2
