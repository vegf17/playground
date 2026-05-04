module SemOp where

import Syntax
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

type LL = [(QVar, Qbit)]

type StQIO a = StateT LL (ExceptT (QIO ()) QIO) a


small :: C -> LL -> QIO (Either U ((C,LL),U))
small Skip _ = return (Left mempty)
small (Syntax.U g qvars) l = let qbits = map snd $ filter (\(q,_) -> elem q qvars) l
                             in return (Left $ applyUQvars g qbits)                                
small (Syntax.Meas qvar c1 c2) l = do
  let q = snd $ head $ filter (\(qq,_) -> qq==qvar) l
  meas <- measQ q -- Bool
  case meas of
    True -> return $ Right ((c1,l), mempty) -- True = |1>
    False -> return $ Right ((c2,l), mempty) -- False = |0>
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
