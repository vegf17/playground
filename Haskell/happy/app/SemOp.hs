module SemOp where

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
--Haskell imports--

--my imports--
import Com
import SemAEBE
import Syntax
--my imports--

--START: small-step semantics--
-- StTQC C '=' S -> [[(Either S (C,S), Prob)]]
type StTQC a = StateT SC (Either SC) a --small

--Codifies the behavior of the small-step operational semantics
small :: C -> StTQC C
small Skip = StateT $ \s -> Left s
small (Asg var e) = StateT $ \s -> Left $ (changeSt var (bigStepExp e s) s)
small (Seq c1 c2) = do 
    s <- get 
    let cp = runStateT (small c1) s  -- :: [[(Either LMem (Com, LMem), Rational)]]
        seqC = compSeq cp c2
    StateT $ \_ -> seqC
small (IfC b c1 c2) = do
  s <- get
  if (bigStepBExp b s) == True
    then StateT $ \_ -> Right (c1,s)
    else StateT $ \_ -> Right (c2,s)
small (Whl b c) = do
  s <- get
  if (bigStepBExp b s) == True 
    then StateT $ \_ -> Right (Seq c (Whl b c), s)
    else StateT $ \_ -> Left s

--auxiliary functions for sequential and parallel composition
compSeq :: Either SC (C,SC) -> C -> Either SC (C,SC)
compSeq (Left s) c = Right (c,s) 
compSeq (Right (cc, s)) c = Right (Seq cc c, s) 

--Evaluates the results of the small-step operational semantics for a given command C and state s
runSmall :: (C, SC) -> Either SC (C, SC)
runSmall (c, s) = runStateT (small c) s
--END: small-step semantics--


big :: C -> StTQC SC
big c = (small c) >>= big

runBig :: (C, SC) -> Either SC (SC,SC)
runBig (c, s) = runStateT (big c) s
