module DistTMonad where

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Except
import Control.Monad
import Control.Applicative

{-
To build a monad in Haskell we need:
(1) a data type (let us call it DT)
(2) Instantiate to Functor
    · fmap :: (a->b) -> (DT a) -> (DT b)
(3) Instantiate to Applicative
    · pure :: a -> (DT a)
    · (<*>) :: (DT (a->b)) -> (DT a) -> (DT b)
(4) Instantiate to Monad
    · return :: a -> (DT a)
    · (>>=) :: (DT a) -> (a-> (DT b)) -> (DT b)

To build a monad transformer in Haskell we need:
(1) Define a type of kind (* -> *) -> * -> * (let us call it MonT)
(2) Instantiate to Functor
    · fmap :: (Monad m) => (a->b) -> (MonT m a) -> (MonT m b)
(3) Instantiate to Applicative
    · pure :: (Monad m) => a -> (MonT m a)
    · (<*>) :: (Monad m) => (MonT m (a->b)) -> (MonT m a) -> (MonT m b)
(4) Instantiate to Monad
    · return :: (Monad m) => a -> (MonT m a)
    · (>>=) :: (Monad m) => (MonT m a) -> (a -> (MonT m b)) -> (MonT m b)
(5) Instantiate to MonadTrans (need to import Control.Monad.Trans)
    · lift :: (Monad m) => (m a) -> (MonT m a)
-}


--START: Dist Monad--
newtype Dist x = Dist { getDist :: [(x, Double)]} deriving (Show,Eq)

instance Functor Dist where
  --fmap :: (a->b) -> (Dist a) -> (Dist b) 
  fmap f (Dist dist) = Dist $ [(f x,p) | (x,p) <- dist] 
  --fmap f (Dist x) = Dist $ map (\(y,p) -> (f y, p)) x

instance Applicative Dist where
    pure x = Dist $ [(x,1)]
    (Dist distf) <*> (Dist dist) = Dist $ [(f x, pf*px) | (x,px)<-dist, (f, pf) <- distf]

instance Monad Dist where
    return = pure
    (Dist x) >>= f = Dist $ [(x'', px'*px) | (x',px)<-x, (x'', px')<- getDist $ f x']
--END: Dist Monad--

--START: DistT-- 
newtype DistT m a = DistT { runDistT :: m [(a, Double)] } 

joinDist :: (Monad m) => (a -> DistT m b) -> [(a, Double)] -> m [[(b, Double)]]
joinDist f dist = forM dist $ \(x, px) -> do
  let DistT mdist = f x
  dist' <- mdist
  return [(y, px * py) | (y, py) <- dist']

instance (Monad m) => Functor (DistT m) where
  fmap f (DistT mdist) = DistT $ do
    dist <- mdist
    return [(f x, p) | (x, p) <- dist]

instance (Monad m) => Applicative (DistT m) where
  pure x = DistT $ return [(x, 1)]
  (DistT mdistf) <*> (DistT mdist) = DistT $ do
    distf <- mdistf
    dist <- mdist
    return [(f x, pf * px) | (f, pf) <- distf, (x, px) <- dist]

instance (Monad m) => Monad (DistT m) where
  return = pure
  (DistT mdist) >>= f = DistT $ do
    dist <- mdist
    appf <- joinDist f dist
    return (concat appf)
    
instance MonadTrans DistT where
  lift ma = DistT $ liftM (\x -> [(x, 1)]) ma

{- Alternative definition of the instance MonadTrans DistT
instance MonadTrans DistT where
  lift ma = DistT $ do
    a <- ma
    return [(a,1)]
-}
--END: DistT--
