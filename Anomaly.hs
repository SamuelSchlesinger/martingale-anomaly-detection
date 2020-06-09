{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Anomaly where

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Except
import System.Random
import qualified Data.Sequence as Seq

class Monad m => Rand m where
  -- random float between 0 and 1
  rand :: m Float

instance Rand IO where
  rand = randomRIO (0, 1)

instance Rand m => Rand (StateT s m) where
  rand = lift rand

instance Rand m => Rand (ExceptT e m) where
  rand = lift rand

class (Ord t, Ord s) => Strangeness t s | t -> s where
  -- strangeness i (Z U {z_n}) = s_i
  strangeness :: Int -> Seq.Seq t -> s

data AnomalyState s t = AnomalyState
  { observed :: Seq.Seq t
  , martingale :: Float
  , martingales :: Seq.Seq Float
  , iteration :: Int
  , lambda :: Float
  , epsilon :: Float
  } deriving Show

initialAnomalyState :: AnomalyState s t
initialAnomalyState = AnomalyState
  { observed = Seq.empty
  , martingale = 1
  , martingales = Seq.empty
  , iteration = 1
  , lambda = 4
  , epsilon = 0.92
  }

runAnomalyT :: (Rand m, Monad m) => Float -> Float -> AnomalyT s t m a -> m (Either (AnomalyDetected s t) a)
runAnomalyT lam eps a = runExceptT $ flip evalStateT (initialAnomalyState { lambda = lam, epsilon = eps }) a

runAnomalyTWith :: (Rand m, Monad m) => [t] -> Float -> Float -> AnomalyT s t m a -> m (Either (AnomalyDetected s t) a)
runAnomalyTWith from lam eps a = runExceptT $ flip evalStateT (initialAnomalyState { lambda = lam, epsilon = eps, iteration = length from - 1, observed = Seq.fromList from}) a

data AnomalyDetected s t = AnomalyDetected
  { anomalyState :: AnomalyState s t
  } deriving Show

type AnomalyT s t m a = StateT (AnomalyState s t) (ExceptT (AnomalyDetected s t) m) a

observe_ :: (Rand m, Strangeness t s) => t -> AnomalyT s t m ()
observe_ t = do
  state@AnomalyState{observed = os, epsilon = eps, martingale = mar, iteration = n, martingales = mars} <- get
  thetaN <- rand
  let sN = strangeness n (t Seq.<| os)
  let strangerThings = length [i | i <- [1..n], strangeness i (t Seq.<| os) > sN ]
  let asStrangeThings = length [i | i <- [1..n], strangeness i (t Seq.<| os) == sN ]
  let pValue = (fromIntegral strangerThings + thetaN * fromIntegral asStrangeThings) / fromIntegral n
  let mar' = eps * (pValue ** (eps - 1)) * mar
  put state { observed = t Seq.<| os, martingale = mar', iteration = n + 1, martingales = mar' Seq.<| mars }

observe :: (Rand m, Strangeness t s) => t -> AnomalyT s t m ()
observe t = do
  observe_ t
  state@AnomalyState{lambda = lam, martingale = mar} <- get
  if mar >= lam
    then throwError (AnomalyDetected state)
    else pure ()

instance Strangeness Float Float where
  strangeness i xs = abs (xs `Seq.index` (i - 1) - sum xs / fromIntegral (Seq.length xs))
