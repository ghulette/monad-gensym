{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             GeneralizedNewtypeDeriving #-}

module Env where

import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map

type Env k e = Map k e

newtype EnvT k e m a = EnvT {env :: ReaderT (Map k e) m a}
  deriving (Monad,Functor,MonadTrans,MonadIO)

class Monad m => MonadEnv k e m | m -> k, m -> e where
  fetch :: k -> m (Maybe e)
  bind :: k -> e -> m a -> m a

instance (Monad m,Ord k) => MonadEnv k e (EnvT k e m) where
  fetch x = EnvT $ do
    val <- asks (Map.lookup x)
    return val
  bind x v (EnvT m) = EnvT $ local (Map.insert x v) m

evalEnvT :: (Monad m,Ord k) => EnvT k e m a -> m a
evalEnvT (EnvT m) = runReaderT m Map.empty
