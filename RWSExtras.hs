{-# LANGUAGE Rank2Types #-}

module RWSExtras (withSub, withSubSt) where

import Control.Monad.RWS
import Control.Monad.State.Class
import Control.Monad.State
import Control.Lens

withSub :: (Monad m, Monoid w) => Lens s s t t -> RWST r w t m a -> RWST r w s m a
withSub len inner = RWST $ \r s -> liftM (_2 %~ flip (set len) s) $ runRWST inner r (view len s)

withSubSt :: (Monad m) => Lens s s t t -> StateT t m a -> StateT s m a
withSubSt len inner = StateT $ \s -> liftM (_2 %~ flip (set len) s) $ runStateT inner (view len s)
