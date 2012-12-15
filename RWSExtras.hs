{-# LANGUAGE Rank2Types #-}

module RWSExtras (withSub) where

import Control.Monad.RWS
import Control.Lens

withSub :: (Monad m, Monoid w) => Lens s s t t -> RWST r w t m a -> RWST r w s m a
withSub len inner = RWST $ \r s -> liftM (_2 %~ flip (set len) s) $ runRWST inner r (view len s)