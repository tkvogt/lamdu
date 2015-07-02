{-# LANGUAGE ConstraintKinds, FlexibleInstances, UndecidableInstances, CPP #-}
module Control.MonadA(MonadA) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative)

class (Monad m, Applicative m) => MonadA m
instance (Monad m, Applicative m) => MonadA m
#else
type MonadA = Monad
#endif
