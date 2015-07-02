{-# LANGUAGE DeriveFunctor, CPP #-}
module Control.Monad.Unit(Unit(..)) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative(..))
#endif

data Unit a = Unit
    deriving (Functor)

instance Applicative Unit where
    pure = const Unit
    _ <*> _ = Unit

instance Monad Unit where
    return = const Unit
    _ >>= _ = Unit
