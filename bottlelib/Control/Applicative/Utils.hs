{-# LANGUAGE CPP #-}
module Control.Applicative.Utils (when) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative(..))
#endif

when :: Applicative f => Bool -> f () -> f ()
when True x = x
when False _ = pure ()
