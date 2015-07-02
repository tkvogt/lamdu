{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, CPP #-}
{-# OPTIONS -fno-warn-orphans #-}
module Data.Monoid.Instances () where

#if __GLASGOW_HASKELL__ < 710
import Control.DeepSeq (NFData)
#endif
import Data.Binary (Binary)
import Data.Monoid (Any(..))

deriving instance Binary Any
#if __GLASGOW_HASKELL__ < 710
deriving instance NFData Any
#endif
