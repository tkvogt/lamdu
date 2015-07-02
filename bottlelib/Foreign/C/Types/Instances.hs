{-# LANGUAGE CPP #-}
{-# OPTIONS -fno-warn-orphans #-}
module Foreign.C.Types.Instances () where

#if __GLASGOW_HASKELL__ < 710
import Control.DeepSeq (NFData(..))
#endif
import Data.Aeson (ToJSON(..), FromJSON(..))
import Foreign.C.Types (CDouble)

#if __GLASGOW_HASKELL__ < 710
instance NFData CDouble where rnf = (`seq` ())
#endif

instance FromJSON CDouble where
    parseJSON = fmap (realToFrac :: Double -> CDouble) . parseJSON
instance ToJSON CDouble where
    toJSON = toJSON . (realToFrac :: CDouble -> Double)
