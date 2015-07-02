{-# LANGUAGE CPP #-}
{-# OPTIONS -fno-warn-orphans #-}
module System.Random.Utils
    ( splits, randFunc, genFromHashable
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif
import Data.Binary (Binary(..))
import Data.Hashable (Hashable, hashWithSalt)
import System.Random (RandomGen, Random, StdGen, split, mkStdGen, random)

-- Yucky work-around for lack of Binary instance
instance Binary StdGen where
    get = read <$> get
    put = put . show

splits :: RandomGen g => g -> [g]
splits = map fst . iterate (split . snd) . split

randFunc :: (Hashable h, Random r) => h -> r
randFunc = fst . random . genFromHashable

genFromHashable :: Hashable a => a -> StdGen
genFromHashable = mkStdGen . hashWithSalt 0
