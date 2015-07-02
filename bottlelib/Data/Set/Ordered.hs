{-# LANGUAGE DeriveFoldable, CPP #-}
module Data.Set.Ordered
    ( OrderedSet
    , singleton
    ) where

#if __GLASGOW_HASKELL__ < 710
import Data.Foldable (Foldable)
import Data.Monoid (Monoid(..))
#endif

newtype OrderedSet a = OrderedSet [a]
    deriving (Show, Eq, Ord, Foldable)
instance Eq a => Monoid (OrderedSet a) where
    mempty = OrderedSet []
    OrderedSet xs `mappend` OrderedSet ys = OrderedSet $ xs ++ filter (`notElem` xs) ys

singleton :: a -> OrderedSet a
singleton = OrderedSet . (:[])
