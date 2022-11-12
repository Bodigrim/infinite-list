{-# LANGUAGE RankNTypes #-}

module Data.List.Infinite.Internal (
  Infinite (..),
  build,
) where

-- | Type of infinite lists.
data Infinite a = a :< Infinite a

infixr 5 :<

build :: forall a. (forall b. (a -> b -> b) -> b) -> Infinite a
build g = g (:<)
{-# INLINE [1] build #-}
