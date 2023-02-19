{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright:   (c) 2022 Bodigrim
-- License:     BSD3
module Data.List.Infinite.Internal (
  Infinite (..),
  build,
) where

-- | Type of infinite lists.
--
-- In terms of recursion schemes, 'Infinite' @a@ is a fix point of the base functor @(a,)@,
-- 'Data.List.Infinite.foldr' is a catamorphism and 'Data.List.Infinite.unfoldr' is an anamorphism.
data Infinite a = a :< Infinite a

infixr 5 :<

build :: forall a. (forall b. (a -> b -> b) -> b) -> Infinite a
build g = g (:<)
{-# INLINE [1] build #-}
