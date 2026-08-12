{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright:   (c) 2022 Bodigrim
-- License:     BSD3
module Data.List.Infinite.Internal (
  Infinite (..),
  build,
  foldr,
) where

import Prelude hiding (foldr)

-- | Type of infinite lists.
--
-- In terms of recursion schemes, 'Infinite' @a@ is a fix point of the base functor @(a,)@,
-- 'Data.List.Infinite.foldr' is a catamorphism and 'Data.List.Infinite.unfoldr' is an anamorphism.
data Infinite a = a :< Infinite a

infixr 5 :<

build :: forall a. (forall b. (a -> b -> b) -> b) -> Infinite a
build g = g (:<)
{-# INLINE [1] build #-}

-- | Right-associative fold of an infinite list, necessarily lazy in the accumulator.
-- Any unconditional attempt to force the accumulator even
-- to the weak head normal form (WHNF)
-- will hang the computation. E. g., the following definition isn't productive:
--
-- > import Data.List.NonEmpty (NonEmpty(..))
-- > toNonEmpty = foldr (\a (x :| xs) -> a :| x : xs) :: Infinite a -> NonEmpty a
--
-- One should use lazy patterns, e. g.,
--
-- > toNonEmpty = foldr (\a ~(x :| xs) -> a :| x : xs)
--
-- This is a catamorphism on infinite lists.
foldr :: (a -> b -> b) -> Infinite a -> b
foldr f = go
  where
    go (x :< xs) = f x (go xs)
{-# INLINE [0] foldr #-}

{-# RULES
"foldr/build" forall cons (g :: forall b. (a -> b -> b) -> b).
  foldr cons (build g) =
    g cons
"foldr/cons/build" forall cons x (g :: forall b. (a -> b -> b) -> b).
  foldr cons (x :< build g) =
    cons x (g cons)
  #-}
