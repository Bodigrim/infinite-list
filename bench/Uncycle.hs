{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
module Uncycle where

import qualified Data.List.Infinite as Inf
import qualified Data.List.NonEmpty as NE
import Data.List.Infinite (Infinite (..))
import Data.List.NonEmpty (NonEmpty (..))
import Test.Tasty.Bench
import GHC.Exts


----

benchmarks :: [Benchmark]
benchmarks =
  [ bench "uncycleBy1" $ nf test uncycleBy1
  , bench "uncycleBy2" $ nf test uncycleBy2
  , bench "uncycleBy3" $ nf test uncycleBy3
  ]


test :: (a ~ Int) => ((a -> a -> Bool) -> Infinite Int -> Infinite ([a], NonEmpty a)) -> (Int, Int)
test f =
  case f (==) (Inf.iterate func1 0) of
    (l, r) :< _ -> (length l, length r)

func1 :: Int -> Int
func1 n
  | n < 1_000_000 = n+1
  | let           = 1234

----

uncycleBy1 :: (a -> a -> Bool) -> Infinite a -> Infinite ([a], NonEmpty a)
uncycleBy1 eq s@(x:<xs) = (pre, cyc) :< uncycleBy1 eq rest
  where
    (pre, cyc, rest) = go 1 1 x xs

    go π λ t (h:<hs)
      | t `eq` h = rollup nil nil `uncurry` Inf.splitAt λ s
      | π == λ   = go (π+π) 1 h hs
      | let      = go π (λ+1) t hs

    rollup f q (t:ts) (h:<hs)
      | t `eq` h    = (list f, t NE.:| (ts <> list q), h:<hs)
      | let         = rollup (snoc f t) (snoc q h) ts hs
    rollup f q _ hs = rollup f nil (list q) hs

    snoc f a z = f (a:z)
    list f     = f []
    nil      z = z

----

uncycleBy2 :: (a -> a -> Bool) -> Infinite a -> Infinite ([a], NonEmpty a)
uncycleBy2 eq s =
    case rollup s (Inf.drop cy s) of
      (l, r, ss) -> (l, r) :< uncycleBy2 eq ss
  where
    -- ~O(2n). Brent's algorithm
    go π λ t (h:<hs)
      | t `eq` h = λ
      | π == λ   = go (π+π) 1 h hs
      | let      = go π (λ+1) t hs

    -- O(n exactly).
    rollup (t:<ts) (h:<hs)
      | t `eq` h
      , (l,r) <- Inf.splitAt (cy-1) ts
      = ([], t NE.:| l, r)
      | (l,r,ss) <- rollup ts hs = (t:l, r, ss)

    -- The length of the cycle
    cy | x:<xs <- s = go 1 1 x xs

----

uncycleBy3 :: (a -> a -> Bool) -> Infinite a -> Infinite ([a], NonEmpty a)
uncycleBy3 eq s =
    case rollup s (Inf.drop cy s) of
      (l, r, ss) -> (l, r) :< uncycleBy2 eq ss
  where
    -- ~O(2n). Brent's algorithm
    go π λ t (h:<hs)
      | t `eq` h      = λ
      | 1# <- π ==# λ = go (π +# π) 1# h hs
      | let           = go π (λ +# 1#) t hs

    -- O(n exactly).
    rollup (t:<ts) (h:<hs)
      | t `eq` h
      , (l,r) <- Inf.splitAt (cy - 1) ts
      = ([], t NE.:| l, r)
      | (l,r,ss) <- rollup ts hs = (t:l, r, ss)

    -- The length of the cycle
    cy | x:<xs <- s = I# (go 1# 1# x xs)

