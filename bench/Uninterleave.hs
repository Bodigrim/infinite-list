{-# LANGUAGE ViewPatterns, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Uninterleave where

import Test.Tasty.Bench
import Data.List.Infinite (Infinite (..))


benchmarks :: [Benchmark]
benchmarks =
  [ bench "uninterleave1" $ nf test uninterleave1
  , bench "uninterleave2" $ nf test uninterleave2
  , bench "uninterleave3" $ nf test uninterleave3
  , bench "uninterleave4" $ nf test uninterleave4
  , bench "uninterleave5" $ nf test uninterleave5
  , bench "uninterleave6" $ nf test uninterleave6
  , bench "uninterleave7" $ nf test uninterleave7
  ]

test :: (a ~ Int) => (Infinite a -> (Infinite a, Infinite a)) -> a
test f = a
  where
    a :< _ = iterate (snd . f . fst . f) nats !! 12

    nats = go 1 where
      go n = n :< go (succ n)

----

uninterleave1 :: Infinite a -> (Infinite a, Infinite a)
uninterleave1 lrs = (uninterleaveL lrs, uninterleaveR lrs)

uninterleaveL :: Infinite a -> Infinite a
uninterleaveL (l :< ~(_ :< ls)) = l :< uninterleaveL ls

uninterleaveR :: Infinite a -> Infinite a
uninterleaveR (_ :< r :< rs) = r :< uninterleaveR rs

----

uninterleave2 :: Infinite a -> (Infinite a, Infinite a)
uninterleave2 lrs = (uninterleaveL lrs, uninterleaveR lrs)
  where
    uninterleaveL :: Infinite a -> Infinite a
    uninterleaveL (l :< ~(_ :< ls)) = l :< uninterleaveL ls

    uninterleaveR :: Infinite a -> Infinite a
    uninterleaveR (_ :< r :< rs) = r :< uninterleaveR rs

----

uninterleave3 :: Infinite a -> (Infinite a, Infinite a)
uninterleave3 lrs = (fmap fst tups, fmap snd tups)
  where
    go (l :< r :< lrs) = (l, r) :< go lrs
    tups = go lrs

----

uninterleave4 :: Infinite a -> (Infinite a, Infinite a)
uninterleave4 (l :< r :< ~(uninterleave4->(ls, rs))) =
  (l :< ls, r :< rs)

----

uninterleave5 :: Infinite a -> (Infinite a, Infinite a)
uninterleave5 (l :< ~(uninterleave5->(rs, ls))) =
  (l :< ls, rs)

----

uninterleave6 :: Infinite a -> (Infinite a, Infinite a)
uninterleave6 lrs = (uninterleaveL2 lrs, uninterleaveR2 lrs)

uninterleaveL2 :: Infinite a -> Infinite a
uninterleaveL2 (l :< ls) = l :< uninterleaveR2 ls

uninterleaveR2 :: Infinite a -> Infinite a
uninterleaveR2 (_ :< rs) = uninterleaveL2 rs

----

uninterleave7 :: Infinite a -> (Infinite a, Infinite a)
uninterleave7 lrs = (uninterleaveL2 lrs, uninterleaveR2 lrs)
  where
    uninterleaveL2 :: Infinite a -> Infinite a
    uninterleaveL2 (l :< ls) = l :< uninterleaveR2 ls

    uninterleaveR2 :: Infinite a -> Infinite a
    uninterleaveR2 (_ :< rs) = uninterleaveL2 rs

