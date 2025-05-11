
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Data.List.Infinite (Infinite (..))
import qualified Data.List.Infinite as Infinite

import qualified Criterion.Main as C

import GHC.Exts (oneShot)

ones :: Infinite Integer
ones = Infinite.repeat 1

partialSums
  :: Num a
  => (forall acc x y . (acc -> x -> (acc, y)) -> acc -> Infinite x -> Infinite y)
  -> Infinite a -> Infinite a
partialSums mapAccumL = mapAccumL (\acc x -> let acc' = acc+x in (acc',acc')) 0

bench
  :: String
  -> (forall acc x y . (acc -> x -> (acc, y)) -> acc -> Infinite x -> Infinite y)
  -> C.Benchmark
bench name mapAccumL = C.bench name $ C.whnf ((Infinite.!! 2000) . partialSums mapAccumL) ones

main :: IO ()
main = C.defaultMain
  [ C.bgroup "mapAccumL"
    [ bench "mapAccumL1" mapAccumL1
    , bench "mapAccumL2" mapAccumL2
      ]
    ]

-- | Same as 'mapAccumL', but strict in accumulator.
mapAccumL1 :: (acc -> x -> (acc, y)) -> acc -> Infinite x -> Infinite y
mapAccumL1 f = go
  where
    go !acc (x :< xs) =
      let (acc', y) = f acc x
      in  y :< go acc' xs

mapAccumL2 :: (acc -> x -> (acc, y)) -> acc -> Infinite x -> Infinite y
mapAccumL2 f acc0 xs =
  Infinite.foldr (\x cont -> \(!acc) -> let (acc', y) = f acc x in y :< cont acc') xs acc0

mapAccumL2FB :: (acc -> x -> (acc, y)) -> x -> (acc -> Infinite y) -> acc -> Infinite y
mapAccumL2FB f = \x cont -> oneShot $ \(!acc) -> let (acc', y) = f acc x in y :< cont acc'

{-# NOINLINE [1] mapAccumL2 #-}

{-# INLINE [0] mapAccumL2FB #-}

{-# RULES
"mapAccumL2" [~1] forall f s xs.
  mapAccumL2 f s xs = Infinite.foldr (mapAccumL2FB f) xs s

"mapAccumL2List" [1] forall f s xs.
  Infinite.foldr (mapAccumL2FB f) xs s = mapAccumL2 f s xs
  #-}
