{-# LANGUAGE PostfixOperators #-}

module Main where

-- import qualified Data.List.Infinite as Inf
import Test.Tasty.Bench
import qualified Uninterleave


main :: IO ()
main = defaultMain $
  [
  ] <> concat
  [ Uninterleave.benchmarks
  ]
