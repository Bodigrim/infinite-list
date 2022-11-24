{-# LANGUAGE PostfixOperators #-}

module Main where

-- import qualified Data.List.Infinite as Inf
import Test.Tasty.Bench
import qualified Uncycle

main :: IO ()
main = defaultMain $
  [
  ] <> concat
  [ Uncycle.benchmarks
  ]
