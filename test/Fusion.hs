-- |
-- Copyright:   (c) 2022 Bodigrim
-- Licence:     BSD3

{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O -dsuppress-all -dno-suppress-type-signatures -fplugin=Test.Tasty.Inspection.Plugin #-}

module Main where

import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.Inspection
import Test.Tasty.Runners

import Data.Coerce
import Data.Ord
import Data.List.Infinite (Infinite(..))
import qualified Data.List.Infinite as I
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

foldrMap :: Infinite Int -> Infinite Int
foldrMap xs = I.foldr (\x acc -> fromIntegral x :< acc) (I.map fromIntegral xs :: Infinite Word)

foldrConsMap :: Int -> Infinite Int -> Infinite Int
foldrConsMap i xs = I.foldr (\x acc -> fromIntegral x :< acc) (fromIntegral i :< (I.map fromIntegral xs :: Infinite Word))

mapMap :: Infinite Int -> Infinite Int
mapMap xs = I.map fromIntegral (I.map fromIntegral xs :: Infinite Word)

mapId :: Infinite Int -> Infinite Int
mapId xs = I.map id (I.map id xs)

mapCoerce :: Infinite Int -> Infinite (Down Int)
mapCoerce xs = I.map coerce xs

headIterate :: Int -> Int
headIterate x = I.head (I.iterate (+ 1) x)

foldrIterate :: Int -> [Int]
foldrIterate x = I.foldr (\a acc -> a : a : acc) (I.iterate (+ 1) x)

foldrIterate' :: Int -> [Int]
foldrIterate' x = I.foldr (\a acc -> a : a : acc) (I.iterate (+ 1) x)

foldrRepeat :: Int -> [Int]
foldrRepeat x = I.foldr (\a acc -> a : a : acc) (I.repeat x)

headFilterIterate :: Int -> Int
headFilterIterate x = I.head (I.filter (> 10) (I.iterate (+ 1) x))

filterFilter :: Infinite Int -> Infinite Int
filterFilter xs = I.filter (> 10) (I.filter (> 5) xs)

filterFilter' :: Infinite Int -> Infinite Int
filterFilter' xs = I.filter (\x -> x > 10 && x > 5) xs

foldrScanl :: Infinite Int -> Infinite Int
foldrScanl xs = I.foldr (\a acc -> fromIntegral a :< acc)
  (I.scanl (\_acc a -> fromIntegral a) (0 :: Word) xs)

foldrScanl' :: Infinite Int -> Infinite Int
foldrScanl' xs = I.foldr (\a acc -> fromIntegral a :< acc)
  (I.scanl' (\_acc a -> fromIntegral a) (0 :: Word) xs)

takeRepeat :: Int -> [Int]
takeRepeat x = I.take x (I.repeat x)

takeDropRepeat :: Int -> [Int]
takeDropRepeat x = I.take x (I.drop x (I.repeat x))

genericTakeGenericDropRepeat :: Word -> [Word]
genericTakeGenericDropRepeat x = I.genericTake x (I.genericDrop x (I.repeat x))

takeWhileIterate :: Int -> [Int]
takeWhileIterate x = I.takeWhile (< 10) (I.iterate (+ 1) x)

takeWhileDropWhileIterate :: Int -> [Int]
takeWhileDropWhileIterate x = I.takeWhile (< 20) $ I.dropWhile (< 10) (I.iterate (+ 1) x)

foldrCycle :: NonEmpty Int -> [Int]
foldrCycle xs = I.foldr (:) (I.cycle xs)

foldrWordsCycle :: [Char] -> [Char]
foldrWordsCycle xs = I.foldr (\a acc -> NE.head a : acc) (I.words (I.cycle (' ' :| xs)))

foldrMapAccumL :: Infinite Int -> Infinite Int
foldrMapAccumL xs = I.foldr (\a acc -> fromIntegral a :< acc)
  (I.mapAccumL (\acc x -> (acc, fromIntegral x :: Word)) (0 :: Int) xs)

mapAccumLRepeat :: Int -> Infinite Int
mapAccumLRepeat n =
  I.mapAccumL (\acc x -> (acc, fromIntegral x)) 'q' (I.repeat (fromIntegral n :: Word))


takeFilterIterate :: [Int]
takeFilterIterate = I.take 100 $ I.filter odd $ I.iterate (+ 1) 0


sumTakeFilterIterate :: Int
sumTakeFilterIterate = sum $ I.take 100 $ I.filter odd $ I.iterate (+ 1) 0

takeFilterCycle :: [Int]
takeFilterCycle = I.take 100 $ I.filter odd $ I.cycle $ 0 :| [1..]

takeFilterEllipsis3 :: [Int]
takeFilterEllipsis3 = I.take 100 $ I.filter odd (0 I....)

takeFilterEllipsis4 :: [Int]
takeFilterEllipsis4 = I.take 100 $ I.filter odd ((0, 3) I.....)

sumTakeFilterEllipsis3 :: Int
sumTakeFilterEllipsis3 = sum $ I.take 100 $ I.filter odd (0 I....)

sumTakeFilterEllipsis4 :: Int
sumTakeFilterEllipsis4 = sum $ I.take 100 $ I.filter odd ((0, 3) I.....)


takeToListFilterIterate :: [Int]
takeToListFilterIterate = Prelude.take 100 $ I.toList $ I.filter odd $ I.iterate (+ 1) 0

sumTakeToListFilterIterate :: Int
sumTakeToListFilterIterate = sum $ Prelude.take 100 $ I.toList $ I.filter odd $ I.iterate (+ 1) 0

takeToListFilterCycle :: [Int]
takeToListFilterCycle = Prelude.take 100 $ I.toList $ I.filter odd $ I.cycle $ 0 :| [1..]

takeToListFilterEllipsis3 :: [Int]
takeToListFilterEllipsis3 = Prelude.take 100 $ I.toList $ I.filter odd (0 I....)

takeToListFilterEllipsis4 :: [Int]
takeToListFilterEllipsis4 = Prelude.take 100 $ I.toList $ I.filter odd ((0, 3) I.....)

sumTakeToListFilterEllipsis3 :: Int
sumTakeToListFilterEllipsis3 = sum $ Prelude.take 100 $ I.toList $ I.filter odd (0 I....)

sumTakeToListFilterEllipsis4 :: Int
sumTakeToListFilterEllipsis4 = sum $ Prelude.take 100 $ I.toList $ I.filter odd ((0, 3) I.....)


headFilterMapEllipsis3 :: Int
headFilterMapEllipsis3 = I.head $ I.filter odd $ I.map (+ 1) (0 I....)

headFilterMapEllipsis4 :: Int
headFilterMapEllipsis4 = I.head $ I.filter odd $ I.map (+ 1) ((0, 3) I.....)

toListConcatRepeat :: [Int]
toListConcatRepeat = I.toList $ I.concat $ I.repeat $ NE.singleton 1

toListConcatMapRepeat :: [Int]
toListConcatMapRepeat = I.toList $ I.concatMap NE.singleton $ I.repeat 1

toListIntersperseRepeat :: [Int]
toListIntersperseRepeat = I.toList $ I.intersperse 1 $ I.repeat 0

toListIntercalateRepeat :: [Int]
toListIntercalateRepeat = I.toList $ I.intercalate (NE.singleton 1) $ I.repeat [0]

headMapZipIterate :: Bool
headMapZipIterate = I.head $ I.map ((> 0) . snd) $ I.zip (I.repeat (1 :: Word)) $ I.iterate id (0 :: Int)

headMapFlipZipIterate :: Bool
headMapFlipZipIterate = I.head $ I.map ((> 0) . fst) $ flip I.zip (I.repeat (1 :: Word)) $ I.iterate id (0 :: Int)

zeros :: Infinite Word
zeros = I.repeat 0
{-# NOINLINE zeros #-}

zipWithRepeat1 :: Infinite Bool
zipWithRepeat1 = I.zipWith (\x y -> x == fromIntegral y) (I.repeat (1 :: Int)) zeros

zipWithRepeat2 :: Infinite Bool
zipWithRepeat2 = I.zipWith (\x y -> y == fromIntegral x) zeros (I.repeat (1 :: Int))

zipWith3Repeat1 :: Infinite Bool
zipWith3Repeat1 = I.zipWith3 (\x y z -> x == fromIntegral (y + z)) (I.repeat (1 :: Int)) zeros zeros

zipWith3Repeat2 :: Infinite Bool
zipWith3Repeat2 = I.zipWith3 (\x y z -> y == fromIntegral (x + z)) zeros (I.repeat (1 :: Int)) zeros

zipWith3Repeat3 :: Infinite Bool
zipWith3Repeat3 = I.zipWith3 (\x y z -> z == fromIntegral (x + y)) zeros zeros (I.repeat (1 :: Int))

zipWith4Repeat1 :: Infinite Bool
zipWith4Repeat1 = I.zipWith4 (\x y z t -> x == fromIntegral (y + z + t)) (I.repeat (1 :: Int)) zeros zeros zeros

zipWith4Repeat2 :: Infinite Bool
zipWith4Repeat2 = I.zipWith4 (\x y z t -> y == fromIntegral (x + z + t)) zeros (I.repeat (1 :: Int)) zeros zeros

zipWith4Repeat3 :: Infinite Bool
zipWith4Repeat3 = I.zipWith4 (\x y z t -> z == fromIntegral (x + y + t)) zeros zeros (I.repeat (1 :: Int)) zeros

zipWith4Repeat4 :: Infinite Bool
zipWith4Repeat4 = I.zipWith4 (\x y z t -> t == fromIntegral (x + y + z)) zeros zeros zeros (I.repeat (1 :: Int))

zipWith5Repeat1 :: Infinite Bool
zipWith5Repeat1 = I.zipWith5 (\x y z t u -> x == fromIntegral (y + z + t + u)) (I.repeat (1 :: Int)) zeros zeros zeros zeros

zipWith5Repeat2 :: Infinite Bool
zipWith5Repeat2 = I.zipWith5 (\x y z t u -> y == fromIntegral (x + z + t + u)) zeros (I.repeat (1 :: Int)) zeros zeros zeros

zipWith5Repeat3 :: Infinite Bool
zipWith5Repeat3 = I.zipWith5 (\x y z t u -> z == fromIntegral (x + y + t + u)) zeros zeros (I.repeat (1 :: Int)) zeros zeros

zipWith5Repeat4 :: Infinite Bool
zipWith5Repeat4 = I.zipWith5 (\x y z t u -> t == fromIntegral (x + y + z + u)) zeros zeros zeros (I.repeat (1 :: Int)) zeros

zipWith5Repeat5 :: Infinite Bool
zipWith5Repeat5 = I.zipWith5 (\x y z t u -> u == fromIntegral (x + y + z + t)) zeros zeros zeros zeros (I.repeat (1 :: Int))

zipWith6Repeat1 :: Infinite Bool
zipWith6Repeat1 = I.zipWith6 (\x y z t u v -> x == fromIntegral (y + z + t + u + v)) (I.repeat (1 :: Int)) zeros zeros zeros zeros zeros

zipWith6Repeat2 :: Infinite Bool
zipWith6Repeat2 = I.zipWith6 (\x y z t u v -> y == fromIntegral (x + z + t + u + v)) zeros (I.repeat (1 :: Int)) zeros zeros zeros zeros

zipWith6Repeat3 :: Infinite Bool
zipWith6Repeat3 = I.zipWith6 (\x y z t u v -> z == fromIntegral (x + y + t + u + v)) zeros zeros (I.repeat (1 :: Int)) zeros zeros zeros

zipWith6Repeat4 :: Infinite Bool
zipWith6Repeat4 = I.zipWith6 (\x y z t u v -> t == fromIntegral (x + y + z + u + v)) zeros zeros zeros (I.repeat (1 :: Int)) zeros zeros

zipWith6Repeat5 :: Infinite Bool
zipWith6Repeat5 = I.zipWith6 (\x y z t u v -> u == fromIntegral (x + y + z + t + v)) zeros zeros zeros zeros (I.repeat (1 :: Int)) zeros

zipWith6Repeat6 :: Infinite Bool
zipWith6Repeat6 = I.zipWith6 (\x y z t u v -> v == fromIntegral (x + y + z + t + u)) zeros zeros zeros zeros zeros (I.repeat (1 :: Int))

zipWith7Repeat1 :: Infinite Bool
zipWith7Repeat1 = I.zipWith7 (\x y z t u v w -> x == fromIntegral (y + z + t + u + v + w)) (I.repeat (1 :: Int)) zeros zeros zeros zeros zeros zeros

zipWith7Repeat2 :: Infinite Bool
zipWith7Repeat2 = I.zipWith7 (\x y z t u v w -> y == fromIntegral (x + z + t + u + v + w)) zeros (I.repeat (1 :: Int)) zeros zeros zeros zeros zeros

zipWith7Repeat3 :: Infinite Bool
zipWith7Repeat3 = I.zipWith7 (\x y z t u v w -> z == fromIntegral (x + y + t + u + v + w)) zeros zeros (I.repeat (1 :: Int)) zeros zeros zeros zeros

zipWith7Repeat4 :: Infinite Bool
zipWith7Repeat4 = I.zipWith7 (\x y z t u v w -> t == fromIntegral (x + y + z + u + v + w)) zeros zeros zeros (I.repeat (1 :: Int)) zeros zeros zeros

zipWith7Repeat5 :: Infinite Bool
zipWith7Repeat5 = I.zipWith7 (\x y z t u v w -> u == fromIntegral (x + y + z + t + v + w)) zeros zeros zeros zeros (I.repeat (1 :: Int)) zeros zeros

zipWith7Repeat6 :: Infinite Bool
zipWith7Repeat6 = I.zipWith7 (\x y z t u v w -> v == fromIntegral (x + y + z + t + u + w)) zeros zeros zeros zeros zeros (I.repeat (1 :: Int)) zeros

zipWith7Repeat7 :: Infinite Bool
zipWith7Repeat7 = I.zipWith7 (\x y z t u v w -> w == fromIntegral (x + y + z + t + u + v)) zeros zeros zeros zeros zeros zeros (I.repeat (1 :: Int))

main :: IO ()
main = defaultMain $ testGroup "All"
  [ $(inspectTest $ 'foldrMap `hasNoType` ''Word)
  , $(inspectTest $ 'foldrConsMap `hasNoType` ''Word)
  , $(inspectTest $ 'mapMap `hasNoType` ''Word)
  , $(inspectTest $ 'mapId `hasNoType` ''Word)
  , $(inspectTest $ 'mapCoerce ==- 'mapId)
  , $(inspectTest $ 'headIterate `hasNoType` ''Infinite)
  , $(inspectTest $ 'foldrIterate `hasNoType` ''Infinite)
  , $(inspectTest $ 'foldrIterate' `hasNoType` ''Infinite)
  , $(inspectTest $ 'foldrRepeat `hasNoType` ''Infinite)
  , $(inspectTest $ 'headFilterIterate `hasNoType` ''Infinite)
  , $(inspectTest $ 'filterFilter ==- 'filterFilter')
  , $(inspectTest $ 'foldrScanl `hasNoType` ''Word)
  , $(inspectTest $ 'foldrScanl' `hasNoType` ''Word)
  , $(inspectTest $ 'takeRepeat `hasNoType` ''Infinite)
  , $(inspectTest $ 'takeDropRepeat `hasNoType` ''Infinite)
  , $(inspectTest $ 'genericTakeGenericDropRepeat `hasNoType` ''Infinite)
  , $(inspectTest $ 'takeWhileIterate `hasNoType` ''Infinite)
  , $(inspectTest $ 'takeWhileDropWhileIterate `hasNoType` ''Infinite)
  , $(inspectTest $ 'foldrCycle `hasNoType` ''Infinite)
  , $(inspectTest $ 'foldrWordsCycle `hasNoType` ''NonEmpty)
  , $(inspectTest $ 'mapAccumLRepeat `hasNoType` ''Word)

  , $(inspectTest $ 'takeFilterIterate `hasNoType` ''Infinite)
  , $(inspectTest $ 'sumTakeFilterIterate `hasNoTypes` [''Infinite, ''[]])
  , $(inspectTest $ 'takeFilterCycle `hasNoType` ''Infinite)
  , $(inspectTest $ 'takeFilterEllipsis3 `hasNoType` ''Infinite)
  , $(inspectTest $ 'takeFilterEllipsis4 `hasNoType` ''Infinite)
  , $(inspectTest $ 'sumTakeFilterEllipsis3 `hasNoTypes` [''Infinite, ''[]])
  , $(inspectTest $ 'sumTakeFilterEllipsis4 `hasNoTypes` [''Infinite, ''[]])

  , $(inspectTest $ 'takeToListFilterIterate `hasNoType` ''Infinite)
  , $(inspectTest $ 'sumTakeToListFilterIterate `hasNoTypes` [''Infinite, ''[]])
  , $(inspectTest $ 'takeToListFilterCycle `hasNoType` ''Infinite)
  , $(inspectTest $ 'takeToListFilterEllipsis3 `hasNoType` ''Infinite)
  , $(inspectTest $ 'takeToListFilterEllipsis4 `hasNoType` ''Infinite)
  , $(inspectTest $ 'sumTakeToListFilterEllipsis3 `hasNoTypes` [''Infinite, ''[]])
  , $(inspectTest $ 'sumTakeToListFilterEllipsis4 `hasNoTypes` [''Infinite, ''[]])

  , $(inspectTest $ 'headFilterMapEllipsis3 `hasNoTypes` [''Infinite, ''[]])
  , $(inspectTest $ 'headFilterMapEllipsis4 `hasNoTypes` [''Infinite, ''[]])
  , $(inspectTest $ 'toListConcatRepeat `hasNoType` ''Infinite)
  , $(inspectTest $ 'toListConcatMapRepeat `hasNoType` ''Infinite)
  , $(inspectTest $ 'toListIntersperseRepeat `hasNoType` ''Infinite)
  , $(inspectTest $ 'toListIntercalateRepeat `hasNoType` ''Infinite)
  , $(inspectTest $ 'headMapZipIterate `hasNoType` ''Word)
  , $(inspectTest $ 'headMapFlipZipIterate `hasNoType` ''Int)

  , $(inspectTest $ 'zipWithRepeat1  `hasNoType` ''Int)
  , $(inspectTest $ 'zipWithRepeat2  `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith3Repeat1 `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith3Repeat2 `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith3Repeat3 `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith4Repeat1 `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith4Repeat2 `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith4Repeat3 `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith4Repeat4 `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith5Repeat1 `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith5Repeat2 `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith5Repeat3 `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith5Repeat4 `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith5Repeat5 `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith6Repeat1 `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith6Repeat2 `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith6Repeat3 `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith6Repeat4 `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith6Repeat5 `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith6Repeat6 `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith7Repeat1 `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith7Repeat2 `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith7Repeat3 `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith7Repeat4 `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith7Repeat5 `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith7Repeat6 `hasNoType` ''Int)
  , $(inspectTest $ 'zipWith7Repeat7 `hasNoType` ''Int)
  ]

invertResult :: TestTree -> TestTree
invertResult = wrapTest (fmap change)
  where
    change r
      | resultSuccessful r
      = r { resultOutcome = Failure TestFailed, resultShortDescription = "FAIL" }
      | otherwise
      = r { resultOutcome = Success, resultShortDescription = "OK", resultDescription = "" }
