-- |
-- Copyright:   (c) 2022 Bodigrim
-- Licence:     BSD3

{-# LANGUAGE PostfixOperators    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-warning-flags #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Monad law, left identity" #-}
{-# HLINT ignore "Monad law, right identity" #-}

module Main where

import Test.QuickCheck.Function
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Bits
import Data.Either
import qualified Data.List as L
import Data.List.Infinite (Infinite(..))
import qualified Data.List.Infinite as I
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Word (Word32)
import Numeric.Natural
import Prelude hiding (Applicative(..))

instance Arbitrary a => Arbitrary (Infinite a) where
  arbitrary = (:<) <$> arbitrary <*> arbitrary
  shrink = const []

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary

trim :: Infinite a -> [a]
trim = I.take 10

trim1 :: Infinite a -> [a]
trim1 = I.take 11

mapMapFusion :: Infinite Int -> Infinite Int
mapMapFusion xs = I.map fromIntegral (I.map fromIntegral xs :: Infinite Word)

mapEither :: (a -> Either b c) -> [a] -> ([b], [c])
mapEither f = foldr (either (first . (:)) (second . (:)) . f) ([], [])

main :: IO ()
main = defaultMain $ testGroup "All"
  [ testProperty "head" $
    \(Blind (xs :: Infinite Int)) ->
      I.head xs === L.head (trim xs)
  , testProperty "tail" $
    \(Blind (xs :: Infinite Int)) ->
      trim (I.tail xs) === L.tail (trim1 xs)
  , testProperty "uncons" $
    \(Blind (xs :: Infinite Int)) ->
      Just (fmap trim (I.uncons xs)) === L.uncons (trim1 xs)

  , testProperty "map" $
    \(applyFun -> f :: Int -> Word) (Blind (xs :: Infinite Int)) ->
      trim (I.map f xs) === L.map f (trim xs)

  , testProperty "fmap" $
    \(applyFun -> f :: Int -> Int) (Blind (xs :: Infinite Int)) ->
      trim (fmap f xs) === fmap f (trim xs)
  , testProperty "<$" $
    \(x :: Word) (Blind (xs :: Infinite Int)) ->
      trim (x <$ xs) === trim (fmap (const x) xs)

  , testProperty "pure" $
    \(applyFun -> f :: Int -> Word) (x :: Int) ->
      trim (pure f <*> pure x) === trim (pure (f x))
  , testProperty "*>" $
    \(Blind (xs :: Infinite Int)) (Blind (ys :: Infinite Word)) ->
      trim (xs *> ys) === trim ((id <$ xs) <*> ys)
  , testProperty "<*" $
    \(Blind (xs :: Infinite Int)) (Blind (ys :: Infinite Word)) ->
      trim (xs <* ys) === trim (liftA2 const xs ys)

  , testProperty ">>= 1" $
    \x ((I.cycle .) . applyFun -> k :: Int -> Infinite Word) ->
      trim (return x >>= k) === trim (k x)
  , testProperty ">>= 2" $
    \(Blind (xs :: Infinite Int)) ->
      trim (xs >>= return) === trim xs
  , testProperty ">>= 3" $
    \(Blind xs) ((I.cycle .) . applyFun -> k :: Int -> Infinite Word)  ((I.cycle .) . applyFun -> h :: Word -> Infinite Char) ->
      trim (xs >>= (k >=> h)) === trim ((xs >>= k) >>= h)
  , testProperty ">>" $
    \(Blind (xs :: Infinite Int)) (Blind (ys :: Infinite Word)) ->
      trim (xs >> ys) === trim ys

  , testProperty "concat" $
    \(Blind (xs :: Infinite (NonEmpty Int))) ->
      trim (I.concat xs) === L.take 10 (L.concatMap NE.toList (I.toList xs))
  , testProperty "concatMap" $
    \(applyFun -> f :: Int -> NonEmpty Word) (Blind xs) ->
      trim (I.concatMap f xs) === L.take 10 (L.concatMap (NE.toList . f) (I.toList xs))

  , testProperty "intersperse" $
    \(x :: Int) (Blind xs) ->
      I.take 19 (I.intersperse x xs) === L.intersperse x (trim xs)
  , testProperty "intersperse laziness 1" $
    I.head (I.intersperse undefined ('q' :< undefined)) === 'q'
  , testProperty "intersperse laziness 2" $
    I.take 2 (I.intersperse 'w' ('q' :< undefined)) === "qw"

  , testProperty "intercalate" $
    \(x :: NonEmpty Int) (Blind xs) ->
      I.take (sum (map length (trim xs)) + 9 * length x) (I.intercalate x xs) === L.intercalate (NE.toList x) (trim xs)
  , testProperty "intercalate laziness 1" $
    I.take 3 (I.intercalate undefined ("foo" :< undefined)) === "foo"
  , testProperty "intercalate laziness 2" $
    I.take 6 (I.intercalate (NE.fromList "bar") ("foo" :< undefined)) === "foobar"

  , testProperty "interleave 1" $
    \(Blind (xs :: Infinite Int)) (Blind ys) ->
      trim (I.map snd (I.filter fst (I.zip (I.cycle (True :| [False])) (I.interleave xs ys)))) === trim xs
  , testProperty "interleave 2" $
    \(Blind (xs :: Infinite Int)) (Blind ys) ->
      trim (I.map snd (I.filter fst (I.zip (I.cycle (False :| [True])) (I.interleave xs ys)))) === trim ys
  , testProperty "interleave laziness" $
    I.head (I.interleave ('a' :< undefined) undefined) === 'a'

  , testProperty "transpose []" $
    \(fmap getBlind -> xss :: [Infinite Int]) -> not (null xss) ==>
      trim (I.transpose xss) === L.transpose (map trim xss)
  , testProperty "transpose NE" $
    \(fmap getBlind -> xss :: NonEmpty (Infinite Int)) ->
      NE.fromList (trim (I.transpose xss)) === NE.transpose (NE.map (NE.fromList . trim) xss)
  , testProperty "transpose laziness 1" $
    I.head (I.transpose ['a' :< undefined, 'b' :< undefined]) === "ab"
  , testProperty "transpose laziness 2" $
    I.head (I.transpose (('a' :< undefined) :| ['b' :< undefined])) === 'a' :| "b"

  , testProperty "subsequences" $
    \(Blind (xs :: Infinite Int)) ->
      I.take 16 (I.subsequences xs) === L.subsequences (I.take 4 xs)
  , testProperty "subsequences laziness 1" $
    I.head (I.subsequences undefined) === ""
  , testProperty "subsequences laziness 2" $
    I.take 2 (I.subsequences ('q' :< undefined)) === ["", "q"]

  , testProperty "permutations" $
    \(Blind (xs :: Infinite Int)) ->
      map (I.take 4) (I.take 24 (I.permutations xs)) === L.permutations (I.take 4 xs)
  , testProperty "permutations laziness" $
    I.take 6 (I.map (I.take 3) (I.permutations ('q' :< 'w' :< 'e' :< undefined))) === ["qwe","wqe","ewq","weq","eqw","qew"]

  , testProperty "... Bool" $
    \(x :: Bool) ->
      trim (x I....) === L.take 10 (L.cycle [x..])
  , testProperty "... Int" $
    \(x :: Int) ->
      trim (x I....) === L.take 10 (L.cycle [x..])
  , testProperty "... Int maxBound" $
    \(NonNegative (x' :: Int)) -> let x = maxBound - x' in
      trim (x I....) === L.take 10 (L.cycle [x..])
  , testProperty "... Word" $
    \(x :: Word) ->
      trim (x I....) === L.take 10 (L.cycle [x..])
  , testProperty "... Word maxBound" $
    \(NonNegative (x' :: Word)) -> let x = maxBound - x' in
      trim (x I....) === L.take 10 (L.cycle [x..])
  , testProperty "... Integer" $
    \(x :: Integer) ->
      trim (x I....) === L.take 10 (L.cycle [x..])
  , testProperty "... Natural" $
    \(NonNegative (x' :: Integer)) -> let x = fromInteger x' :: Natural in
      trim (x I....) === L.take 10 (L.cycle [x..])

  , testProperty ".... Bool" $
    \(x :: Bool) y ->
      trim ((x, y) I.....) === L.take 10 (L.cycle [x, y..])
  , testProperty ".... Int" $
    \(x :: Int) y ->
      trim ((x, y) I.....) === L.take 10 (L.cycle [x, y..]) .&&.
      trim ((maxBound + x, y) I.....) === L.take 10 (L.cycle [maxBound + x, y..]) .&&.
      trim ((x, maxBound + y) I.....) === L.take 10 (L.cycle [x, maxBound + y..]) .&&.
      trim ((maxBound + x, maxBound + y) I.....) === L.take 10 (L.cycle [maxBound + x, maxBound + y..])
  , testProperty ".... Word" $
    \(x :: Word) y ->
      trim ((x, y) I.....) === L.take 10 (L.cycle [x, y..]) .&&.
      trim ((maxBound + x, y) I.....) === L.take 10 (L.cycle [maxBound + x, y..]) .&&.
      trim ((x, maxBound + y) I.....) === L.take 10 (L.cycle [x, maxBound + y..]) .&&.
      trim ((maxBound + x, maxBound + y) I.....) === L.take 10 (L.cycle [maxBound + x, maxBound + y..])
  , testProperty ".... Integer" $
    \(x :: Integer) y ->
      trim ((x, y) I.....) === L.take 10 (L.cycle [x, y..])
  , testProperty ".... Natural" $
    \(NonNegative (x' :: Integer)) (NonNegative (y' :: Integer)) ->
      let x = fromInteger x' :: Natural in let y = fromInteger y' in
        trim ((x, y) I.....) === L.take 10 (L.cycle [x, y..])

  , testProperty "toList" $
    \(Blind (xs :: Infinite Int)) ->
      L.take 10 (I.toList xs) === trim xs

  , testProperty "scanl" $
    \(curry . applyFun -> f :: Word -> Int -> Word) s (Blind xs) ->
      trim1 (I.scanl f s xs) === L.scanl f s (trim xs)
  , testProperty "scanl laziness" $
    I.head (I.scanl undefined 'q' undefined) === 'q'
  , testProperty "scanl'" $
    \(curry . applyFun -> f :: Word -> Int -> Word) s (Blind xs) ->
      trim1 (I.scanl' f s xs) === L.scanl' f s (trim xs)
  , testProperty "scanl' laziness" $
    I.head (I.scanl' undefined 'q' undefined) === 'q'
  , testProperty "scanl1" $
    \(curry . applyFun -> f :: Int -> Int -> Int) (Blind xs) ->
      trim (I.scanl1 f xs) === L.scanl1 f (trim xs)
  , testProperty "scanl1 laziness" $
    I.head (I.scanl1 undefined ('q' :< undefined)) === 'q'

  , testProperty "mapAccumL" $
    \(curry . applyFun -> f :: Bool -> Int -> (Bool, Word)) (Blind xs) ->
      trim (I.mapAccumL f False xs) === snd (L.mapAccumL f False (trim xs))
  , testProperty "mapAccumL laziness" $
    I.head (I.mapAccumL (\_ x -> (undefined, x)) undefined ('q' :< undefined)) === 'q'

  , testProperty "iterate" $
    \(applyFun -> f :: Int -> Int) s ->
      trim (I.iterate f s) === L.take 10 (L.iterate f s)
  , testProperty "iterate laziness" $
      I.head (I.iterate undefined 'q') === 'q'
  , testProperty "iterate'" $
    \(applyFun -> f :: Int -> Int) s ->
      trim (I.iterate' f s) === L.take 10 (L.iterate f s)
  , testProperty "iterate' laziness" $
      I.head (I.iterate' undefined 'q') === 'q'

  , testProperty "repeat" $
    \(s :: Int) ->
      trim (I.repeat s) === L.replicate 10 s

  , testProperty "cycle" $
    \(xs :: NonEmpty Int) ->
      trim (I.cycle xs) === L.take 10 (L.cycle (NE.toList xs))
  , testProperty "cycle laziness" $
    I.head (I.cycle ('q' :| undefined)) === 'q'

  , testProperty "unfoldr" $
    \(applyFun -> f :: Word -> (Int, Word)) s ->
      trim (I.unfoldr f s) === L.take 10 (L.unfoldr (Just . f) s)
  , testProperty "unfoldr laziness" $
    I.head (I.unfoldr (, undefined) 'q') === 'q'

  , testProperty "take" $
    \n (Blind (xs :: Infinite Int)) ->
      L.take 10 (I.take n xs) === L.take n (trim xs)
  , testProperty "take laziness 1" $
    I.take 0 undefined === ""
  , testProperty "take laziness 2" $
    I.take 1 ('q' :< undefined) === "q"
  , testProperty "drop" $
    \n (Blind (xs :: Infinite Int)) ->
      trim (I.drop n xs) === L.drop n (I.take (max n 0 + 10) xs)
  , testProperty "drop laziness" $
    I.head (I.drop 0 ('q' :< undefined)) === 'q'
  , testProperty "splitAt" $
    \n (Blind (xs :: Infinite Int)) ->
      bimap (L.take 10) trim (I.splitAt n xs) ===
        first (L.take 10) (L.splitAt n (I.take (max n 0 + 10) xs))
  , testProperty "splitAt laziness 1" $
    fst (I.splitAt 0 undefined) === ""
  , testProperty "splitAt laziness 2" $
    fst (I.splitAt 1 ('q' :< undefined)) === "q"

  , testProperty "takeWhile" $
    \(applyFun -> f :: Ordering -> Bool) (Blind xs) ->
      L.take 10 (L.takeWhile f (I.foldr (:) xs)) ===
        L.take 10 (I.takeWhile f xs)
  , testProperty "takeWhile laziness 1" $
      L.null (I.takeWhile (const False) ('q' :< undefined))
  , testProperty "takeWhile laziness 2" $
      L.head (I.takeWhile (const True) ('q' :< undefined)) === 'q'
  , testProperty "fst . span" $
    \(applyFun -> f :: Ordering -> Bool) (Blind xs) ->
      let ys = L.take 10 (fst (I.span f xs)) in
        L.take 10 (L.takeWhile f (I.take (length ys + 10) xs)) ===
          L.take 10 (fst (I.span f xs))
  , testProperty "fst . break" $
    \(applyFun -> f :: Ordering -> Bool) (Blind xs) ->
      let ys = L.take 10 (fst (I.break f xs)) in
        L.take 10 (L.takeWhile (not . f) (I.take (length ys + 10) xs)) ===
          L.take 10 (fst (I.break f xs))
  , testProperty "dropWhile" $
    \(applyFun -> f :: Ordering -> Bool) (Blind xs) ->
      trim (L.foldr (:<) (I.dropWhile f xs) (I.takeWhile f xs)) === trim xs
  , testProperty "snd . span" $
    \(applyFun -> f :: Ordering -> Bool) (Blind xs) ->
      trim (L.foldr (:<) (snd (I.span f xs)) (I.takeWhile f xs)) === trim xs
  , testProperty "snd . break" $
    \(applyFun -> f :: Ordering -> Bool) (Blind xs) ->
      trim (L.foldr (:<) (snd (I.break f xs)) (I.takeWhile (not . f) xs)) === trim xs
  , testProperty "span laziness" $
    L.head (fst (I.span (/= '\n') ('q' :< undefined))) === 'q'
  , testProperty "break laziness" $
    L.head (fst (I.break (== '\n') ('q' :< undefined))) === 'q'

  , testProperty "stripPrefix" $
    \(xs :: [Int]) (Blind (ys :: Infinite Int)) ->
      fmap trim (I.stripPrefix xs ys) === fmap (L.take 10) (L.stripPrefix xs (I.take (length xs + 10) ys))
  , testProperty "stripPrefix laziness 1" $
    isNothing (I.stripPrefix ('q' : undefined) ('w' :< undefined))
  , testProperty "stripPrefix laziness 2" $
    isJust (I.stripPrefix "foo" ('f' :< 'o' :< 'o' :< undefined))
  , testProperty "isPrefixOf" $
    \(xs :: [Int]) (Blind (ys :: Infinite Int)) ->
      I.isPrefixOf xs ys === L.isPrefixOf xs (I.take (length xs + 10) ys)
  , testProperty "isPrefixOf laziness 1" $
    I.isPrefixOf "" undefined
  , testProperty "isPrefixOf laziness 2" $
    not (I.isPrefixOf ('q' : undefined) ('w' :< undefined))
  , testProperty "isPrefixOf laziness 3" $
    I.isPrefixOf "foo" ('f' :< 'o' :< 'o' :< undefined)

  , testProperty "zip" $
    \(Blind (xs1 :: Infinite Int)) (Blind (xs2 :: Infinite Word)) ->
      trim (I.zip xs1 xs2) === L.zip (trim xs1) (trim xs2)
  , testProperty "zip3" $
    \(Blind (xs1 :: Infinite Int)) (Blind (xs2 :: Infinite Word)) (Blind (xs3 :: Infinite Bool)) ->
      trim (I.zip3 xs1 xs2 xs3) === L.zip3 (trim xs1) (trim xs2) (trim xs3)
  , testProperty "zip4" $
    \(Blind (xs1 :: Infinite Int)) (Blind (xs2 :: Infinite Word)) (Blind (xs3 :: Infinite Bool)) (Blind (xs4 :: Infinite Char)) ->
      trim (I.zip4 xs1 xs2 xs3 xs4) === L.zip4 (trim xs1) (trim xs2) (trim xs3) (trim xs4)
  , testProperty "zip5" $
    \(Blind (xs1 :: Infinite Int)) (Blind (xs2 :: Infinite Word)) (Blind (xs3 :: Infinite Bool)) (Blind (xs4 :: Infinite Char)) (Blind (xs5 :: Infinite Ordering)) ->
      trim (I.zip5 xs1 xs2 xs3 xs4 xs5) === L.zip5 (trim xs1) (trim xs2) (trim xs3) (trim xs4) (trim xs5)
  , testProperty "zip6" $
    \(Blind (xs1 :: Infinite Int)) (Blind (xs2 :: Infinite Word)) (Blind (xs3 :: Infinite Bool)) (Blind (xs4 :: Infinite Char)) (Blind (xs5 :: Infinite Ordering)) (Blind (xs6 :: Infinite String)) ->
      trim (I.zip6 xs1 xs2 xs3 xs4 xs5 xs6) === L.zip6 (trim xs1) (trim xs2) (trim xs3) (trim xs4) (trim xs5) (trim xs6)
  , testProperty "zip7" $
    \(Blind (xs1 :: Infinite Int)) (Blind (xs2 :: Infinite Word)) (Blind (xs3 :: Infinite Bool)) (Blind (xs4 :: Infinite Char)) (Blind (xs5 :: Infinite Ordering)) (Blind (xs6 :: Infinite String)) (Blind (xs7 :: Infinite Integer)) ->
      trim (I.zip7 xs1 xs2 xs3 xs4 xs5 xs6 xs7) === L.zip7 (trim xs1) (trim xs2) (trim xs3) (trim xs4) (trim xs5) (trim xs6) (trim xs7)

  , testProperty "unzip" $
    \(Blind (xs :: Infinite (Int, Word))) ->
      bimap trim trim (I.unzip xs) === L.unzip (trim xs)
  , testProperty "unzip3" $
    \(Blind (xs :: Infinite (Int, Word, Bool))) ->
      (\(xs1, xs2, xs3) -> (trim xs1, trim xs2, trim xs3)) (I.unzip3 xs) === L.unzip3 (trim xs)
  , testProperty "unzip4" $
    \(Blind (xs :: Infinite (Int, Word, Bool, Char))) ->
      (\(xs1, xs2, xs3, xs4) -> (trim xs1, trim xs2, trim xs3, trim xs4)) (I.unzip4 xs) === L.unzip4 (trim xs)
  , testProperty "unzip5" $
    \(Blind (xs :: Infinite (Int, Word, Bool, Char, Ordering))) ->
      (\(xs1, xs2, xs3, xs4, xs5) -> (trim xs1, trim xs2, trim xs3, trim xs4, trim xs5)) (I.unzip5 xs) === L.unzip5 (trim xs)
  , testProperty "unzip6" $
    \(Blind (xs :: Infinite (Int, Word, Bool, Char, Ordering, String))) ->
      (\(xs1, xs2, xs3, xs4, xs5, xs6) -> (trim xs1, trim xs2, trim xs3, trim xs4, trim xs5, trim xs6)) (I.unzip6 xs) === L.unzip6 (trim xs)
  , testProperty "unzip7" $
    \(Blind (xs :: Infinite (Int, Word, Bool, Char, Ordering, String, Integer))) ->
      (\(xs1, xs2, xs3, xs4, xs5, xs6, xs7) -> (trim xs1, trim xs2, trim xs3, trim xs4, trim xs5, trim xs6, trim xs7)) (I.unzip7 xs) === L.unzip7 (trim xs)

  , testProperty "lines" $
    \(Blind (xs :: Infinite Char)) ->
      I.take 3 (I.lines xs) === L.take 3 (L.lines (I.foldr (:) xs))
  , testProperty "lines laziness 1" $
    L.head (I.head (I.lines ('q' :< undefined))) === 'q'
  , testProperty "lines laziness 2" $
    L.null (I.head (I.lines ('\n' :< undefined)))
  , testProperty "words" $
    \(Blind (xs :: Infinite Char)) ->
      I.take 3 (I.map NE.toList (I.words xs)) === L.take 3 (L.words (I.foldr (:) xs))
  , testProperty "words laziness" $
    NE.head (I.head (I.words ('q' :< undefined))) === 'q'
  , testProperty "unlines" $
    \(Blind (xs :: Infinite [Char])) ->
      trim (I.unlines xs) === L.take 10 (L.unlines (trim xs))
  , testProperty "unlines laziness" $
    I.take 2 (I.unlines ("q" :< undefined)) === "q\n"
  , testProperty "unwords" $
    \(Blind (xs :: Infinite (NonEmpty Char))) ->
      trim (I.unwords xs) === L.take 10 (L.unwords (L.map NE.toList (I.foldr (:) xs)))
  , testProperty "unwords laziness" $
    I.take 2 (I.unwords (('q' :| []) :< undefined)) === "q "
  , testProperty "unlines . lines" $
    \(Blind (xs :: Infinite Char)) ->
      I.take 100 xs === I.take 100 (I.unlines (I.lines xs))

  , testProperty "group" $
    \(Blind (ys :: Infinite Ordering)) ->
      trim (I.group ys) === L.take 10 (NE.group (I.foldr (:) ys))
  , testProperty "groupBy" $
    \(curry . applyFun -> f :: Ordering -> Ordering -> Bool) (Blind ys) ->
      all (\x -> not $ all (f x) [minBound..maxBound]) [minBound..maxBound] ==>
        trim (I.groupBy f ys) === L.take 10 (NE.groupBy f (I.foldr (:) ys))
  , testProperty "group laziness" $
    NE.head (I.head (I.group ('q' :< undefined))) === 'q'
  , testProperty "nub" $
    \(Blind (ys :: Infinite (Large Int))) ->
      fmap getLarge (I.take 3 (I.nub ys)) === fmap getLarge (L.take 3 (L.nub (I.foldr (:) ys)))
  , testProperty "nub laziness" $
    I.head (I.nub ('q' :< undefined)) === 'q'

  , testProperty "delete" $
    \(x :: Ordering) (Blind xs) ->
      trim (I.delete x xs) === L.take 10 (L.delete x (I.foldr (:) xs))
  , testProperty "delete laziness" $
    I.head (I.delete 'q' ('w' :< undefined)) === 'w'
  , testProperty "insert" $
    \(x :: Int) (Blind xs) ->
      trim (I.insert x xs) === L.take 10 (L.insert x (I.foldr (:) xs))
  , testProperty "insert laziness" $
    I.take 2 (I.insert 'q' ('w' :< undefined)) === "qw"

  , testProperty "\\\\" $
    \(Blind (xs :: Infinite Ordering)) ys ->
      trim (xs I.\\ ys) === L.take 10 (I.foldr (:) xs L.\\ ys)
  , testProperty "\\\\ laziness" $
    I.head (('q' :< undefined) I.\\ []) === 'q'
  , testProperty "union" $
    \xs (Blind (ys :: Infinite Ordering)) ->
      I.take 3 (I.union xs ys) === L.take 3 (xs `L.union` I.foldr (:) ys)
  , testProperty "union laziness" $
    I.head (I.union ('q' : undefined) undefined) === 'q'
  , testProperty "intersect" $
    \(Blind (xs :: Infinite Ordering)) ys -> not (null ys) ==>
      I.head (I.intersect xs ys) === L.head (I.foldr (:) xs `L.intersect` ys)
  , testProperty "intersect laziness" $
    I.head (I.intersect ('q' :< undefined) ('q' : undefined)) === 'q'

  , testProperty "inits" $
    \(Blind (xs :: Infinite Int)) ->
      I.take 21 (I.inits xs) === L.inits (I.take 20 xs)
  , testProperty "inits laziness 1" $
    L.null (I.head (I.inits undefined))
  , testProperty "inits laziness 2" $
    I.take 2 (I.inits ('q' :< undefined)) === ["", "q"]
  , testProperty "inits1" $
    \(Blind (xs :: Infinite Int)) ->
      map NE.toList (trim (I.inits1 xs)) === L.tail (L.inits (trim xs))
  , testProperty "tails" $
    \(Blind (xs :: Infinite Int)) ->
      map trim (trim (I.tails xs)) === map (L.take 10) (L.take 10 (L.tails (I.take 20 xs)))
  , testProperty "tails laziness" $
    I.head (I.head (I.tails ('q' :< undefined))) === 'q'

  , testProperty "lookup" $
    \(xs :: [(Int, Word)]) y zs ->
      let pairs = NE.fromList (xs ++ (y : zs)) in
        Just (I.lookup (fst y) (I.cycle pairs)) === L.lookup (fst y) (NE.toList pairs)
  , testProperty "lookup laziness" $
    I.lookup True ((True, 'q') :< undefined) === 'q'
  , testProperty "find" $
    \(xs :: [(Int, Word)]) y zs ->
      let pairs = NE.fromList (xs ++ (y : zs)) in
        Just (I.find ((== snd y) . snd) (I.cycle pairs)) === L.find ((== snd y) . snd) (NE.toList pairs)
  , testProperty "find laziness" $
    I.find odd (1 :< undefined) === (1 :: Int)

  , testProperty "filter" $
    \(applyFun -> f :: Int -> Bool) xs (Blind ys) ->
      let us = L.filter f xs in
        us === I.take (length us) (I.filter f (I.prependList xs ys))
  , testProperty "mapMaybe" $
    \(applyFun -> f :: Int -> Maybe Word) xs (Blind ys) ->
      let us = mapMaybe f xs in
        us === I.take (length us) (I.mapMaybe f (I.prependList xs ys))
  , testProperty "catMaybes" $
    \(xs :: [Maybe Word]) (Blind ys) ->
      let us = catMaybes xs in
        us === I.take (length us) (I.catMaybes (I.prependList xs ys))
  , testProperty "partition" $
    \(applyFun -> f :: Int -> Bool) xs (Blind ys) ->
      let (us, vs) = L.partition f xs in
        let (us', vs') = I.partition f (I.prependList xs ys) in
          us === I.take (length us) us' .&&. vs === I.take (length vs) vs'
  , testProperty "mapEither" $
    \(applyFun -> f :: Int -> Either Word Char) xs (Blind ys) ->
      let (us, vs) = mapEither f xs in
        let (us', vs') = I.mapEither f (I.prependList xs ys) in
          us === I.take (length us) us' .&&. vs === I.take (length vs) vs'
  , testProperty "partitionEithers" $
    \(xs :: [Either Word Char]) (Blind ys) ->
      let (us, vs) = partitionEithers xs in
        let (us', vs') = I.partitionEithers (I.prependList xs ys) in
          us === I.take (length us) us' .&&. vs === I.take (length vs) vs'

  , testProperty "!!" $
    \(Blind (xs :: Infinite Int)) n ->
      xs I.!! n === I.foldr (:) xs L.!! fromIntegral n
  , testProperty "tabulate" $
    \(applyFun -> f :: Word -> Char) n ->
      I.tabulate f I.!! n === f n

  , testProperty "elemIndex" $
    \xs (x :: Int) (Blind ys) ->
      let zs = I.prependList xs (x :< ys) in
        Just (fromIntegral (I.elemIndex x zs)) === L.elemIndex x (I.foldr (:) zs)
  , testProperty "elemIndices" $
    \xs (x :: Ordering) (Blind ys) ->
      let zs = I.prependList xs (x :< ys) in
        let is = L.elemIndices x (xs ++ [x]) in
          map fromIntegral (I.take (length is) (I.elemIndices x zs)) === is

  , testProperty ">>= 32bit" $
    let ix = maxBound :: Word32 in
      finiteBitSize (0 :: Word) /= 32 ||
        I.head (I.tail (I.genericDrop ix (I.repeat () >>= const (False :< I.repeat True))))
  ]
