{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}

-- |
-- Copyright:   (c) 2022 Bodigrim
-- License:     BSD3
--
-- Modern lightweight library for infinite lists with fusion:
--
-- * API similar to "Data.List".
-- * No non-boot dependencies.
-- * Top performance, driven by fusion.
-- * Avoid dangerous instances like `Data.Foldable.Foldable`.
-- * Use `NonEmpty` where applicable.
-- * Use `Word` for indices.
-- * Be lazy, but not too lazy.
--
-- @
-- {\-# LANGUAGE PostfixOperators #-\}
-- import Data.List.Infinite (Infinite(..), (...), (....))
-- import qualified Data.List.Infinite as Inf
-- @
module Data.List.Infinite (
  -- * Construction
  Infinite (..),

  -- * Elimination
  head,
  tail,
  uncons,
  toList,
  foldr,

  -- * Traversals
  map,
  scanl,
  scanl',
  scanl1,
  mapAccumL,

  -- * Transformations
  concat,
  concatMap,
  intersperse,
  intercalate,
  interleave,
  transpose,
  subsequences,
  subsequences1,
  permutations,

  -- * Building
  (...),
  (....),
  iterate,
  iterate',
  unfoldr,
  tabulate,
  repeat,
  cycle,

  -- * Sublists
  prependList,
  take,
  drop,
  splitAt,
  takeWhile,
  dropWhile,
  span,
  break,
  group,
  inits,
  inits1,
  tails,
  isPrefixOf,
  stripPrefix,

  -- * Searching
  filter,
  lookup,
  find,
  mapMaybe,
  catMaybes,
  partition,
  mapEither,
  partitionEithers,

  -- * Indexing
  (!!),
  elemIndex,
  elemIndices,
  findIndex,
  findIndices,

  -- * Zipping
  zip,
  zipWith,
  zip3,
  zipWith3,
  zip4,
  zipWith4,
  zip5,
  zipWith5,
  zip6,
  zipWith6,
  zip7,
  zipWith7,
  unzip,
  unzip3,
  unzip4,
  unzip5,
  unzip6,
  unzip7,

  -- * Functions on strings
  lines,
  words,
  unlines,
  unwords,

  -- * Set operations
  nub,
  delete,
  (\\),
  union,
  intersect,

  -- * Ordered lists
  insert,

  -- * Generalized functions
  nubBy,
  deleteBy,
  deleteFirstsBy,
  unionBy,
  intersectBy,
  groupBy,
  insertBy,
  genericTake,
  genericDrop,
  genericSplitAt,
) where

import Control.Applicative (Applicative (..))
import Control.Arrow (first, second)
import Control.Monad (Monad (..))
import Data.Bits ((.&.))
import Data.Char (Char, isSpace)
import Data.Coerce (coerce)
import Data.Either (Either, either)
import Data.Eq (Eq, (/=), (==))
import qualified Data.Foldable as F
import Data.Functor (Functor (..))
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (maybe)
import Data.Ord (Ord, Ordering (..), compare, (<), (<=), (>), (>=))
import qualified GHC.Exts
import Numeric.Natural (Natural)
import Prelude (Bool (..), Enum, Int, Integer, Integral, Maybe (..), Word, const, enumFrom, enumFromThen, flip, id, maxBound, minBound, not, otherwise, snd, uncurry, (&&), (+), (-), (.), (||))

#if MIN_VERSION_base(4,10,0)
import GHC.Exts (oneShot)
#else
import GHC.Magic (oneShot)
#endif

import Data.List.Infinite.Internal
import Data.List.Infinite.Zip

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

-- | Paramorphism on infinite lists.
para :: forall a b. (a -> Infinite a -> b -> b) -> Infinite a -> b
para f = go
  where
    go :: Infinite a -> b
    go (x :< xs) = f x xs (go xs)

-- | Convert to a list. Use 'cycle' to go in the opposite direction.
toList :: Infinite a -> [a]
toList = foldr (:)
{-# NOINLINE [0] toList #-}

{-# RULES
"toList" [~1] forall xs.
  toList xs =
    GHC.Exts.build (\cons -> const (foldr cons xs))
  #-}

-- | Generate an infinite progression, starting from a given element,
-- similar to @[x..]@.
-- For better user experience consider enabling @{\-# LANGUAGE PostfixOperators #-\}@:
--
-- >>> :set -XPostfixOperators
-- >>> Data.List.Infinite.take 10 (0...)
-- [0,1,2,3,4,5,6,7,8,9]
--
-- Beware that for finite types '(...)' applies 'cycle' atop of @[x..]@:
--
-- >>> :set -XPostfixOperators
-- >>> Data.List.Infinite.take 10 (EQ...)
-- [EQ,GT,EQ,GT,EQ,GT,EQ,GT,EQ,GT]
--
-- Remember that 'Int' is a finite type as well. One is unlikely to hit this
-- on a 64-bit architecture, but on a 32-bit machine it's fairly possible to traverse
-- @((0 :: 'Int') ...)@ far enough to encounter @0@ again.
(...) :: Enum a => a -> Infinite a
(...) = unsafeCycle . enumFrom
{-# INLINE [0] (...) #-}

infix 0 ...

{-# RULES
"ellipsis3Int" (...) = ellipsis3Int
"ellipsis3Word" (...) = ellipsis3Word
"ellipsis3Integer" (...) = ellipsis3Integer
"ellipsis3Natural" (...) = ellipsis3Natural
  #-}

ellipsis3Int :: Int -> Infinite Int
ellipsis3Int from = iterate' (\n -> if n == maxBound then from else n + 1) from
{-# INLINE ellipsis3Int #-}

ellipsis3Word :: Word -> Infinite Word
ellipsis3Word from = iterate' (\n -> if n == maxBound then from else n + 1) from
{-# INLINE ellipsis3Word #-}

ellipsis3Integer :: Integer -> Infinite Integer
ellipsis3Integer = iterate' (+ 1)
{-# INLINE ellipsis3Integer #-}

ellipsis3Natural :: Natural -> Infinite Natural
ellipsis3Natural = iterate' (+ 1)
{-# INLINE ellipsis3Natural #-}

-- | Generate an infinite arithmetic progression, starting from given elements,
-- similar to @[x,y..]@.
-- For better user experience consider enabling @{\-# LANGUAGE PostfixOperators #-\}@:
--
-- >>> :set -XPostfixOperators
-- >>> Data.List.Infinite.take 10 ((1,3)....)
-- [1,3,5,7,9,11,13,15,17,19]
--
-- Beware that for finite types '(....)' applies 'cycle' atop of @[x,y..]@:
--
-- >>> :set -XPostfixOperators
-- >>> Data.List.Infinite.take 10 ((EQ,GT)....)
-- [EQ,GT,EQ,GT,EQ,GT,EQ,GT,EQ,GT]
--
-- Remember that 'Int' is a finite type as well: for a sufficiently large
-- step of progression @y - x@ one may observe @((x :: Int, y)....)@ cycling back
-- to emit @x@ fairly soon.
(....) :: Enum a => (a, a) -> Infinite a
(....) = unsafeCycle . uncurry enumFromThen
{-# INLINE [0] (....) #-}

infix 0 ....

{-# RULES
"ellipsis4Int" (....) = ellipsis4Int
"ellipsis4Word" (....) = ellipsis4Word
"ellipsis4Integer" (....) = ellipsis4Integer
"ellipsis4Natural" (....) = ellipsis4Natural
  #-}

ellipsis4Int :: (Int, Int) -> Infinite Int
ellipsis4Int (from, thn)
  | from <= thn =
      let d = thn - from
       in iterate' (\n -> if n > maxBound - d then from else n + d) from
  | otherwise =
      let d = from - thn
       in iterate' (\n -> if n < minBound + d then from else n - d) from
{-# INLINE ellipsis4Int #-}

ellipsis4Word :: (Word, Word) -> Infinite Word
ellipsis4Word (from, thn)
  | from <= thn =
      let d = thn - from
       in iterate' (\n -> if n > maxBound - d then from else n + d) from
  | otherwise =
      let d = from - thn
       in iterate' (\n -> if n < d then from else n - d) from
{-# INLINE ellipsis4Word #-}

ellipsis4Integer :: (Integer, Integer) -> Infinite Integer
ellipsis4Integer (from, thn) = iterate' (+ (thn - from)) from
{-# INLINE ellipsis4Integer #-}

ellipsis4Natural :: (Natural, Natural) -> Infinite Natural
ellipsis4Natural (from, thn)
  | from <= thn =
      iterate' (+ (thn - from)) from
  | otherwise =
      let d = from - thn
       in iterate' (\n -> if n < d then from else n - d) from
{-# INLINE ellipsis4Natural #-}

-- | Just a pointwise 'map'.
instance Functor Infinite where
  fmap = map
  (<$) = const . repeat

-- | This instance operates pointwise, similar to 'Control.Applicative.ZipList'.
instance Applicative Infinite where
  pure = repeat
  (f :< fs) <*> (x :< xs) = f x :< (fs <*> xs)
  (<*) = const
  (*>) = const id
#if MIN_VERSION_base(4,10,0)
  liftA2 = zipWith
#endif

-- | 'Control.Applicative.ZipList' cannot be made a lawful 'Monad',
-- but 'Infinite', being a
-- [@Representable@](https://hackage.haskell.org/package/adjunctions/docs/Data-Functor-Rep.html#t:Representable),
-- can. Namely, 'Control.Monad.join'
-- picks up a diagonal of an infinite matrix of 'Infinite' ('Infinite' @a@).
-- Bear in mind that this instance gets slow
-- very soon because of linear indexing, so it is not recommended to be used
-- in practice.
instance Monad Infinite where
  xs >>= f = go 0 xs
    where
      go !n (y :< ys) = (f y `index` n) :< go (n + 1) ys
      index :: Infinite a -> Natural -> a
      index ys n = head (genericDrop n ys)
  {-# INLINE (>>=) #-}
  (>>) = (*>)

-- | Get the first elements of an infinite list.
head :: Infinite a -> a
head (x :< _) = x
{-# NOINLINE [1] head #-}

{-# RULES
"head/build" forall (g :: forall b. (a -> b -> b) -> b).
  head (build g) =
    g const
  #-}

-- | Get the elements of an infinite list after the first one.
tail :: Infinite a -> Infinite a
tail (_ :< xs) = xs

-- | Split an infinite list into its 'head' and 'tail'.
uncons :: Infinite a -> (a, Infinite a)
uncons (x :< xs) = (x, xs)

-- | Apply a function to every element of an infinite list.
map :: (a -> b) -> Infinite a -> Infinite b
map = foldr . ((:<) .)

mapFB :: (elt -> lst -> lst) -> (a -> elt) -> a -> lst -> lst
mapFB = (.)

{-# NOINLINE [0] map #-}

{-# INLINE [0] mapFB #-}

{-# RULES
"map" [~1] forall f xs.
  map f xs =
    build (\cons -> foldr (mapFB cons f) xs)
"mapList" [1] forall f.
  foldr (mapFB (:<) f) =
    map f
"mapFB" forall cons f g.
  mapFB (mapFB cons f) g =
    mapFB cons (f . g)
"map/coerce" [1]
  map coerce =
    coerce
  #-}

-- | Flatten out an infinite list of non-empty lists.
--
-- The peculiar type with 'NonEmpty' is to guarantee that 'concat'
-- is productive and results in an infinite list. Otherwise the
-- concatenation of infinitely many @[a]@ could still be a finite list.
concat :: Infinite (NonEmpty a) -> Infinite a
concat = foldr (\(x :| xs) acc -> x :< (xs `prependList` acc))
{-# NOINLINE [1] concat #-}

{-# RULES
"concat" forall xs.
  concat xs =
    build (\cons -> foldr (flip (F.foldr cons)) xs)
  #-}

-- | First 'map' every element, then 'concat'.
--
-- The peculiar type with 'NonEmpty' is to guarantee that 'concatMap'
-- is productive and results in an infinite list. Otherwise the
-- concatenation of infinitely many @[b]@ could still be a finite list.
concatMap :: (a -> NonEmpty b) -> Infinite a -> Infinite b
concatMap f = foldr (\a acc -> let (x :| xs) = f a in x :< (xs `prependList` acc))
{-# NOINLINE [1] concatMap #-}

{-# RULES
"concatMap" forall f xs.
  concatMap f xs =
    build (\cons -> foldr (flip (F.foldr cons) . f) xs)
  #-}

-- | Interleave two infinite lists.
interleave :: Infinite a -> Infinite a -> Infinite a
interleave (x :< xs) ys = x :< interleave ys xs

-- | Insert an element between adjacent elements of an infinite list.
intersperse :: a -> Infinite a -> Infinite a
intersperse a = foldr (\x -> (x :<) . (a :<))
{-# NOINLINE [1] intersperse #-}

{-# RULES
"intersperse" forall a xs.
  intersperse a xs =
    build (\cons -> foldr (\x -> cons x . cons a) xs)
  #-}

-- | Insert a non-empty list between adjacent elements of an infinite list,
-- and subsequently flatten it out.
--
-- The peculiar type with 'NonEmpty' is to guarantee that 'intercalate'
-- is productive and results in an infinite list. If separator is an empty list,
-- concatenation of infinitely many @[a]@ could still be a finite list.
intercalate :: NonEmpty a -> Infinite [a] -> Infinite a
intercalate ~(a :| as) = foldr (\xs -> prependList xs . (a :<) . prependList as)
{-# NOINLINE [1] intercalate #-}

{-# RULES
"intercalate" forall as xss.
  intercalate as xss =
    build (\cons -> foldr (\xs acc -> F.foldr cons (F.foldr cons acc as) xs) xss)
  #-}

-- | Transpose rows and columns of an argument.
--
-- This is actually @distribute@ from
-- [@Distributive@](https://hackage.haskell.org/package/distributive/docs/Data-Distributive.html#t:Distributive)
-- type class in disguise.
transpose :: Functor f => f (Infinite a) -> Infinite (f a)
transpose xss = fmap head xss :< transpose (fmap tail xss)

-- | Generate an infinite list of all subsequences of the argument.
subsequences :: Infinite a -> Infinite [a]
subsequences = ([] :<) . map NE.toList . subsequences1

-- | Generate an infinite list of all non-empty subsequences of the argument.
subsequences1 :: Infinite a -> Infinite (NonEmpty a)
subsequences1 = foldr go
  where
    go :: a -> Infinite (NonEmpty a) -> Infinite (NonEmpty a)
    go x sxs = (x :| []) :< foldr f sxs
      where
        f ys r = ys :< (x `NE.cons` ys) :< r

-- | Generate an infinite list of all permutations of the argument.
permutations :: Infinite a -> Infinite (Infinite a)
permutations xs0 = xs0 :< perms xs0 []
  where
    perms :: forall a. Infinite a -> [a] -> Infinite (Infinite a)
    perms (t :< ts) is = List.foldr interleaveList (perms ts (t : is)) (List.permutations is)
      where
        interleaveList :: [a] -> Infinite (Infinite a) -> Infinite (Infinite a)
        interleaveList = (snd .) . interleaveList' id

        interleaveList' :: (Infinite a -> b) -> [a] -> Infinite b -> (Infinite a, Infinite b)
        interleaveList' _ [] r = (ts, r)
        interleaveList' f (y : ys) r = (y :< us, f (t :< y :< us) :< zs)
          where
            (us, zs) = interleaveList' (f . (y :<)) ys r

-- | Fold an infinite list from the left and return a list of successive reductions,
-- starting from the initial accumulator:
--
-- > scanl f acc (x1 :< x2 :< ...) = acc :< f acc x1 :< f (f acc x1) x2 :< ...
scanl :: (b -> a -> b) -> b -> Infinite a -> Infinite b
scanl f z0 = (z0 :<) . flip (foldr (\x acc z -> let fzx = f z x in fzx :< acc fzx)) z0

scanlFB :: (elt' -> elt -> elt') -> (elt' -> lst -> lst) -> elt -> (elt' -> lst) -> elt' -> lst
scanlFB f cons = \elt g -> oneShot (\x -> let elt' = f x elt in elt' `cons` g elt')

{-# NOINLINE [1] scanl #-}

{-# INLINE [0] scanlFB #-}

{-# RULES
"scanl" [~1] forall f a bs.
  scanl f a bs =
    build (\cons -> a `cons` foldr (scanlFB f cons) bs a)
"scanlList" [1] forall f (a :: a) bs.
  foldr (scanlFB f (:<)) bs a =
    tail (scanl f a bs)
  #-}

-- | Same as 'scanl', but strict in accumulator.
scanl' :: (b -> a -> b) -> b -> Infinite a -> Infinite b
scanl' f z0 = (z0 :<) . flip (foldr (\x acc z -> let !fzx = f z x in fzx :< acc fzx)) z0

scanlFB' :: (elt' -> elt -> elt') -> (elt' -> lst -> lst) -> elt -> (elt' -> lst) -> elt' -> lst
scanlFB' f cons = \elt g -> oneShot (\x -> let !elt' = f x elt in elt' `cons` g elt')

{-# NOINLINE [1] scanl' #-}

{-# INLINE [0] scanlFB' #-}

{-# RULES
"scanl'" [~1] forall f a bs.
  scanl' f a bs =
    build (\cons -> a `cons` foldr (scanlFB' f cons) bs a)
"scanlList'" [1] forall f (a :: a) bs.
  foldr (scanlFB' f (:<)) bs a =
    tail (scanl' f a bs)
  #-}

-- | Fold an infinite list from the left and return a list of successive reductions,
-- starting from the first element:
--
-- > scanl1 f (x0 :< x1 :< x2 :< ...) = x0 :< f x0 x1 :< f (f x0 x1) x2 :< ...
scanl1 :: (a -> a -> a) -> Infinite a -> Infinite a
scanl1 f (x :< xs) = scanl f x xs

-- | Fold an infinite list from the left and return a list of successive reductions,
-- keeping accumulator in a state:
--
-- > mapAccumL f acc0 (x1 :< x2 :< ...) =
-- >   let (acc1, y1) = f acc0 x1 in
-- >     let (acc2, y2) = f acc1 x2 in
-- >       ...
-- >         y1 :< y2 :< ...
--
-- If you are looking how to traverse with a state, look no further.
mapAccumL :: (acc -> x -> (acc, y)) -> acc -> Infinite x -> Infinite y
mapAccumL f = flip (foldr (\x acc s -> let (s', y) = f s x in y :< acc s'))

mapAccumLFB :: (acc -> x -> (acc, y)) -> x -> (acc -> Infinite y) -> acc -> Infinite y
mapAccumLFB f = \x r -> oneShot (\s -> let (s', y) = f s x in y :< r s')

{-# NOINLINE [1] mapAccumL #-}

{-# INLINE [0] mapAccumLFB #-}

{-# RULES
"mapAccumL" [~1] forall f s xs.
  mapAccumL f s xs =
    foldr (mapAccumLFB f) xs s
"mapAccumLList" [1] forall f s xs.
  foldr (mapAccumLFB f) xs s =
    mapAccumL f s xs
  #-}

-- | Generate an infinite list of repeated applications.
iterate :: (a -> a) -> a -> Infinite a
iterate f = go
  where
    go x = x :< go (f x)

iterateFB :: (elt -> lst -> lst) -> (elt -> elt) -> elt -> lst
iterateFB cons f = go
  where
    go x = x `cons` go (f x)

{-# NOINLINE [1] iterate #-}

{-# INLINE [0] iterateFB #-}

{-# RULES
"iterate" [~1] forall f x. iterate f x = build (\cons -> iterateFB cons f x)
"iterateFB" [1] iterateFB (:<) = iterate
  #-}

-- | Same as 'iterate', but strict in accumulator.
iterate' :: (a -> a) -> a -> Infinite a
iterate' f = go
  where
    go !x = x :< go (f x)

iterateFB' :: (elt -> lst -> lst) -> (elt -> elt) -> elt -> lst
iterateFB' cons f = go
  where
    go !x = x `cons` go (f x)

{-# NOINLINE [1] iterate' #-}

{-# INLINE [0] iterateFB' #-}

{-# RULES
"iterate'" [~1] forall f x. iterate' f x = build (\cons -> iterateFB' cons f x)
"iterateFB'" [1] iterateFB' (:<) = iterate'
  #-}

-- | Repeat the same element ad infinitum.
repeat :: a -> Infinite a
repeat x = go
  where
    go = x :< go

repeatFB :: (elt -> lst -> lst) -> elt -> lst
repeatFB cons x = go
  where
    go = x `cons` go

{-# NOINLINE [1] repeat #-}

{-# INLINE [0] repeatFB #-}

{-# RULES
"repeat" [~1] forall x. repeat x = build (`repeatFB` x)
"repeatFB" [1] repeatFB (:<) = repeat
  #-}

-- | Repeat a non-empty list ad infinitum.
-- If you were looking for something like @fromList :: [a] -> Infinite a@,
-- look no further.
--
-- It would be less annoying to take @[a]@ instead of 'NonEmpty' @a@,
-- but we strive to avoid partial functions.
cycle :: NonEmpty a -> Infinite a
cycle (x :| xs) = unsafeCycle (x : xs)
{-# INLINE cycle #-}

unsafeCycle :: [a] -> Infinite a
unsafeCycle xs = go
  where
    go = xs `prependList` go

unsafeCycleFB :: (elt -> lst -> lst) -> [elt] -> lst
unsafeCycleFB cons xs = go
  where
    go = F.foldr cons go xs

{-# NOINLINE [1] unsafeCycle #-}

{-# INLINE [0] unsafeCycleFB #-}

{-# RULES
"unsafeCycle" [~1] forall x. unsafeCycle x = build (`unsafeCycleFB` x)
"unsafeCycleFB" [1] unsafeCycleFB (:<) = unsafeCycle
  #-}

-- | Build an infinite list from a seed value.
--
-- This is an anamorphism on infinite lists.
unfoldr :: (b -> (a, b)) -> b -> Infinite a
unfoldr f = go
  where
    go b = let (a, b') = f b in a :< go b'
{-# INLINE unfoldr #-}

-- | Generate an infinite list of @f@ 0, @f@ 1, @f@ 2...
--
-- 'tabulate' and '(!!)' witness that 'Infinite' is
-- [@Representable@](https://hackage.haskell.org/package/adjunctions/docs/Data-Functor-Rep.html#t:Representable).
tabulate :: (Word -> a) -> Infinite a
tabulate f = unfoldr (\n -> (f n, n + 1)) 0
{-# INLINE tabulate #-}

-- | Take a prefix of given length.
take :: Int -> Infinite a -> [a]
take = GHC.Exts.inline genericTake
{-# INLINE [1] take #-}

{-# INLINE [1] genericTake #-}

{-# INLINE [0] genericTakeFB #-}

{-# RULES
"take"
  take =
    genericTake
"genericTake" [~1] forall n xs.
  genericTake n xs =
    GHC.Exts.build
      ( \cons nil ->
          if n >= 1
            then foldr (genericTakeFB cons nil) xs n
            else nil
      )
"genericTakeList" [1] forall n xs.
  foldr (genericTakeFB (:) []) xs n =
    genericTake n xs
  #-}

-- | Take a prefix of given length.
genericTake :: Integral i => i -> Infinite a -> [a]
genericTake n
  | n < 1 = const []
  | otherwise = flip (foldr (\hd f m -> hd : (if m <= 1 then [] else f (m - 1)))) n

genericTakeFB :: Integral i => (elt -> lst -> lst) -> lst -> elt -> (i -> lst) -> i -> lst
genericTakeFB cons nil x xs = \m -> if m <= 1 then x `cons` nil else x `cons` xs (m - 1)

-- | Drop a prefix of given length.
drop :: Int -> Infinite a -> Infinite a
drop = GHC.Exts.inline genericDrop

-- | Drop a prefix of given length.
genericDrop :: Integral i => i -> Infinite a -> Infinite a
genericDrop = flip (para (\hd tl f m -> if m < 1 then hd :< tl else f (m - 1)))
{-# INLINEABLE genericDrop #-}

-- | Split an infinite list into a prefix of given length and the rest.
splitAt :: Int -> Infinite a -> ([a], Infinite a)
splitAt = GHC.Exts.inline genericSplitAt

-- | Split an infinite list into a prefix of given length and the rest.
genericSplitAt :: Integral i => i -> Infinite a -> ([a], Infinite a)
genericSplitAt n
  | n < 1 = ([],)
  | otherwise = flip (para (\hd tl f m -> if m <= 1 then ([hd], tl) else first (hd :) (f (m - 1)))) n
{-# INLINEABLE genericSplitAt #-}

-- | Take the longest prefix satisfying a predicate.
takeWhile :: (a -> Bool) -> Infinite a -> [a]
takeWhile p = foldr (\x xs -> if p x then x : xs else [])

takeWhileFB :: (elt -> Bool) -> (elt -> lst -> lst) -> lst -> elt -> lst -> lst
takeWhileFB p cons nil = \x r -> if p x then x `cons` r else nil

{-# NOINLINE [1] takeWhile #-}

{-# INLINE [0] takeWhileFB #-}

{-# RULES
"takeWhile" [~1] forall p xs.
  takeWhile p xs =
    GHC.Exts.build (\cons nil -> foldr (takeWhileFB p cons nil) xs)
"takeWhileList" [1] forall p.
  foldr (takeWhileFB p (:) []) =
    takeWhile p
  #-}

-- | Drop the longest prefix satisfying a predicate.
--
-- This function isn't productive (e. g., 'head' . 'dropWhile' @f@ won't terminate),
-- if all elements of the input list satisfy the predicate.
dropWhile :: (a -> Bool) -> Infinite a -> Infinite a
dropWhile p = para (\x xs -> if p x then id else const (x :< xs))

-- | Split an infinite list into the longest prefix satisfying a predicate and the rest.
--
-- This function isn't productive in the second component of the tuple
-- (e. g., 'head' . 'snd' . 'span' @f@ won't terminate),
-- if all elements of the input list satisfy the predicate.
span :: (a -> Bool) -> Infinite a -> ([a], Infinite a)
span p = para (\x xs -> if p x then first (x :) else const ([], x :< xs))

-- | Split an infinite list into the longest prefix /not/ satisfying a predicate and the rest.
--
-- This function isn't productive in the second component of the tuple
-- (e. g., 'head' . 'snd' . 'break' @f@ won't terminate),
-- if no elements of the input list satisfy the predicate.
break :: (a -> Bool) -> Infinite a -> ([a], Infinite a)
break = span . (not .)

-- | If a list is a prefix of an infinite list, strip it and return the rest.
-- Otherwise return 'Nothing'.
stripPrefix :: Eq a => [a] -> Infinite a -> Maybe (Infinite a)
stripPrefix [] = Just
stripPrefix (p : ps) = flip (para alg) (p :| ps)
  where
    alg x xs acc (y :| ys)
      | x == y = maybe (Just xs) acc (NE.nonEmpty ys)
      | otherwise = Nothing

-- | Group consecutive equal elements.
group :: Eq a => Infinite a -> Infinite (NonEmpty a)
group = groupBy (==)

-- | Overloaded version of 'group'.
groupBy :: (a -> a -> Bool) -> Infinite a -> Infinite (NonEmpty a)
-- Quite surprisingly, 'groupBy' is not a simple catamorphism.
-- Since @f@ is not guaranteed to be transitive, it's a full-blown
-- histomorphism, at which point a manual recursion becomes much more readable.
groupBy f = go
  where
    go (x :< xs) = (x :| ys) :< go zs
      where
        (ys, zs) = span (f x) xs

-- | Generate all prefixes of an infinite list.
inits :: Infinite a -> Infinite [a]
inits =
  map (\(SnocBuilder _ front rear) -> front List.++ List.reverse rear)
    . scanl'
      (\(SnocBuilder count front rear) x -> snocBuilder (count + 1) front (x : rear))
      (SnocBuilder 0 [] [])

data SnocBuilder a = SnocBuilder
  { _count :: !Word
  , _front :: [a]
  , _rear :: [a]
  }

snocBuilder :: Word -> [a] -> [a] -> SnocBuilder a
snocBuilder count front rear
  | count < 8 || (count .&. (count + 1)) /= 0 =
      SnocBuilder count front rear
  | otherwise =
      SnocBuilder count (front List.++ List.reverse rear) []
{-# INLINE snocBuilder #-}

-- | Generate all non-empty prefixes of an infinite list.
inits1 :: Infinite a -> Infinite (NonEmpty a)
inits1 (x :< xs) = map (x :|) (inits xs)

-- | Generate all suffixes of an infinite list.
tails :: Infinite a -> Infinite (Infinite a)
tails = foldr (\x xss@(~(xs :< _)) -> (x :< xs) :< xss)

-- | Check whether a list is a prefix of an infinite list.
isPrefixOf :: Eq a => [a] -> Infinite a -> Bool
isPrefixOf [] = const True
isPrefixOf (p : ps) = flip (foldr alg) (p :| ps)
  where
    alg x acc (y :| ys) = x == y && maybe True acc (NE.nonEmpty ys)

-- | Find the first pair, whose first component is equal to the first argument,
-- and return the second component.
-- If there is nothing to be found, this function will hang indefinitely.
lookup :: Eq a => a -> Infinite (a, b) -> b
lookup a = foldr (\(a', b) b' -> if a == a' then b else b')

-- | Find the first element, satisfying a predicate.
-- If there is nothing to be found, this function will hang indefinitely.
find :: (a -> Bool) -> Infinite a -> a
find f = foldr (\a a' -> if f a then a else a')

-- | Filter an infinite list, removing elements which does not satisfy a predicate.
--
-- This function isn't productive (e. g., 'head' . 'filter' @f@ won't terminate),
-- if no elements of the input list satisfy the predicate.
--
-- A common objection is that since it could happen that no elements of the input
-- satisfy the predicate, the return type should be @[a]@ instead of 'Infinite' @a@.
-- This would not however make 'filter' any more productive. Note that such
-- hypothetical 'filter' could not ever generate @[]@ constructor, only @(:)@, so
-- we would just have a more lax type gaining nothing instead. Same reasoning applies
-- to other filtering \/ partitioning \/ searching functions.
filter :: (a -> Bool) -> Infinite a -> Infinite a
filter f = foldr (\a -> if f a then (a :<) else id)

filterFB :: (elt -> lst -> lst) -> (elt -> Bool) -> elt -> lst -> lst
filterFB cons f x r
  | f x = x `cons` r
  | otherwise = r

{-# NOINLINE [1] filter #-}

{-# INLINE [0] filterFB #-}

{-# RULES
"filter" [~1] forall f xs.
  filter f xs =
    build (\cons -> foldr (filterFB cons f) xs)
"filterList" [1] forall f.
  foldr (filterFB (:<) f) =
    filter f
"filterFB" forall cons f g.
  filterFB (filterFB cons f) g =
    filterFB cons (\x -> f x && g x)
  #-}

-- | Split an infinite list into two infinite lists: the first one contains elements,
-- satisfying a predicate, and the second one the rest.
--
-- This function isn't productive in the first component of the tuple
-- (e. g., 'head' . 'Data.Tuple.fst' . 'partition' @f@ won't terminate),
-- if no elements of the input list satisfy the predicate.
-- Same for the second component,
-- if all elements of the input list satisfy the predicate.
partition :: (a -> Bool) -> Infinite a -> (Infinite a, Infinite a)
partition f = foldr (\a -> if f a then first (a :<) else second (a :<))

-- | Return /n/-th element of an infinite list.
-- On contrary to @Data.List.@'List.!!', this function takes 'Word' instead of 'Int'
-- to avoid 'Prelude.error' on negative arguments.
--
-- This is actually @index@ from
-- [@Representable@](https://hackage.haskell.org/package/adjunctions/docs/Data-Functor-Rep.html#t:Representable)
-- type class in disguise.
(!!) :: Infinite a -> Word -> a
(!!) = foldr (\x acc m -> if m == 0 then x else acc (m - 1))

infixl 9 !!

-- | Return an index of the first element, equal to a given.
-- If there is nothing to be found, this function will hang indefinitely.
elemIndex :: Eq a => a -> Infinite a -> Word
elemIndex = findIndex . (==)

-- | Return indices of all elements, equal to a given.
--
-- This function isn't productive (e. g., 'head' . 'elemIndices' @f@ won't terminate),
-- if no elements of the input list are equal the given one.
elemIndices :: Eq a => a -> Infinite a -> Infinite Word
elemIndices = findIndices . (==)

-- | Return an index of the first element, satisfying a predicate.
-- If there is nothing to be found, this function will hang indefinitely.
findIndex :: (a -> Bool) -> Infinite a -> Word
findIndex f = flip (foldr (\x acc !m -> if f x then m else acc (m + 1))) 0

-- | Return indices of all elements, satisfying a predicate.
--
-- This function isn't productive (e. g., 'head' . 'elemIndices' @f@ won't terminate),
-- if no elements of the input list satisfy the predicate.
findIndices :: (a -> Bool) -> Infinite a -> Infinite Word
findIndices f = flip (foldr (\x acc !m -> (if f x then (m :<) else id) (acc (m + 1)))) 0

-- | Unzip an infinite list of tuples.
unzip :: Infinite (a, b) -> (Infinite a, Infinite b)
unzip = foldr (\(a, b) ~(as, bs) -> (a :< as, b :< bs))
{-# INLINE unzip #-}

-- | Unzip an infinite list of triples.
unzip3 :: Infinite (a, b, c) -> (Infinite a, Infinite b, Infinite c)
unzip3 = foldr (\(a, b, c) ~(as, bs, cs) -> (a :< as, b :< bs, c :< cs))
{-# INLINE unzip3 #-}

-- | Unzip an infinite list of quadruples.
unzip4 :: Infinite (a, b, c, d) -> (Infinite a, Infinite b, Infinite c, Infinite d)
unzip4 = foldr (\(a, b, c, d) ~(as, bs, cs, ds) -> (a :< as, b :< bs, c :< cs, d :< ds))
{-# INLINE unzip4 #-}

-- | Unzip an infinite list of quintuples.
unzip5 :: Infinite (a, b, c, d, e) -> (Infinite a, Infinite b, Infinite c, Infinite d, Infinite e)
unzip5 = foldr (\(a, b, c, d, e) ~(as, bs, cs, ds, es) -> (a :< as, b :< bs, c :< cs, d :< ds, e :< es))
{-# INLINE unzip5 #-}

-- | Unzip an infinite list of sextuples.
unzip6 :: Infinite (a, b, c, d, e, f) -> (Infinite a, Infinite b, Infinite c, Infinite d, Infinite e, Infinite f)
unzip6 = foldr (\(a, b, c, d, e, f) ~(as, bs, cs, ds, es, fs) -> (a :< as, b :< bs, c :< cs, d :< ds, e :< es, f :< fs))
{-# INLINE unzip6 #-}

-- | Unzip an infinite list of septuples.
unzip7 :: Infinite (a, b, c, d, e, f, g) -> (Infinite a, Infinite b, Infinite c, Infinite d, Infinite e, Infinite f, Infinite g)
unzip7 = foldr (\(a, b, c, d, e, f, g) ~(as, bs, cs, ds, es, fs, gs) -> (a :< as, b :< bs, c :< cs, d :< ds, e :< es, f :< fs, g :< gs))
{-# INLINE unzip7 #-}

-- | Split an infinite string into lines, by @\\n@. Empty lines are preserved.
--
-- In contrast to their counterparts from "Data.List", it holds that
-- 'unlines' @.@ 'lines' @=@ 'id'.
lines :: Infinite Char -> Infinite [Char]
lines = foldr go
  where
    go '\n' xs = [] :< xs
    go c ~(x :< xs) = (c : x) :< xs

-- | Concatenate lines together with @\\n@.
--
-- In contrast to their counterparts from "Data.List", it holds that
-- 'unlines' @.@ 'lines' @=@ 'id'.
unlines :: Infinite [Char] -> Infinite Char
unlines = foldr (\l xs -> l `prependList` ('\n' :< xs))

-- | Split an infinite string into words, by any 'isSpace' symbol.
-- Leading spaces are removed and, as underlined by the return type,
-- repeated spaces are treated as a single delimiter.
words :: Infinite Char -> Infinite (NonEmpty Char)
-- This is fundamentally a zygomorphism with 'isSpace' . 'head' as the small algebra.
-- But manual implementation via catamorphism requires twice less calls of 'isSpace'.
words = uncurry repack . foldr go
  where
    repack zs acc = maybe acc (:< acc) (NE.nonEmpty zs)

    go x ~(zs, acc) = (zs', acc')
      where
        s = isSpace x
        zs' = if s then [] else x : zs
        acc' = if s then repack zs acc else acc

wordsFB :: (NonEmpty Char -> lst -> lst) -> Infinite Char -> lst
wordsFB cons = uncurry repack . foldr go
  where
    repack zs acc = maybe acc (`cons` acc) (NE.nonEmpty zs)

    go x ~(zs, acc) = (zs', acc')
      where
        s = isSpace x
        zs' = if s then [] else x : zs
        acc' = if s then repack zs acc else acc

{-# NOINLINE [1] words #-}

{-# INLINE [0] wordsFB #-}

{-# RULES
"words" [~1] forall s. words s = build (`wordsFB` s)
"wordsList" [1] wordsFB (:<) = words
  #-}

-- | Concatenate words together with a space.
--
-- The function is meant to be a counterpart of with 'words'.
-- If you need to concatenate together 'Infinite' @[@'Char'@]@,
-- use 'intercalate' @(@'pure' @' ')@.
unwords :: Infinite (NonEmpty Char) -> Infinite Char
unwords = foldr (\(l :| ls) acc -> l :< ls `prependList` (' ' :< acc))

unwordsFB :: (Char -> lst -> lst) -> Infinite (NonEmpty Char) -> lst
unwordsFB cons = foldr (\(l :| ls) acc -> l `cons` List.foldr cons (' ' `cons` acc) ls)

{-# NOINLINE [1] unwords #-}

{-# INLINE [0] unwordsFB #-}

{-# RULES
"unwords" [~1] forall s. unwords s = build (`unwordsFB` s)
"unwordsList" [1] unwordsFB (:<) = unwords
  #-}

-- | Remove duplicate from a list, keeping only the first occurrence of each element.
nub :: Eq a => Infinite a -> Infinite a
nub = nubBy (==)

-- | Overloaded version of 'nub'.
nubBy :: (a -> a -> Bool) -> Infinite a -> Infinite a
nubBy eq = flip (foldr (\x acc seen -> if List.any (`eq` x) seen then acc seen else x :< acc (x : seen))) []

-- | Remove all occurrences of an element from an infinite list.
delete :: Eq a => a -> Infinite a -> Infinite a
delete = deleteBy (==)

-- | Overloaded version of 'delete'.
deleteBy :: (a -> b -> Bool) -> a -> Infinite b -> Infinite b
deleteBy eq x = para (\y ys acc -> if eq x y then ys else y :< acc)

-- | Take an infinite list and remove the first occurrence of every element
-- of a finite list.
(\\) :: Eq a => Infinite a -> [a] -> Infinite a
(\\) = deleteFirstsBy (==)

-- | Overloaded version of '(\\)'.
deleteFirstsBy :: (a -> b -> Bool) -> Infinite b -> [a] -> Infinite b
deleteFirstsBy eq = List.foldl (flip (deleteBy eq))

-- | Union of a finite and an infinite list. It contains the finite list
-- as a prefix and afterwards all non-duplicate elements of the infinite list,
-- which are not members of the finite list.
union :: Eq a => [a] -> Infinite a -> Infinite a
union = unionBy (==)

-- | Overloaded version of 'union'.
unionBy :: (a -> a -> Bool) -> [a] -> Infinite a -> Infinite a
unionBy eq xs ys = xs `prependList` List.foldl (flip (deleteBy eq)) (nubBy eq ys) xs

-- | Insert an element at the first position where it is less than or equal
-- to the next one. If the input was sorted, the output remains sorted as well.
insert :: Ord a => a -> Infinite a -> Infinite a
insert = insertBy compare

-- | Overloaded version of 'insert'.
insertBy :: (a -> a -> Ordering) -> a -> Infinite a -> Infinite a
insertBy cmp x = para (\y ys acc -> case cmp x y of GT -> y :< acc; _ -> x :< y :< ys)

-- | Return all elements of an infinite list, which are simultaneously
-- members of a finite list.
intersect :: Eq a => Infinite a -> [a] -> Infinite a
intersect = intersectBy (==)

-- | Overloaded version of 'intersect'.
intersectBy :: (a -> b -> Bool) -> Infinite a -> [b] -> Infinite a
intersectBy eq xs ys = filter (\x -> List.any (eq x) ys) xs

-- | Prepend a list to an infinite list.
prependList :: [a] -> Infinite a -> Infinite a
prependList = flip (F.foldr (:<))

-- | Apply a function to every element of an infinite list and collect 'Just' results.
--
-- This function isn't productive (e. g., 'head' . 'mapMaybe' @f@ won't terminate),
-- if no elements of the input list result in 'Just'.
--
-- @since 0.1.1
mapMaybe :: (a -> Maybe b) -> Infinite a -> Infinite b
mapMaybe = foldr . (maybe id (:<) .)

-- | Keep only 'Just' elements.
--
-- This function isn't productive (e. g., 'head' . 'catMaybes' won't terminate),
-- if no elements of the input list are 'Just'.
--
-- @since 0.1.1
catMaybes :: Infinite (Maybe a) -> Infinite a
catMaybes = foldr (maybe id (:<))

-- | Apply a function to every element of an infinite list and
-- separate 'Data.Either.Left' and 'Data.Either.Right' results.
--
-- This function isn't productive (e. g., 'head' . 'Data.Tuple.fst' .
-- 'mapEither' @f@ won't terminate),
-- if no elements of the input list result in 'Data.Either.Left' or 'Data.Either.Right'.
--
-- @since 0.1.1
mapEither :: (a -> Either b c) -> Infinite a -> (Infinite b, Infinite c)
mapEither = foldr . (either (first . (:<)) (second . (:<)) .)

-- | Separate 'Data.Either.Left' and 'Data.Either.Right' elements.
--
-- This function isn't productive (e. g., 'head' . 'Data.Tuple.fst' . 'partitionEithers'
-- won't terminate),
-- if no elements of the input list are 'Data.Either.Left' or 'Data.Either.Right'.
--
-- @since 0.1.1
partitionEithers :: Infinite (Either a b) -> (Infinite a, Infinite b)
partitionEithers = foldr (either (first . (:<)) (second . (:<)))
