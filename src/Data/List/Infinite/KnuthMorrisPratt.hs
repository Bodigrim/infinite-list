{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

-- | See https://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm
module Data.List.Infinite.KnuthMorrisPratt (
  splitOn,
) where

import Data.Array.Byte (ByteArray (..), MutableByteArray (..))
import Data.Bits (countTrailingZeros, finiteBitSize)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import GHC.Exts (Array#, Int (..), MutableArray#, iShiftL#, indexArray#, indexIntArray#, newArray#, newByteArray#, readIntArray#, sizeofArray#, unsafeFreezeArray#, unsafeFreezeByteArray#, writeArray#, writeIntArray#)
import GHC.ST (ST (..), runST)

import Data.List.Infinite.Internal (Infinite (..))

data Array a = Array (Array# a)

data MutableArray s a = MutableArray (MutableArray# s a)

newArray :: Int -> ST s (MutableArray s a)
newArray (I# n#) =
  ST
    ( \s# -> case newArray# n# undefined s# of
        (# s'#, arr# #) -> (# s'#, MutableArray arr# #)
    )

writeArray :: MutableArray s a -> Int -> a -> ST s ()
writeArray (MutableArray arr#) (I# i#) x =
  ST
    ( \s# -> case writeArray# arr# i# x s# of
        s'# -> (# s'#, () #)
    )

indexArray :: Array a -> Int -> a
indexArray (Array arr#) (I# i#) =
  let (# x #) = indexArray# arr# i# in x

sizeofArray :: Array a -> Int
sizeofArray (Array arr#) = I# (sizeofArray# arr#)

unsafeFreezeArray :: MutableArray s a -> ST s (Array a)
unsafeFreezeArray (MutableArray arr#) =
  ST
    ( \s# -> case unsafeFreezeArray# arr# s# of
        (# s'#, arr'# #) -> (# s'#, Array arr'# #)
    )

newIntArray :: Int -> ST s (MutableByteArray s)
newIntArray (I# n#) =
  ST
    ( \s# -> case newByteArray# (n# `iShiftL#` shift#) s# of
        (# s'#, arr# #) -> (# s'#, MutableByteArray arr# #)
    )
  where
    !(I# shift#) = countTrailingZeros (finiteBitSize (0 :: Int)) - 3

writeIntArray :: MutableByteArray s -> Int -> Int -> ST s ()
writeIntArray (MutableByteArray arr#) (I# i#) (I# x#) =
  ST
    ( \s# -> case writeIntArray# arr# i# x# s# of
        s'# -> (# s'#, () #)
    )

readIntArray :: MutableByteArray s -> Int -> ST s Int
readIntArray (MutableByteArray arr#) (I# i#) =
  ST
    ( \s# -> case readIntArray# arr# i# s# of
        (# s'#, x# #) -> (# s'#, I# x# #)
    )

indexIntArray :: ByteArray -> Int -> Int
indexIntArray (ByteArray arr#) (I# i#) =
  I# (indexIntArray# arr# i#)

unsafeFreezeIntArray :: MutableByteArray s -> ST s ByteArray
unsafeFreezeIntArray (MutableByteArray arr#) =
  ST
    ( \s# -> case unsafeFreezeByteArray# arr# s# of
        (# s'#, arr'# #) -> (# s'#, ByteArray arr'# #)
    )

buildArray :: [a] -> Array a
buildArray ws = runST $ do
  let n = length ws
  marr <- newArray n
  let go !_ [] = pure ()
      go !ix (x : xs) = do
        writeArray marr ix x
        go (ix + 1) xs
  go 0 ws
  unsafeFreezeArray marr

buildJumps :: Eq a => Array a -> ByteArray
buildJumps ws = runST $ do
  let n = sizeofArray ws
  marr <- newIntArray n
  writeIntArray marr 0 (-1)
  let go pos cnd
        | pos >= n = pure ()
        | otherwise = do
            let wPos = indexArray ws pos
            cnd'' <-
              if wPos == indexArray ws cnd
                then do
                  tCnd <- readIntArray marr cnd
                  writeIntArray marr pos tCnd
                  pure cnd
                else do
                  writeIntArray marr pos cnd
                  let gogo cnd'
                        | cnd' < 0 || wPos == indexArray ws cnd' = pure cnd'
                        | otherwise = do
                            tCnd' <- readIntArray marr cnd'
                            gogo tCnd'
                  gogo cnd
            go (pos + 1) (cnd'' + 1)
  go 1 0
  unsafeFreezeIntArray marr

-- | Split on the given sublist.
-- Right inverse of 'Data.List.Infinite.intercalate'.
splitOn :: forall a. Eq a => NonEmpty a -> Infinite a -> Infinite [a]
splitOn needle' haystack = go 0 haystack
  where
    needle = buildArray (NE.toList needle')
    jumps = buildJumps needle
    needleLen = sizeofArray needle

    go :: Int -> Infinite a -> Infinite [a]
    go !k hhay@(h :< hay)
      | h == indexArray needle k =
          if k == needleLen - 1
            then [] :< go 0 hay
            else go (k + 1) hay
      | otherwise =
          let k' = indexIntArray jumps k
           in if k' < 0
                then (\(~(x :< xs)) -> (map (indexArray needle) [0 .. k - 1] ++ h : x) :< xs) (go 0 hay)
                else (\(~(x :< xs)) -> (map (indexArray needle) [0 .. k - k' - 1] ++ x) :< xs) (go k' hhay)
