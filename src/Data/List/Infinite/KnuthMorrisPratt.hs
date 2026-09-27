{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.List.Infinite.KnuthMorrisPratt (
  splitOn,
) where

import Data.Array.Byte (ByteArray (..), MutableByteArray (..))
import Data.Bits (countTrailingZeros, finiteBitSize)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import GHC.Exts (Array#, Int (..), MutableArray#, iShiftL#, iShiftRL#, indexArray#, indexIntArray#, newArray#, newByteArray#, readIntArray#, sizeofArray#, sizeofByteArray#, unsafeFreezeArray#, unsafeFreezeByteArray#, writeArray#, writeIntArray#)
import GHC.ST (ST (..), runST)

import Data.List.Infinite.Internal (Infinite (..))

data Array a = Array (Array# a)

instance Show a => Show (Array a) where
  show arr = show $ map (indexArray arr) [0 .. sizeofArray arr - 1]

data MutableArray s a = MutableArray (MutableArray# s a)

newArray :: Int -> ST s (MutableArray s a)
{-# INLINE newArray #-}
newArray (I# n#) =
  ST
    ( \s# -> case newArray# n# undefined s# of
        (# s'#, arr# #) -> (# s'#, MutableArray arr# #)
    )

writeArray :: MutableArray s a -> Int -> a -> ST s ()
{-# INLINE writeArray #-}
writeArray (MutableArray arr#) (I# i#) x =
  ST
    ( \s# -> case writeArray# arr# i# x s# of
        s'# -> (# s'#, () #)
    )

indexArray :: Array a -> Int -> a
{-# INLINE indexArray #-}
indexArray (Array arr#) (I# i#) =
  let (# x #) = indexArray# arr# i# in x

sizeofArray :: Array a -> Int
{-# INLINE sizeofArray #-}
sizeofArray (Array arr#) = I# (sizeofArray# arr#)

unsafeFreezeArray :: MutableArray s a -> ST s (Array a)
{-# INLINE unsafeFreezeArray #-}
unsafeFreezeArray (MutableArray arr#) =
  ST
    ( \s# -> case unsafeFreezeArray# arr# s# of
        (# s'#, arr'# #) -> (# s'#, Array arr'# #)
    )

newIntArray :: Int -> ST s (MutableByteArray s)
{-# INLINE newIntArray #-}
newIntArray (I# n#) =
  ST
    ( \s# -> case newByteArray# (n# `iShiftL#` shift#) s# of
        (# s'#, arr# #) -> (# s'#, MutableByteArray arr# #)
    )
  where
    !(I# shift#) = countTrailingZeros (finiteBitSize (0 :: Int)) - 3

sizeofIntArray :: ByteArray -> Int
{-# INLINE sizeofIntArray #-}
sizeofIntArray (ByteArray arr#) = I# (sizeofByteArray# arr# `iShiftRL#` shift#)
  where
    !(I# shift#) = countTrailingZeros (finiteBitSize (0 :: Int)) - 3

writeIntArray :: MutableByteArray s -> Int -> Int -> ST s ()
{-# INLINE writeIntArray #-}
writeIntArray (MutableByteArray arr#) (I# i#) (I# x#) =
  ST
    ( \s# -> case writeIntArray# arr# i# x# s# of
        s'# -> (# s'#, () #)
    )

readIntArray :: MutableByteArray s -> Int -> ST s Int
{-# INLINE readIntArray #-}
readIntArray (MutableByteArray arr#) (I# i#) =
  ST
    ( \s# -> case readIntArray# arr# i# s# of
        (# s'#, x# #) -> (# s'#, I# x# #)
    )

indexIntArray :: ByteArray -> Int -> Int
{-# INLINE indexIntArray #-}
indexIntArray (ByteArray arr#) (I# i#) =
  I# (indexIntArray# arr# i#)

unsafeFreezeIntArray :: MutableByteArray s -> ST s ByteArray
{-# INLINE unsafeFreezeIntArray #-}
unsafeFreezeIntArray (MutableByteArray arr#) =
  ST
    ( \s# -> case unsafeFreezeByteArray# arr# s# of
        (# s'#, arr'# #) -> (# s'#, ByteArray arr'# #)
    )

data JumpTable a = JumpTable
  { jtNeedle :: !(Array a)
  , jtJumps :: !ByteArray
  }

instance Show a => Show (JumpTable a) where
  show (JumpTable n j) =
    "JumpTable "
      ++ show n
      ++ " "
      ++ show (map (indexIntArray j) [0 .. sizeofIntArray j - 1])

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

-- | https://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm#Description_of_pseudocode_for_the_table-building_algorithm
buildJumps :: Eq a => Array a -> ByteArray
buildJumps ws = runST $ do
  let n = sizeofArray ws
  marr <- newIntArray n
  writeIntArray marr 0 (-1)
  let go pos cnd
        | pos >= n = pure ()
        | otherwise = do
            let wPos = indexArray ws pos
                wCnd = indexArray ws cnd
            cnd'' <-
              if wPos == wCnd
                then do
                  tCnd <- readIntArray marr cnd
                  writeIntArray marr pos tCnd
                  pure cnd
                else do
                  writeIntArray marr pos cnd
                  let gogo cnd' =
                        if cnd' < 0
                          then pure cnd'
                          else do
                            let wCnd' = indexArray ws cnd'
                            if wPos == wCnd'
                              then pure cnd'
                              else do
                                tCnd' <- readIntArray marr cnd'
                                gogo tCnd'
                  gogo cnd
            go (pos + 1) (cnd'' + 1)
  go 1 0
  unsafeFreezeIntArray marr

buildJumpTable :: Eq a => [a] -> JumpTable a
buildJumpTable xs =
  JumpTable
    { jtNeedle = as
    , jtJumps = buildJumps as
    }
  where
    as = buildArray xs

-- | Split on the given sublist.
-- Right inverse of 'Data.List.Infinite.intercalate'.
splitOn :: forall a. Eq a => NonEmpty a -> Infinite a -> Infinite [a]
splitOn needle haystack = go 0 haystack
  where
    JumpTable {..} = buildJumpTable (NE.toList needle)
    needleLen = sizeofArray jtNeedle

    go :: Int -> Infinite a -> Infinite [a]
    go !k hhay@(h :< hay)
      | h == indexArray jtNeedle k =
          if k == needleLen - 1
            then [] :< go 0 hay
            else go (k + 1) hay
      | otherwise =
          let k' = indexIntArray jtJumps k
           in if k' < 0
                then (\(~(x :< xs)) -> (map (indexArray jtNeedle) [0 .. k - 1] ++ h : x) :< xs) (go 0 hay)
                else (\(~(x :< xs)) -> (map (indexArray jtNeedle) [0 .. k - k' - 1] ++ x) :< xs) (go k' hhay)
