{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright:   (c) 2024 Bodigrim
-- License:     BSD3
module Data.List.Infinite.Set (
  Set,
  empty,
  member,
  insert,
) where

-- | Okasaki red-black tree.
data Set a
  = Empty
  | Red !(Set a) !a !(Set a)
  | Black !(Set a) !a !(Set a)

empty :: Set a
empty = Empty

member :: (a -> a -> Ordering) -> a -> Set a -> Bool
member cmp = member'
  where
    member' !x = go
      where
        go = \case
          Empty -> False
          Red left center right -> whereToGo left center right
          Black left center right -> whereToGo left center right

        whereToGo left center right = case x `cmp` center of
          LT -> go left
          EQ -> True
          GT -> go right
{-# INLINE member #-}

insert :: (a -> a -> Ordering) -> a -> Set a -> Set a
insert cmp = insert'
  where
    insert' !x = blacken . go
      where
        go node = case node of
          Empty -> Red Empty x Empty
          Red left center right -> case x `cmp` center of
            LT -> Red (go left) center right
            EQ -> node
            GT -> Red left center (go right)
          Black left center right -> case x `cmp` center of
            LT -> balanceLeft (go left) center right
            EQ -> node
            GT -> balanceRight left center (go right)

    blacken node = case node of
      Empty -> Empty
      Red left center right -> Black left center right
      Black {} -> node
{-# INLINE insert #-}

balanceLeft :: Set a -> a -> Set a -> Set a
balanceLeft (Red (Red a b c) d e) f g =
  Red (Black a b c) d (Black e f g)
balanceLeft (Red a b (Red c d e)) f g =
  Red (Black a b c) d (Black e f g)
balanceLeft left center right =
  Black left center right
{-# INLINE balanceLeft #-}

balanceRight :: Set a -> a -> Set a -> Set a
balanceRight a b (Red (Red c d e) f g) =
  Red (Black a b c) d (Black e f g)
balanceRight a b (Red c d (Red e f g)) =
  Red (Black a b c) d (Black e f g)
balanceRight left center right =
  Black left center right
{-# INLINE balanceRight #-}
