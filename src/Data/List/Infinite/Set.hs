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

data Color = Red | Black
  deriving (Show)

-- | Okasaki red-black tree.
data Set a = Empty | Node !Color !(Set a) !a !(Set a)
  deriving (Show)

empty :: Set a
empty = Empty

member :: (a -> a -> Ordering) -> a -> Set a -> Bool
member cmp = member'
  where
    member' x = go
      where
        go = \case
          Empty -> False
          Node _ left center right -> case cmp x center of
            LT -> go left
            EQ -> True
            GT -> go right

insert :: (a -> a -> Ordering) -> a -> Set a -> Set a
insert cmp = insert'
  where
    insert' x = blacken . go
      where
        go = \case
          Empty -> Node Red Empty x Empty
          Node color left center right -> case cmp x center of
            LT -> balance color (go left) center right
            EQ -> Node color left center right
            GT -> balance color left center (go right)

    blacken = \case
      Empty -> Empty
      Node _ left center right -> Node Black left center right

balance :: Color -> Set a -> a -> Set a -> Set a
balance Black (Node Red (Node Red a b c) d e) f g =
  Node Red (Node Black a b c) d (Node Black e f g)
balance Black (Node Red a b (Node Red c d e)) f g =
  Node Red (Node Black a b c) d (Node Black e f g)
balance Black a b (Node Red (Node Red c d e) f g) =
  Node Red (Node Black a b c) d (Node Black e f g)
balance Black a b (Node Red c d (Node Red e f g)) =
  Node Red (Node Black a b c) d (Node Black e f g)
balance color left center right =
  Node color left center right
