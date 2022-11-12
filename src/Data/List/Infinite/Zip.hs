-- |
-- Copyright:   (c) 2022 Bodigrim
-- Licence:     BSD3
module Data.List.Infinite.Zip (
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
) where

import Prelude (flip, (.))

import Data.List.Infinite.Internal

-- | Zip two infinite lists.
zip :: Infinite a -> Infinite b -> Infinite (a, b)
zip = zipWith (,)
{-# INLINE zip #-}

-- | Zip two infinite lists with a given function.
zipWith :: (a -> b -> c) -> Infinite a -> Infinite b -> Infinite c
zipWith fun = go
  where
    go (a :< as) (b :< bs) = fun a b :< go as bs

zipWithFB :: (elt -> lst -> lst') -> (a -> b -> elt) -> a -> b -> lst -> lst'
zipWithFB = (.) . (.)

{-# NOINLINE [1] zipWith #-}

{-# INLINE [0] zipWithFB #-}

{-# RULES
"zipWith" [~1] forall f xs ys.
  zipWith f xs ys =
    build (\cons -> foldr2 (zipWithFB cons f) xs ys)
"zipWithList" [1] forall f.
  foldr2 (zipWithFB (:<) f) =
    zipWith f
  #-}

foldr2 :: (elt1 -> elt2 -> lst -> lst) -> Infinite elt1 -> Infinite elt2 -> lst
foldr2 cons = go
  where
    go (a :< as) (b :< bs) = cons a b (go as bs)
{-# INLINE [0] foldr2 #-}

foldr2_left :: (elt1 -> elt2 -> lst -> lst') -> elt1 -> (Infinite elt2 -> lst) -> Infinite elt2 -> lst'
foldr2_left cons a r (b :< bs) = cons a b (r bs)

{-# RULES
"foldr2/1" forall (cons :: elt1 -> elt2 -> lst -> lst) (bs :: Infinite elt2) (g :: forall b. (elt1 -> b -> b) -> b).
  foldr2 cons (build g) bs =
    g (foldr2_left cons) bs
"foldr2/2" forall (cons :: elt1 -> elt2 -> lst -> lst) (as :: Infinite elt1) (g :: forall b. (elt2 -> b -> b) -> b).
  foldr2 cons as (build g) =
    g (foldr2_left (flip cons)) as
  #-}

-- | Zip three infinite lists.
zip3 :: Infinite a -> Infinite b -> Infinite c -> Infinite (a, b, c)
zip3 = zipWith3 (,,)
{-# INLINE zip3 #-}

-- | Zip three infinite lists with a given function.
zipWith3 :: (a -> b -> c -> d) -> Infinite a -> Infinite b -> Infinite c -> Infinite d
zipWith3 fun = go
  where
    go (a :< as) (b :< bs) (c :< cs) = fun a b c :< go as bs cs

zipWith3FB :: (elt -> lst -> lst') -> (a -> b -> c -> elt) -> a -> b -> c -> lst -> lst'
zipWith3FB = (.) . (.) . (.)

{-# NOINLINE [1] zipWith3 #-}

{-# INLINE [0] zipWith3FB #-}

{-# RULES
"zipWith3" [~1] forall f xs ys zs.
  zipWith3 f xs ys zs =
    build (\cons -> foldr3 (zipWith3FB cons f) xs ys zs)
"zipWith3List" [1] forall f.
  foldr3 (zipWith3FB (:<) f) =
    zipWith3 f
  #-}

foldr3 :: (elt1 -> elt2 -> elt3 -> lst -> lst) -> Infinite elt1 -> Infinite elt2 -> Infinite elt3 -> lst
foldr3 cons = go
  where
    go (a :< as) (b :< bs) (c :< cs) = cons a b c (go as bs cs)
{-# INLINE [0] foldr3 #-}

foldr3_left :: (elt1 -> elt2 -> elt3 -> lst -> lst') -> elt1 -> (Infinite elt2 -> Infinite elt3 -> lst) -> Infinite elt2 -> Infinite elt3 -> lst'
foldr3_left cons a r (b :< bs) (c :< cs) = cons a b c (r bs cs)

{-# RULES
"foldr3/1" forall (cons :: elt1 -> elt2 -> elt3 -> lst -> lst) (bs :: Infinite elt2) (cs :: Infinite elt3) (g :: forall b. (elt1 -> b -> b) -> b).
  foldr3 cons (build g) bs cs =
    g (foldr3_left cons) bs cs
"foldr3/2" forall (cons :: elt1 -> elt2 -> elt3 -> lst -> lst) (as :: Infinite elt1) (cs :: Infinite elt3) (g :: forall b. (elt2 -> b -> b) -> b).
  foldr3 cons as (build g) cs =
    g (foldr3_left (flip cons)) as cs
"foldr3/3" forall (cons :: elt1 -> elt2 -> elt3 -> lst -> lst) (as :: Infinite elt1) (bs :: Infinite elt2) (g :: forall b. (elt3 -> b -> b) -> b).
  foldr3 cons as bs (build g) =
    g (foldr3_left (\c a b -> cons a b c)) as bs
  #-}

-- | Zip four infinite lists.
zip4 :: Infinite a -> Infinite b -> Infinite c -> Infinite d -> Infinite (a, b, c, d)
zip4 = zipWith4 (,,,)
{-# INLINE zip4 #-}

-- | Zip four infinite lists with a given function.
zipWith4 :: (a -> b -> c -> d -> e) -> Infinite a -> Infinite b -> Infinite c -> Infinite d -> Infinite e
zipWith4 fun = go
  where
    go (a :< as) (b :< bs) (c :< cs) (d :< ds) = fun a b c d :< go as bs cs ds

zipWith4FB :: (elt -> lst -> lst') -> (a -> b -> c -> d -> elt) -> a -> b -> c -> d -> lst -> lst'
zipWith4FB = (.) . (.) . (.) . (.)

{-# NOINLINE [1] zipWith4 #-}

{-# INLINE [0] zipWith4FB #-}

{-# RULES
"zipWith4" [~1] forall f xs ys zs ts.
  zipWith4 f xs ys zs ts =
    build (\cons -> foldr4 (zipWith4FB cons f) xs ys zs ts)
"zipWith4List" [1] forall f.
  foldr4 (zipWith4FB (:<) f) =
    zipWith4 f
  #-}

foldr4 :: (elt1 -> elt2 -> elt3 -> elt4 -> lst -> lst) -> Infinite elt1 -> Infinite elt2 -> Infinite elt3 -> Infinite elt4 -> lst
foldr4 cons = go
  where
    go (a :< as) (b :< bs) (c :< cs) (d :< ds) = cons a b c d (go as bs cs ds)
{-# INLINE [0] foldr4 #-}

foldr4_left :: (elt1 -> elt2 -> elt3 -> elt4 -> lst -> lst') -> elt1 -> (Infinite elt2 -> Infinite elt3 -> Infinite elt4 -> lst) -> Infinite elt2 -> Infinite elt3 -> Infinite elt4 -> lst'
foldr4_left cons a r (b :< bs) (c :< cs) (d :< ds) = cons a b c d (r bs cs ds)

{-# RULES
"foldr4/1" forall (cons :: elt1 -> elt2 -> elt3 -> elt4 -> lst -> lst) (bs :: Infinite elt2) (cs :: Infinite elt3) (ds :: Infinite elt4) (g :: forall b. (elt1 -> b -> b) -> b).
  foldr4 cons (build g) bs cs ds =
    g (foldr4_left cons) bs cs ds
"foldr4/2" forall (cons :: elt1 -> elt2 -> elt3 -> elt4 -> lst -> lst) (as :: Infinite elt1) (cs :: Infinite elt3) (ds :: Infinite elt4) (g :: forall b. (elt2 -> b -> b) -> b).
  foldr4 cons as (build g) cs ds =
    g (foldr4_left (flip cons)) as cs ds
"foldr4/3" forall (cons :: elt1 -> elt2 -> elt3 -> elt4 -> lst -> lst) (as :: Infinite elt1) (bs :: Infinite elt2) (ds :: Infinite elt4) (g :: forall b. (elt3 -> b -> b) -> b).
  foldr4 cons as bs (build g) ds =
    g (foldr4_left (\c a b d -> cons a b c d)) as bs ds
"foldr4/4" forall (cons :: elt1 -> elt2 -> elt3 -> elt4 -> lst -> lst) (as :: Infinite elt1) (bs :: Infinite elt2) (cs :: Infinite elt3) (g :: forall b. (elt4 -> b -> b) -> b).
  foldr4 cons as bs cs (build g) =
    g (foldr4_left (\d a b c -> cons a b c d)) as bs cs
  #-}

-- | Zip five infinite lists.
zip5 :: Infinite a -> Infinite b -> Infinite c -> Infinite d -> Infinite e -> Infinite (a, b, c, d, e)
zip5 = zipWith5 (,,,,)
{-# INLINE zip5 #-}

-- | Zip five infinite lists with a given function.
zipWith5 :: (a -> b -> c -> d -> e -> f) -> Infinite a -> Infinite b -> Infinite c -> Infinite d -> Infinite e -> Infinite f
zipWith5 fun = go
  where
    go (a :< as) (b :< bs) (c :< cs) (d :< ds) (e :< es) = fun a b c d e :< go as bs cs ds es

zipWith5FB :: (elt -> lst -> lst') -> (a -> b -> c -> d -> e -> elt) -> a -> b -> c -> d -> e -> lst -> lst'
zipWith5FB = (.) . (.) . (.) . (.) . (.)

{-# NOINLINE [1] zipWith5 #-}

{-# INLINE [0] zipWith5FB #-}

{-# RULES
"zipWith5" [~1] forall f xs ys zs ts us.
  zipWith5 f xs ys zs ts us =
    build (\cons -> foldr5 (zipWith5FB cons f) xs ys zs ts us)
"zipWith5List" [1] forall f.
  foldr5 (zipWith5FB (:<) f) =
    zipWith5 f
  #-}

foldr5 :: (elt1 -> elt2 -> elt3 -> elt4 -> elt5 -> lst -> lst) -> Infinite elt1 -> Infinite elt2 -> Infinite elt3 -> Infinite elt4 -> Infinite elt5 -> lst
foldr5 cons = go
  where
    go (a :< as) (b :< bs) (c :< cs) (d :< ds) (e :< es) = cons a b c d e (go as bs cs ds es)
{-# INLINE [0] foldr5 #-}

foldr5_left :: (elt1 -> elt2 -> elt3 -> elt4 -> elt5 -> lst -> lst') -> elt1 -> (Infinite elt2 -> Infinite elt3 -> Infinite elt4 -> Infinite elt5 -> lst) -> Infinite elt2 -> Infinite elt3 -> Infinite elt4 -> Infinite elt5 -> lst'
foldr5_left cons a r (b :< bs) (c :< cs) (d :< ds) (e :< es) = cons a b c d e (r bs cs ds es)

{-# RULES
"foldr5/1" forall (cons :: elt1 -> elt2 -> elt3 -> elt4 -> elt5 -> lst -> lst) (bs :: Infinite elt2) (cs :: Infinite elt3) (ds :: Infinite elt4) (es :: Infinite elt5) (g :: forall b. (elt1 -> b -> b) -> b).
  foldr5 cons (build g) bs cs ds es =
    g (foldr5_left cons) bs cs ds es
"foldr5/2" forall (cons :: elt1 -> elt2 -> elt3 -> elt4 -> elt5 -> lst -> lst) (as :: Infinite elt1) (cs :: Infinite elt3) (ds :: Infinite elt4) (es :: Infinite elt5) (g :: forall b. (elt2 -> b -> b) -> b).
  foldr5 cons as (build g) cs ds es =
    g (foldr5_left (flip cons)) as cs ds es
"foldr5/3" forall (cons :: elt1 -> elt2 -> elt3 -> elt4 -> elt5 -> lst -> lst) (as :: Infinite elt1) (bs :: Infinite elt2) (ds :: Infinite elt4) (es :: Infinite elt5) (g :: forall b. (elt3 -> b -> b) -> b).
  foldr5 cons as bs (build g) ds es =
    g (foldr5_left (\c a b d e -> cons a b c d e)) as bs ds es
"foldr5/4" forall (cons :: elt1 -> elt2 -> elt3 -> elt4 -> elt5 -> lst -> lst) (as :: Infinite elt1) (bs :: Infinite elt2) (cs :: Infinite elt3) (es :: Infinite elt5) (g :: forall b. (elt4 -> b -> b) -> b).
  foldr5 cons as bs cs (build g) es =
    g (foldr5_left (\d a b c e -> cons a b c d e)) as bs cs es
"foldr5/5" forall (cons :: elt1 -> elt2 -> elt3 -> elt4 -> elt5 -> lst -> lst) (as :: Infinite elt1) (bs :: Infinite elt2) (cs :: Infinite elt3) (ds :: Infinite elt4) (g :: forall b. (elt5 -> b -> b) -> b).
  foldr5 cons as bs cs ds (build g) =
    g (foldr5_left (\e a b c d -> cons a b c d e)) as bs cs ds
  #-}

-- | Zip six infinite lists.
zip6 :: Infinite a -> Infinite b -> Infinite c -> Infinite d -> Infinite e -> Infinite f -> Infinite (a, b, c, d, e, f)
zip6 = zipWith6 (,,,,,)
{-# INLINE zip6 #-}

-- | Zip six infinite lists with a given function.
zipWith6 :: (a -> b -> c -> d -> e -> f -> g) -> Infinite a -> Infinite b -> Infinite c -> Infinite d -> Infinite e -> Infinite f -> Infinite g
zipWith6 fun = go
  where
    go (a :< as) (b :< bs) (c :< cs) (d :< ds) (e :< es) (f :< fs) = fun a b c d e f :< go as bs cs ds es fs

zipWith6FB :: (elt -> lst -> lst') -> (a -> b -> c -> d -> e -> f -> elt) -> a -> b -> c -> d -> e -> f -> lst -> lst'
zipWith6FB = (.) . (.) . (.) . (.) . (.) . (.)

{-# NOINLINE [1] zipWith6 #-}

{-# INLINE [0] zipWith6FB #-}

{-# RULES
"zipWith6" [~1] forall f xs ys zs ts us vs.
  zipWith6 f xs ys zs ts us vs =
    build (\cons -> foldr6 (zipWith6FB cons f) xs ys zs ts us vs)
"zipWith6List" [1] forall f.
  foldr6 (zipWith6FB (:<) f) =
    zipWith6 f
  #-}

foldr6 :: (elt1 -> elt2 -> elt3 -> elt4 -> elt5 -> elt6 -> lst -> lst) -> Infinite elt1 -> Infinite elt2 -> Infinite elt3 -> Infinite elt4 -> Infinite elt5 -> Infinite elt6 -> lst
foldr6 cons = go
  where
    go (a :< as) (b :< bs) (c :< cs) (d :< ds) (e :< es) (f :< fs) = cons a b c d e f (go as bs cs ds es fs)
{-# INLINE [0] foldr6 #-}

foldr6_left :: (elt1 -> elt2 -> elt3 -> elt4 -> elt5 -> elt6 -> lst -> lst') -> elt1 -> (Infinite elt2 -> Infinite elt3 -> Infinite elt4 -> Infinite elt5 -> Infinite elt6 -> lst) -> Infinite elt2 -> Infinite elt3 -> Infinite elt4 -> Infinite elt5 -> Infinite elt6 -> lst'
foldr6_left cons a r (b :< bs) (c :< cs) (d :< ds) (e :< es) (f :< fs) = cons a b c d e f (r bs cs ds es fs)

{-# RULES
"foldr6/1" forall (cons :: elt1 -> elt2 -> elt3 -> elt4 -> elt5 -> elt6 -> lst -> lst) (bs :: Infinite elt2) (cs :: Infinite elt3) (ds :: Infinite elt4) (es :: Infinite elt5) (fs :: Infinite elt6) (g :: forall b. (elt1 -> b -> b) -> b).
  foldr6 cons (build g) bs cs ds es fs =
    g (foldr6_left cons) bs cs ds es fs
"foldr6/2" forall (cons :: elt1 -> elt2 -> elt3 -> elt4 -> elt5 -> elt6 -> lst -> lst) (as :: Infinite elt1) (cs :: Infinite elt3) (ds :: Infinite elt4) (es :: Infinite elt5) (fs :: Infinite elt6) (g :: forall b. (elt2 -> b -> b) -> b).
  foldr6 cons as (build g) cs ds es fs =
    g (foldr6_left (flip cons)) as cs ds es fs
"foldr6/3" forall (cons :: elt1 -> elt2 -> elt3 -> elt4 -> elt5 -> elt6 -> lst -> lst) (as :: Infinite elt1) (bs :: Infinite elt2) (ds :: Infinite elt4) (es :: Infinite elt5) (fs :: Infinite elt6) (g :: forall b. (elt3 -> b -> b) -> b).
  foldr6 cons as bs (build g) ds es fs =
    g (foldr6_left (\c a b d e f -> cons a b c d e f)) as bs ds es fs
"foldr6/4" forall (cons :: elt1 -> elt2 -> elt3 -> elt4 -> elt5 -> elt6 -> lst -> lst) (as :: Infinite elt1) (bs :: Infinite elt2) (cs :: Infinite elt3) (es :: Infinite elt5) (fs :: Infinite elt6) (g :: forall b. (elt4 -> b -> b) -> b).
  foldr6 cons as bs cs (build g) es fs =
    g (foldr6_left (\d a b c e f -> cons a b c d e f)) as bs cs es fs
"foldr6/5" forall (cons :: elt1 -> elt2 -> elt3 -> elt4 -> elt5 -> elt6 -> lst -> lst) (as :: Infinite elt1) (bs :: Infinite elt2) (cs :: Infinite elt3) (ds :: Infinite elt4) (fs :: Infinite elt6) (g :: forall b. (elt5 -> b -> b) -> b).
  foldr6 cons as bs cs ds (build g) fs =
    g (foldr6_left (\e a b c d f -> cons a b c d e f)) as bs cs ds fs
"foldr6/6" forall (cons :: elt1 -> elt2 -> elt3 -> elt4 -> elt5 -> elt6 -> lst -> lst) (as :: Infinite elt1) (bs :: Infinite elt2) (cs :: Infinite elt3) (ds :: Infinite elt4) (es :: Infinite elt5) (g :: forall b. (elt6 -> b -> b) -> b).
  foldr6 cons as bs cs ds es (build g) =
    g (foldr6_left (\f a b c d e -> cons a b c d e f)) as bs cs ds es
  #-}

-- | Zip seven infinite lists.
zip7 :: Infinite a -> Infinite b -> Infinite c -> Infinite d -> Infinite e -> Infinite f -> Infinite g -> Infinite (a, b, c, d, e, f, g)
zip7 = zipWith7 (,,,,,,)
{-# INLINE zip7 #-}

-- | Zip seven infinite lists with a given function.
zipWith7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> Infinite a -> Infinite b -> Infinite c -> Infinite d -> Infinite e -> Infinite f -> Infinite g -> Infinite h
zipWith7 fun = go
  where
    go (a :< as) (b :< bs) (c :< cs) (d :< ds) (e :< es) (f :< fs) (g :< gs) = fun a b c d e f g :< go as bs cs ds es fs gs

zipWith7FB :: (elt -> lst -> lst') -> (a -> b -> c -> d -> e -> f -> g -> elt) -> a -> b -> c -> d -> e -> f -> g -> lst -> lst'
zipWith7FB = (.) . (.) . (.) . (.) . (.) . (.) . (.)

{-# NOINLINE [1] zipWith7 #-}

{-# INLINE [0] zipWith7FB #-}

{-# RULES
"zipWith7" [~1] forall f xs ys zs ts us vs ws.
  zipWith7 f xs ys zs ts us vs ws =
    build (\cons -> foldr7 (zipWith7FB cons f) xs ys zs ts us vs ws)
"zipWith7List" [1] forall f.
  foldr7 (zipWith7FB (:<) f) =
    zipWith7 f
  #-}

foldr7 :: (elt1 -> elt2 -> elt3 -> elt4 -> elt5 -> elt6 -> elt7 -> lst -> lst) -> Infinite elt1 -> Infinite elt2 -> Infinite elt3 -> Infinite elt4 -> Infinite elt5 -> Infinite elt6 -> Infinite elt7 -> lst
foldr7 cons = go
  where
    go (a :< as) (b :< bs) (c :< cs) (d :< ds) (e :< es) (f :< fs) (g :< gs) = cons a b c d e f g (go as bs cs ds es fs gs)
{-# INLINE [0] foldr7 #-}

foldr7_left :: (elt1 -> elt2 -> elt3 -> elt4 -> elt5 -> elt6 -> elt7 -> lst -> lst') -> elt1 -> (Infinite elt2 -> Infinite elt3 -> Infinite elt4 -> Infinite elt5 -> Infinite elt6 -> Infinite elt7 -> lst) -> Infinite elt2 -> Infinite elt3 -> Infinite elt4 -> Infinite elt5 -> Infinite elt6 -> Infinite elt7 -> lst'
foldr7_left cons a r (b :< bs) (c :< cs) (d :< ds) (e :< es) (f :< fs) (g :< gs) = cons a b c d e f g (r bs cs ds es fs gs)

{-# RULES
"foldr7/1" forall (cons :: elt1 -> elt2 -> elt3 -> elt4 -> elt5 -> elt6 -> elt7 -> lst -> lst) (bs :: Infinite elt2) (cs :: Infinite elt3) (ds :: Infinite elt4) (es :: Infinite elt5) (fs :: Infinite elt6) (gs :: Infinite elt7) (g :: forall b. (elt1 -> b -> b) -> b).
  foldr7 cons (build g) bs cs ds es fs gs =
    g (foldr7_left cons) bs cs ds es fs gs
"foldr7/2" forall (cons :: elt1 -> elt2 -> elt3 -> elt4 -> elt5 -> elt6 -> elt7 -> lst -> lst) (as :: Infinite elt1) (cs :: Infinite elt3) (ds :: Infinite elt4) (es :: Infinite elt5) (fs :: Infinite elt6) (gs :: Infinite elt7) (g :: forall b. (elt2 -> b -> b) -> b).
  foldr7 cons as (build g) cs ds es fs gs =
    g (foldr7_left (flip cons)) as cs ds es fs gs
"foldr7/3" forall (cons :: elt1 -> elt2 -> elt3 -> elt4 -> elt5 -> elt6 -> elt7 -> lst -> lst) (as :: Infinite elt1) (bs :: Infinite elt2) (ds :: Infinite elt4) (es :: Infinite elt5) (fs :: Infinite elt6) (gs :: Infinite elt7) (g :: forall b. (elt3 -> b -> b) -> b).
  foldr7 cons as bs (build g) ds es fs gs =
    g (foldr7_left (\c a b d e f g' -> cons a b c d e f g')) as bs ds es fs gs
"foldr7/4" forall (cons :: elt1 -> elt2 -> elt3 -> elt4 -> elt5 -> elt6 -> elt7 -> lst -> lst) (as :: Infinite elt1) (bs :: Infinite elt2) (cs :: Infinite elt3) (es :: Infinite elt5) (fs :: Infinite elt6) (gs :: Infinite elt7) (g :: forall b. (elt4 -> b -> b) -> b).
  foldr7 cons as bs cs (build g) es fs gs =
    g (foldr7_left (\d a b c e f g' -> cons a b c d e f g')) as bs cs es fs gs
"foldr7/5" forall (cons :: elt1 -> elt2 -> elt3 -> elt4 -> elt5 -> elt6 -> elt7 -> lst -> lst) (as :: Infinite elt1) (bs :: Infinite elt2) (cs :: Infinite elt3) (ds :: Infinite elt4) (fs :: Infinite elt6) (gs :: Infinite elt7) (g :: forall b. (elt5 -> b -> b) -> b).
  foldr7 cons as bs cs ds (build g) fs gs =
    g (foldr7_left (\e a b c d f g' -> cons a b c d e f g')) as bs cs ds fs gs
"foldr7/6" forall (cons :: elt1 -> elt2 -> elt3 -> elt4 -> elt5 -> elt6 -> elt7 -> lst -> lst) (as :: Infinite elt1) (bs :: Infinite elt2) (cs :: Infinite elt3) (ds :: Infinite elt4) (es :: Infinite elt5) (gs :: Infinite elt7) (g :: forall b. (elt6 -> b -> b) -> b).
  foldr7 cons as bs cs ds es (build g) gs =
    g (foldr7_left (\f a b c d e g' -> cons a b c d e f g')) as bs cs ds es gs
"foldr7/7" forall (cons :: elt1 -> elt2 -> elt3 -> elt4 -> elt5 -> elt6 -> elt7 -> lst -> lst) (as :: Infinite elt1) (bs :: Infinite elt2) (cs :: Infinite elt3) (ds :: Infinite elt4) (es :: Infinite elt5) (fs :: Infinite elt6) (g :: forall b. (elt7 -> b -> b) -> b).
  foldr7 cons as bs cs ds es fs (build g) =
    g (foldr7_left (\g' a b c d e f -> cons a b c d e f g')) as bs cs ds es fs
  #-}
