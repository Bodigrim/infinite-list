# infinite-list

Modern lightweight library for infinite lists with fusion:

* API similar to `Data.List`.
* No non-boot dependencies.
* Avoid dangerous instances like `Foldable`.
* Provide fusion framework.
* Use `NonEmpty` where applicable.
* Use `Word` for indices.
* Be lazy, but not too lazy.

```haskell
{-# LANGUAGE PostfixOperators #-}
import Data.List.Infinite (Infinite(..), (...), (....))
import qualified Data.List.Infinite as Inf
```

## Prior art and inspiration

* [`Data.Stream.Infinite`](https://hackage.haskell.org/package/streams/docs/Data-Stream-Infinite.html) from [`streams`](https://hackage.haskell.org/package/streams) package:
  * Large dependency footprint, e. g., `adjunctions`.
  * Provides dangerous instances such as `Foldable`.
  * No fusion framework.

* [`Data.Stream`](https://hackage.haskell.org/package/Stream/docs/Data-Stream.html) from [`Stream`](https://hackage.haskell.org/package/Stream) package:
  * No fusion framework.
  * No repository or issue tracker.

* [`GHC.Data.List.Infinite`](https://gitlab.haskell.org/ghc/ghc/-/blob/080fffa1015bcc0cff8ab4ad1eeb507fb7a13383/compiler/GHC/Data/List/Infinite.hs) in GHC source tree:
  * Limited API, only to cater for GHC internals.
  * Not available as a separate package outside of GHC.

## Why no `Foldable` or `Traversable`?

The breakdown of members of `Foldable` is as follows:

* `foldr`, `foldr1`, `foldMap`, `fold`, `toList` and `null` can be productive on infinite lists;
* `foldr'`, `foldMap'` cannot, because forcing an accumulator even to a WHNF makes fold non-terminating;
* `foldl`, `foldl'`, `foldl'` cannot, because no left fold can;
* `length` always diverges;
* `elem` either returns `True`, or does not terminate, but never returns `False`;
* `maximum`, `minimum`, `sum` and `product` are unlikely to be productive, unless an underlying `instance Ord` or `instance Num` is extremely lazy.

Altogether it means that code, polymorphic by `Foldable`, cannot confidently work with infinite lists. Even a trivial refactoring can get you in a deep trouble. It's better to save users from this pitfall and do not provide `instance Foldable` at all. We do provide a right fold however.

Since there is no `Foldable`, there could be no `Traversable`. Even if it was not prohibited because of a missing superclass, there are only a few monads, which are lazy enough to be productive for infinite traversals. If you are looking for a traverse with a lazy state, use `mapAccumL`.

## Laziness

Operations, returning a data type with a single constructor, can be implemented in an extremely lazy fashion. Namely, always return the constructor before inspecting any of the arguments. For instance, note the irrefuttable pattern mathing in `Data.List.NonEmpty`:

```haskell
map :: (a -> b) -> NonEmpty a -> NonEmpty b
map f ~(a :| as) = f a :| fmap f as
```

Because of it forcing the result to WHNF does not force any of the arguments, e. g., ``Data.List.NonEmpty.map undefined undefined `seq` 1`` returns `1`. This is not the case for normal lists: since there are two constructors, `map` has to inspect the argument before returning anything, and ``Data.List.map undefined undefined `seq` 1`` throws an error.

While `Data.List.Infinite` has a single constructor, we believe that following the example of `Data.List.NonEmpty` is harmful for the majority of applications. Instead the laziness of the API is modelled on the laziness of respective operations on `Data.List`: a function `Data.List.Infinite.foo` operating over `Infinite a` is expected to have the same strictness properties as `Data.List.foo` operating over `[a]`. For instance, ``Data.List.Infinite.map undefined undefined `seq` 1`` diverges.
