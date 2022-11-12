# infinite-list

Modern lightweight library for infinite lists with fusion:

* API similar to `Data.List`.
* No non-boot dependencies.
* Avoid dangerous instances like `Foldable`.
* Provide fusion framework.
* Use `NonEmpty` where applicable.

```haskell
{-# LANGUAGE PostfixOperators #-}
import Data.List.Infinite (Infinite(..), (...))
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
