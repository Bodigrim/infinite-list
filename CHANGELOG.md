# 0.1.2

* Add `heteroZip` and `heteroZipWith`.
* Add `traverse_` and `for_`.

# 0.1.1

* Add `mapMaybe` and `catMaybes`.
* Add `mapEither` and `partitionEithers`.
* Decrease operator precedence for `(...)` and `(....)`.
* Add fusion rules for `genericTake`.
* Remove harmful fusion rules for `drop` and `dropWhile`.
  Cf. https://gitlab.haskell.org/ghc/ghc/-/issues/23021.
* Fix `instance Monad Infinite` on 32-bit machines.
  It was violating monad laws once the index exceeds 2^32.

# 0.1

* Initial release.
