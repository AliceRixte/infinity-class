# Commutative Semigroup

[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Hackage](https://img.shields.io/hackage/v/infinity-class.svg)](https://hackage.haskell.org/package/infinity-class)  [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/AliceRixte/infinity-class/LICENSE)

A typeclass for types that can represent infinity.

```haskell
class (Num a, Ord a) => Infinity a where
  -- | A value representing infinity.
  infinity :: a
```
