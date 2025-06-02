--------------------------------------------------------------------------------
-- |
--
-- Module      :  Numeric.Infinity
-- Description :  Infinity typeclass
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
--
-- A typeclass for types that can represent infinity.
--------------------------------------------------------------------------------

module Numeric.Infinity
  ( Infinity(..)
  , isFinite
  , isInfinite
  , toRational'
  )
  where

import Prelude hiding (isInfinite)
import Data.Ord
import Data.Functor.Identity
import Data.Functor.Const
import Data.Functor.Compose
import Data.Ratio
import GHC.Real (Ratio ((:%)))
import Foreign.C.Types

import Data.Bounded.Orphans ()

-- | A typeclass for types that can represent infinity.
--
-- Being an instance of this class breaks the @'Num'@ laws.
--
-- As a matter of fact the @'Num'@ instances of @'Float'@ and @'Double'@ are
-- already breaking the @'Num'@ law that says that @x + 'negate' x ==
-- 'fromInteger' 0@ because @1//0 - 1//0 == NaN@.
--
-- Instances of this class must satisfy the following laws :
--
-- * @a < 'infinity'@ for all @a /= infinity@
--
-- [Bounded : ]
--
-- * @-'infinity'  == @ 'minBound'
-- * 'infinity' @ == 'maxBound'@
--
-- [Num :]
-- If @'Num' a@ holds, instances must satisfy the following law :
--
-- * @a > -'infinity'@ for all @a /= -infinity@
--
-- * Addition :
--
--     * @'infinity' + a == 'infinity'@ when @a /= -'infinity'@
--     * @ -'infinity' + a == -'infinity' @ when @ a /= 'infinity' @
--     * @'infinity' - 'infinity'@ is unspecified. (This is in contradiction
--       with the @'Num'@ law that says that @x + negate x@ = @fromInteger 0@)
--
-- * Multiplication :
--
--     * @'infinity' * a == 'infinity'@ when @a > 0@
--     * @'infinity' * a == -'infinity'@ when @a < 0@
--     * @'infinity' * 0 @, @0 * 'infinity'@, @-'infinity' * 0@ and
--       @0 * -'infinity'@ are all unspecified
--       (This is in contradiction with the @'Num'@  distributivity law combined
--       with the law @x + negate x@ = @fromInteger 0@)
--
-- * Absolute value :
--
--     * 'abs' @'infinity' == 'infinity'@
--     * 'abs' @(-'infinity') == 'infinity'@
--
-- * Signum :
--
--     * 'signum' @ 'infinity' == 1@
--     * 'signum' @ (-'infinity') == -1@
--
class (Ord a, Bounded a) => Infinity a where
  -- | A value representing infinity.
  infinity :: a


instance Integral a => Infinity (Ratio a) where
  infinity = 1 :% 0

instance Infinity Float where
  infinity = 1 / 0

instance Infinity Double where
  infinity = 1 / 0

instance Infinity CFloat where
  infinity = 1 / 0

instance Infinity CDouble where
  infinity = 1 / 0

instance Infinity a => Infinity (Identity a) where
  infinity = Identity infinity

instance Infinity a => Infinity (Const a b) where
  infinity = Const infinity

instance Infinity (f (g a)) => Infinity (Compose f g a) where
  infinity = Compose infinity

-- | Is the value finite?
--
isFinite :: (Num a, Infinity a) => a -> Bool
isFinite = not . isInfinite

-- | Is the value infinite?
--
isInfinite :: (Num a, Infinity a) => a -> Bool
isInfinite x = x == infinity || x == -infinity

-- | A version of 'toRational' that properly handles infinity.
--
-- As of April 15th 2025, @2 ^ 1024 % 1 == toRational (1 / 0 :: Double)@. For
-- this reason we need to handle infinity values as a special case.
--
-- See this stack overflow question for more details:
-- https://stackoverflow.com/questions/79574823/in-haskell-why-torational-1-0-infinity-is-false
toRational' :: (Infinity a, Real a) => a -> Rational
toRational' a
  | a == infinity = infinity
  | a == -infinity = -infinity
  | otherwise = toRational a
