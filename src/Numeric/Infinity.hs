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
  )
  where

import Data.Ord
import Data.Functor.Identity
import Data.Functor.Const
import Data.Functor.Compose
import Data.Ratio
import Foreign.C.Types



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
-- [Ord :]
--
--  * @a < 'infinity'@ for all @a /= infinity@
--  * @a > -'infinity'@ for all @a /= -infinity@
--
-- [Num :]
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
class (Num a, Ord a) => Infinity a where
  -- | A value representing infinity.
  infinity :: a


instance Integral a => Infinity (Ratio a) where
  infinity = 1 % 0
  {-# INLINE infinity #-}

instance Infinity Float where
  infinity = 1 / 0
  {-# INLINE infinity #-}

instance Infinity Double where
  infinity = 1 / 0
  {-# INLINE infinity #-}

instance Infinity CFloat where
  infinity = 1 / 0
  {-# INLINE infinity #-}

instance Infinity CDouble where
  infinity = 1 / 0
  {-# INLINE infinity #-}

instance Infinity a => Infinity (Identity a) where
  infinity = Identity infinity
  {-# INLINE infinity #-}

instance Infinity a => Infinity (Const a b) where
  infinity = Const infinity
  {-# INLINE infinity #-}

instance Infinity a => Infinity (Down a) where
  infinity = Down infinity
  {-# INLINE infinity #-}

instance Infinity (f (g a)) => Infinity (Compose f g a) where
  infinity = Compose infinity
  {-# INLINE infinity #-}