

{-# OPTIONS_GHC -fno-warn-orphans #-}

--------------------------------------------------------------------------------
-- |
--
-- Module      : Data.Bounded.Orphans
-- Description :  Bounded orphan instances using infinity
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
--
-- Bounded orphan instances using infinity.

-- For more insights, see this thread on the Haskell mailing list :
-- https://mail.haskell.org/pipermail/haskell/2005-March/015490.html
--
--------------------------------------------------------------------------------

module Data.Bounded.Orphans
  () where

import Foreign.C.Types
import Data.Ratio

instance Bounded Float where
  minBound = -1/0
  maxBound = 1/0

instance Bounded Double where
  minBound = -1/0
  maxBound = 1/0

instance Bounded CFloat where
  minBound = -1/0
  maxBound = 1/0

instance Bounded CDouble where
  minBound = -1/0
  maxBound = 1/0

instance Integral a => Bounded (Ratio a) where
  minBound = -1 % 0
  maxBound = 1 % 0
