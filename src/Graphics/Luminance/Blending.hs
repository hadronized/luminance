-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- That module exports blending-related types and functions.
--
-- Given two pixels @src@ and @dst@ – source and destination, we associate
-- each pixel a /blending factor/ – respectively, @srcK@ and @dstK@. @src@ is
-- the pixel being computed, and @dst@ is the pixel that is already stored in
-- the framebuffer.
--
-- The pixels can be blended in several ways. See the documentation of
-- 'BlendingMode' for further details.
--
-- The factors are encoded with 'BlendingFactor'.
-----------------------------------------------------------------------------

module Graphics.Luminance.Blending (
    -- * Blending modes
    BlendingMode(..)
    -- * Blending factors
  , BlendingFactor(..)
  ) where

import Graphics.Luminance.Core.Blending
