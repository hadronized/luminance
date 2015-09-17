-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Pixel (
    -- * Channel size
    ChannelSize
  , C8(..)
  , C16(..)
  , C32(..)
    -- * Channel type
  , ChannelType
  , CInts(..)
  , CUInts(..)
  , CFloats(..)
    -- * Channel shape
  , CR(..)
  , CRG(..)
  , CRGB(..)
  , CRGBA(..)
  , CDepth(..)
    -- * Pixel format
  , Format(..)
  , Pixel
  , RGB8UI
  , RGBA8UI
  , RGBA8F
  , RGB32F
  , RGBA32F
  , Depth32F
    -- * Color pixel
  , ColorPixel
  ) where

import Graphics.Luminance.Core.Pixel
