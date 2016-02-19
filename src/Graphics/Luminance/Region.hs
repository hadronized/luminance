-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015, 2016 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Region (
    -- * Regions
    Region
  , gpuRegion
  , newFrame
  , newShading
    -- * Drawing
  , drawGeometry
  ) where

import Graphics.Luminance.Core.Region
