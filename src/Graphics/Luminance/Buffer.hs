-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015, 2016 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Buffer (
    -- * Buffer creation
    Buffer
  , bufferID
  , createBuffer
  , createBuffer_
    -- * Buffer access
  , BufferRW
    -- * Buffer regions
  , Region
  , BuildRegion
  , newRegion
    -- * Operations on buffer regions
  , readWhole
  , writeWhole
  , fill
  , (@?)
  , (@!)
  , writeAt
  , writeAt'
  ) where

import Graphics.Luminance.Core.Buffer
