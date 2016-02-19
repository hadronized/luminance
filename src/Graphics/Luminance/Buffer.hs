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
    createBuffer
    -- * Buffer access
  , BufferRW
    -- * Buffer regions
  , Buffer 
  , BuildBuffer
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
