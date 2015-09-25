-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.RenderCmd (
    -- * Render commands
    RenderCmd
  , renderCmd
    -- * Special render commands
  , stdRenderCmd
  , stdRenderCmd_
  ) where

import Graphics.Luminance.Core.RenderCmd
