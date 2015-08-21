-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.RenderCmd where

import Graphics.Luminance.Blending
import Graphics.Luminance.Shader.Uniform ( Uniformed(..) )

data RenderCmd rw c d a = RenderCmd (Maybe (BlendingMode,BlendingFactor)) Bool (Uniformed a)
