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
import Graphics.Luminance.Shader.Uniform ( U(..) )

data RenderCmd rw c d u a = RenderCmd (Maybe (BlendingMode,BlendingFactor,BlendingFactor)) Bool (U u) u a

instance Functor (RenderCmd rw c d u) where
  fmap f (RenderCmd blending depthTest uni u a) = RenderCmd blending depthTest uni u (f a)
