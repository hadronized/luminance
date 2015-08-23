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

-- FIXME: we need to make a tighter link between c and blending and between d and the depth test.
data RenderCmd rw c d u a = RenderCmd (Maybe (BlendingMode,BlendingFactor,BlendingFactor)) Bool (U u) u a

instance Functor (RenderCmd rw c d u) where
  fmap f (RenderCmd blending depthTest uni u a) = RenderCmd blending depthTest uni u (f a)

renderCmd :: Maybe (BlendingMode,BlendingFactor,BlendingFactor)
          -> Bool
          -> U u
          -> u 
          -> a 
          -> RenderCmd rw c d u a
renderCmd = RenderCmd
