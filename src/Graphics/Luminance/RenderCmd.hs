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

data RenderCmd rw c d a = RenderCmd (Maybe (BlendingMode,BlendingFactor,BlendingFactor)) Bool (Uniformed a)

instance Functor (RenderCmd rw c d) where
  fmap f (RenderCmd blending depthTest a) = RenderCmd blending depthTest (fmap f a)

instance Applicative (RenderCmd rw c d) where
  pure x = RenderCmd Nothing True (pure x)
  RenderCmd _ _ f <*> RenderCmd blending depthTest a = RenderCmd blending depthTest (f <*> a)
