{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015, 2016 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-----------------------------------------------------------------------------

module Graphics.Luminance.Driver where

import Control.Monad.Except ( MonadError )
import Data.Semigroup ( Semigroup )
import Graphics.Luminance.Core.RW ( Writable )
import Graphics.Luminance.Core.Shader.Program ( HasProgramError )
import Graphics.Luminance.Core.Shader.Stage ( HasStageError, StageType )
import Graphics.Luminance.BufferDriver
import Graphics.Luminance.FramebufferDriver
import Graphics.Luminance.GeometryDriver
import Graphics.Luminance.PixelDriver
import Graphics.Luminance.ShaderDriver
import Graphics.Luminance.TextureDriver

-- |A driver to implement to be considered as a luminance backend.
class (BufferDriver m, FramebufferDriver m,GeometryDriver m,PixelDriver m,ShaderDriver m,TextureDriver m) => Driver m where
  -- draw
  -- |Draw output.
  type Output m :: * -> * -> *
  type RenderCommand m :: * -> *
  -- |Issue a draw command to the GPU. Don’t be afraid of the type signature. Let’s explain it.
  --
  -- The first parameter is the framebuffer you want to perform the rendering in. It must be
  -- writable.
  --
  -- The second parameter is a list of /shading commands/. A shading command is composed of three
  -- parts:
  --
  -- * a 'Program' used for shading;
  -- * a @(a -> 'U'')@ uniform sink used to update uniforms in the program passed as first value;
  --   this is useful if you want to update uniforms only once per draw or for all render
  --   commands, like time, user event, etc.;
  -- * a list of /render commands/ function; that function enables you to update uniforms via the
  --   @(a -> 'U'')@ uniform sink for each render command that follows.
  --
  -- This function outputs yields a value of type @'Output' m c d'@, which represents the output of
  -- the render – typically, textures or '()'.
  --draw :: (Monoid (U' m),Writable w) => Framebuffer m w c d -> [(Program m a,a -> U' m,[a -> (U' m,RenderCommand m (Geometry m))])] -> m (Output m c d)
