-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Framebuffer where

import Graphics.Luminance.Memory ( GC )

data Framebuffer :: * -> * -> * where
  ColorOnlyFramebuffer  :: GC GLuint -> Framebuffer (Color t f) NoDepth
  DepthOnlyFramebuffer  :: GC GLuint -> Framebuffer NoColor (Depth t f)
  ColorDepthFramebuffer :: GC GLuint -> Framebuffer (Color ct cf) (Depth dt df)

data Color t f

data Depth t f

data NoColor

data NoDepth
