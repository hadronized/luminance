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

data Framebuffer c d = Framebuffer { framebufferID :: GC GLint }

type ColorFramebuffer t f = Framebuffer (Color t f) NoDepth
type DepthFramebuffer t f = Framebuffer NoColor (Depth t f)
type ColorDepthFramebuffer ct cf dt df = Framebuffer (Color ct cf) (Depth dt df)
type CompleteFramebuffer = Framebuffer NoColor NoDepth

data Color t f

data Depth t f

data NoColor

data NoDepth

defaultFramebuffer :: (MonadIO m) => m CompleteFramebuffer
defaultFramebuffer = Framebuffer <$> embedGC -1 (pure ())

mkFramebuffer :: (MonadIO m) => m (Framebuffer c d)
mkFramebuffer nb = liftIO $ do
  p <- malloc
  glGenFramebuffers 1 p
  peek p >>= $ \fb -> Framebuffer <$> embdGC fb (glDeleteFramebuffers 1 p)
