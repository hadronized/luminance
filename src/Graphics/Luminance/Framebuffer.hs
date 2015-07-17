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

import Control.Monad.IO.Class ( MonadIO(..) )
import Graphics.GL
import Graphics.Luminance.Memory

newtype Framebuffer c d = Framebuffer { framebufferID :: GC GLint }

type ColorFramebuffer t f = Framebuffer (Color t f) Complete
type DepthFramebuffer t f = Framebuffer Complete (Depth t f)
type ColorDepthFramebuffer ct cf dt df = Framebuffer (Color ct cf) (Depth dt df)
type CompleteFramebuffer = Framebuffer Complete Complete -- not really totally complete

data Color t f

data Depth t f

data Complete

defaultFramebuffer :: (MonadIO m) => m CompleteFramebuffer
defaultFramebuffer = Framebuffer <$> embedGC (-1) (pure ())

mkFramebuffer :: (MonadIO m) => m (Framebuffer c d)
mkFramebuffer = liftIO $ do
  p <- malloc
  glGenFramebuffers 1 p
  peek p >>= \fid ->
    Framebuffer <$> embedGC (fromIntegral fid) (glDeleteFramebuffers 1 p)

colorFramebuffer :: (MonadIO m) => t -> f -> m (ColorFramebuffer t f)
colorFramebuffer _ _ = mkFramebuffer

depthFramebuffer :: (MonadIO m) => t -> f -> m (DepthFramebuffer t f)
depthFramebuffer _ _ = mkFramebuffer

colorDepthFramebuffer :: (MonadIO m)
                      => ct
                      -> cf
                      -> dt
                      -> df
                      -> m (ColorDepthFramebuffer ct cf dt df)
colorDepthFramebuffer _ _ _ _ = mkFramebuffer

{-
attachColorTexture :: (MonadIO m)
                   => Framebuffer (Color t f) d
                   -> Texture t f
                   -> m (Either String (Framebuffer Complete d))
attachColorTexture fb tex = liftIO $ do
  -- ?!
-}
