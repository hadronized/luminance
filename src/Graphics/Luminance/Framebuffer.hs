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
import Control.Monad.Trans.Resource ( MonadResource, register )
import Foreign.Marshal.Alloc ( malloc )
import Foreign.Storable ( peek )
import Graphics.GL

newtype Framebuffer c d = Framebuffer { framebufferID :: GLint }

type ColorFramebuffer t f = Framebuffer (Color t f) Complete
type DepthFramebuffer t f = Framebuffer Complete (Depth t f)
type ColorDepthFramebuffer ct cf dt df = Framebuffer (Color ct cf) (Depth dt df)
type CompleteFramebuffer = Framebuffer Complete Complete -- not really totally complete

data Color t f

data Depth t f

data Complete

defaultFramebuffer :: CompleteFramebuffer
defaultFramebuffer = Framebuffer (-1)

mkFramebuffer :: (MonadIO m,MonadResource m) => m (Framebuffer c d)
mkFramebuffer = do
  (fid,p) <- liftIO $ do
    p <- malloc
    glGenFramebuffers 1 p
    fid <- peek p
    pure (fid,p)
  _ <- register $ glDeleteFramebuffers 1 p
  pure . Framebuffer $ fromIntegral fid

colorFramebuffer :: (MonadIO m,MonadResource m) => m (ColorFramebuffer t f)
colorFramebuffer = mkFramebuffer

depthFramebuffer :: (MonadIO m,MonadResource m) => m (DepthFramebuffer t f)
depthFramebuffer = mkFramebuffer

colorDepthFramebuffer :: (MonadIO m,MonadResource m)
                      => m (ColorDepthFramebuffer ct cf dt df)
colorDepthFramebuffer = mkFramebuffer

{-
attachColorTexture :: (MonadIO m)
                   => Framebuffer (Color t f) d
                   -> Texture t f
                   -> m (Either String (Framebuffer Complete d))
attachColorTexture fb tex = liftIO $ do
  -- ?!
-}
