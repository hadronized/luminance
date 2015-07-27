{-# LANGUAGE UndecidableInstances #-}

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
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Utils ( with )
import Foreign.Storable ( peek )
import Graphics.GL
import Graphics.Luminance.Pixel ( Format(..), Pixel )
import Graphics.Luminance.Texture ( Texture2D(textureID), createTexture )
import Numeric.Natural ( Natural )

data Framebuffer rw c d = Framebuffer {
    framebufferID :: GLint
  , framebufferW  :: Natural
  , framebufferH  :: Natural
  , framebufferMM :: Natural
  } deriving (Eq,Show)

type ColorFramebuffer rw c = Framebuffer rw c ()
type DepthFramebuffer rw d = Framebuffer rw () d

-- |A chain of types, right-associated.
data a :. b = a :. b deriving (Eq,Functor,Ord,Show)

infixr 6 :.

newtype ColorAttachment = ColorAttachment
  { colorAttachment :: Natural } deriving (Eq,Ord,Show)

fromColorAttachment :: (Eq a,Num a) => ColorAttachment -> a
fromColorAttachment (ColorAttachment i) = GL_TEXTURE0 + fromIntegral i

createFramebuffer :: forall c d m rw. (MonadIO m,MonadResource m,FramebufferColorAttachment c)
                  => Natural
                  -> Natural
                  -> Natural
                  -> m (Framebuffer rw c d)
createFramebuffer w h mipmaps = do
  fid <- liftIO . alloca $ \p -> do
    glGenFramebuffers 1 p
    peek p
  createColorTexture (ColorAttachment 0) (undefined :: c) fid w h mipmaps
  _ <- register . with fid $ glDeleteFramebuffers 1
  pure $ Framebuffer (fromIntegral fid) w h mipmaps

class FramebufferColorAttachment a where
  createColorTexture :: (MonadIO m,MonadResource m)
                     => ColorAttachment
                     -> a
                     -> GLuint
                     -> Natural
                     -> Natural
                     -> Natural
                     -> m ()

instance FramebufferColorAttachment () where
  createColorTexture _ _ _ _ _ _ = pure ()

instance (Pixel (Format t c)) => FramebufferColorAttachment (Format t c) where
  createColorTexture ca _ fid w h mipmaps = do
    tex :: Texture2D (Format t c) <- createTexture w h mipmaps
    liftIO $ glNamedFramebufferTexture fid (fromColorAttachment ca)
      (textureID tex) 0

instance (FramebufferColorAttachment a,FramebufferColorAttachment b) => FramebufferColorAttachment (a :. b) where
  createColorTexture ca@(ColorAttachment i) _ fid w h mipmaps = do
    createColorTexture ca (undefined :: a) fid w h mipmaps
    createColorTexture (ColorAttachment $ succ i) (undefined :: b) fid w h mipmaps
