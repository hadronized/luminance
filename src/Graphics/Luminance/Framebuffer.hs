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
import Data.Proxy ( Proxy(..) )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Utils ( with )
import Foreign.Storable ( peek )
import Graphics.GL
import Graphics.Luminance.Pixel ( Format(..), Pixel )
import Graphics.Luminance.Texture ( Texture2D(textureID), createTexture )
import Graphics.Luminance.Tuple ( (:.) )
import Numeric.Natural ( Natural )

data Framebuffer rw c d = Framebuffer {
    framebufferID :: GLuint
  , framebufferW  :: Natural
  , framebufferH  :: Natural
  , framebufferMM :: Natural
  } deriving (Eq,Show)

type ColorFramebuffer rw c = Framebuffer rw c ()
type DepthFramebuffer rw d = Framebuffer rw () d

data Attachment
  = ColorAttachment Natural
  | DepthAttachment
  deriving (Eq,Ord,Show)

fromAttachment :: (Eq a,Num a) => Attachment -> a
fromAttachment a = case a of
  ColorAttachment i -> GL_TEXTURE0 + fromIntegral i
  DepthAttachment   -> GL_DEPTH_ATTACHMENT

createFramebuffer :: forall c d m rw. (MonadIO m,MonadResource m,FramebufferAttachment c,FramebufferAttachment d)
                  => Natural
                  -> Natural
                  -> Natural
                  -> m (Framebuffer rw c d)
createFramebuffer w h mipmaps = do
  fid <- liftIO . alloca $ \p -> do
    glCreateFramebuffers 1 p
    peek p
  createFramebufferTexture (ColorAttachment 0) (Proxy :: Proxy c) fid w h mipmaps
  createFramebufferTexture DepthAttachment (Proxy :: Proxy d) fid w h mipmaps
  let colorOutputsLength = attachmentLength (Proxy :: Proxy c)
      hasDepth = attachmentLength (Proxy :: Proxy d) == 1
  configureFramebufferBuffers fid colorOutputsLength hasDepth
  _ <- register . with fid $ glDeleteFramebuffers 1
  pure $ Framebuffer fid w h mipmaps

class FramebufferAttachment a where
  createFramebufferTexture :: (MonadIO m,MonadResource m)
                           => Attachment
                           -> Proxy a
                           -> GLuint
                           -> Natural
                           -> Natural
                           -> Natural
                           -> m ()
  attachmentLength         :: Proxy a -> Natural

instance FramebufferAttachment () where
  createFramebufferTexture _ _ _ _ _ _ = pure ()
  attachmentLength _ = 0

instance (Pixel (Format t c)) => FramebufferAttachment (Format t c) where
  createFramebufferTexture ca _ fid w h mipmaps = do
    tex :: Texture2D (Format t c) <- createTexture w h mipmaps
    liftIO $ glNamedFramebufferTexture fid (fromAttachment ca)
      (textureID tex) 0
  attachmentLength _ = 1

instance (FramebufferAttachment a,FramebufferAttachment b) => FramebufferAttachment (a :. b) where
  createFramebufferTexture ca _ fid w h mipmaps = case ca of
    ColorAttachment i -> do
      _ <- createFramebufferTexture ca (Proxy :: Proxy a) fid w h mipmaps
      createFramebufferTexture (ColorAttachment $ succ i) (Proxy :: Proxy b) fid w h mipmaps
    _ -> pure ()
  attachmentLength _ = attachmentLength (Proxy :: Proxy a) + attachmentLength (Proxy :: Proxy b)

configureFramebufferBuffers :: (MonadIO m,MonadResource m)
                            => GLuint
                            -> Natural
                            -> Bool
                            -> m ()
configureFramebufferBuffers fid colorOutputs hasDepth
  | colorOutputs == 0 && hasDepth = do
      glNamedFramebufferDrawBuffer fid GL_NONE
      glNamedFramebufferReadBuffer fid GL_NONE
  | otherwise = pure ()
