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

import Control.Monad ( when )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Resource ( MonadResource, register )
import Data.Proxy ( Proxy(..) )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( withArrayLen )
import Foreign.Marshal.Utils ( with )
import Foreign.Storable ( peek )
import Graphics.GL
import Graphics.Luminance.Pixel
import Graphics.Luminance.Renderbuffer ( createRenderbuffer, renderbufferID )
import Graphics.Luminance.RW
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

createFramebuffer :: forall c d m rw. (MonadIO m,MonadResource m,FramebufferColorAttachment c,FramebufferColorRW rw,FramebufferDepthAttachment d)
                  => Natural
                  -> Natural
                  -> Natural
                  -> m (Framebuffer rw c d)
createFramebuffer w h mipmaps = do
  fid <- liftIO . alloca $ \p -> do
    glCreateFramebuffers 1 p
    peek p
  colorOutputNb <- addColorOutput fid 0 w h mipmaps (Proxy :: Proxy c)
  hasDepthOutput <- addDepthOutput fid w h mipmaps (Proxy :: Proxy d)
  setColorBuffers fid colorOutputNb (Proxy :: Proxy rw)
  when hasDepthOutput $ setDepthRenderbuffer fid w h
  _ <- register . with fid $ glDeleteFramebuffers 1
  pure $ Framebuffer fid w h mipmaps

--------------------------------------------------------------------------------
-- Framebuffer attachments -----------------------------------------------------

class FramebufferColorAttachment c where
  addColorOutput :: (MonadIO m,MonadResource m)
                 => GLuint
                 -> Natural
                 -> Natural
                 -> Natural
                 -> Natural
                 -> Proxy c
                 -> m Natural

instance FramebufferColorAttachment () where
  addColorOutput _ _ _ _ _ _ = pure 0

instance (ColorPixel (Format t c)) => FramebufferColorAttachment (Format t c) where
  addColorOutput fid ca w h mipmaps proxy = do
    addOutput fid (ColorAttachment ca) w h mipmaps proxy
    pure 1

instance (ColorPixel a,ColorPixel b,FramebufferColorAttachment a,FramebufferColorAttachment b) => FramebufferColorAttachment (a :. b) where
  addColorOutput fid ca w h mipmaps _ = do
    d0 <- addColorOutput fid ca w h mipmaps (Proxy :: Proxy a)
    d1 <- addColorOutput fid (succ ca) w h mipmaps (Proxy :: Proxy b)
    pure $ d0 + d1

class FramebufferDepthAttachment d where
  addDepthOutput :: (MonadIO m,MonadResource m)
                 => GLuint
                 -> Natural
                 -> Natural
                 -> Natural
                 -> Proxy d
                 -> m Bool

instance FramebufferDepthAttachment () where
  addDepthOutput _ _ _ _ _ = pure False

instance (Pixel (Format t (CDepth d))) => FramebufferDepthAttachment (Format t (CDepth d)) where
  addDepthOutput fid w h mipmaps proxy = addOutput fid DepthAttachment w h mipmaps proxy >> pure True

class FramebufferColorRW rw where
  setFramebufferColorRW :: (MonadIO m) => GLuint -> Natural -> Proxy rw -> m ()

instance FramebufferColorRW W where
  setFramebufferColorRW fid nb _ = liftIO $ do
    withArrayLen (colorAttachmentsFromMax nb) $ \n buffers ->
      glNamedFramebufferDrawBuffers fid (fromIntegral n) buffers

colorAttachmentsFromMax :: Natural -> [GLenum]
colorAttachmentsFromMax m = [fromAttachment (ColorAttachment a) | a <- [0..m-1]]

addOutput :: forall m p. (MonadIO m,MonadResource m,Pixel p)
          => GLuint
          -> Attachment
          -> Natural
          -> Natural
          -> Natural
          -> Proxy p
          -> m ()
addOutput fid ca w h mipmaps _ = do
  tex :: Texture2D p <- createTexture w h mipmaps
  liftIO $ glNamedFramebufferTexture fid (fromAttachment ca)
    (textureID tex) 0

setColorBuffers :: forall m rw. (FramebufferColorRW rw,MonadIO m)
                => GLuint 
                -> Natural
                -> Proxy rw
                -> m ()
setColorBuffers fid colorOutputNb _ = case colorOutputNb of
  0 -> do
    -- disable color outputs
    glNamedFramebufferDrawBuffer fid GL_NONE
    glNamedFramebufferReadBuffer fid GL_NONE
  _ -> setFramebufferColorRW fid colorOutputNb (Proxy :: Proxy rw)

setDepthRenderbuffer :: (MonadIO m,MonadResource m)
                     => GLuint
                     -> Natural
                     -> Natural
                     -> m ()
setDepthRenderbuffer fid w h = do
  renderbuffer <- createRenderbuffer w h (Proxy :: Proxy Depth32F)
  glNamedFramebufferRenderbuffer fid (fromAttachment DepthAttachment) GL_RENDERBUFFER
    (renderbufferID renderbuffer)

