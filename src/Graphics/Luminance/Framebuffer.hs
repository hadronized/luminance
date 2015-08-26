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

import Control.Monad ( unless )
import Control.Monad.Except ( MonadError(..) )
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

---------------------------------------------------------------------------------
-- Framebuffer ------------------------------------------------------------------

newtype Framebuffer rw c d = Framebuffer { framebufferID :: GLuint } deriving (Eq,Show)

type ColorFramebuffer rw c = Framebuffer rw c ()
type DepthFramebuffer rw d = Framebuffer rw () d

newtype FramebufferError = IncompleteFramebuffer String deriving (Eq,Show)

class HasFramebufferError a where
  fromFramebufferError :: FramebufferError -> a

createFramebuffer :: forall c d e m rw. (HasFramebufferError e,MonadError e m,MonadIO m,MonadResource m,FramebufferColorAttachment c,FramebufferColorRW rw,FramebufferDepthAttachment d,FramebufferTarget rw)
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
  unless hasDepthOutput $ setDepthRenderbuffer fid w h
  _ <- register . with fid $ glDeleteFramebuffers 1
  status <- glCheckNamedFramebufferStatus fid $ framebufferTarget (Proxy :: Proxy rw)
  if 
    | status == GL_FRAMEBUFFER_COMPLETE -> pure (Framebuffer fid)
    | otherwise -> throwError . fromFramebufferError . IncompleteFramebuffer $ translateFramebufferStatus status

translateFramebufferStatus :: GLenum -> String
translateFramebufferStatus status = case status of
  GL_FRAMEBUFFER_UNDEFINED                     -> "undefined default read or write framebuffer"
  GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT         -> "incomplete attachment"
  GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT -> "missing image attachment"
  GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER        -> "incomplete draw buffer"
  GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER        -> "incomplete read buffer"
  GL_FRAMEBUFFER_UNSUPPORTED                   -> "unsupported (internal) format(s)"
  GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE        -> "incomplete multisample configuration"
  GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS      -> "layered attachment mismatch"
  _                                            -> "unknown error"

--------------------------------------------------------------------------------
-- Framebuffer attachment ------------------------------------------------------

data Attachment
  = ColorAttachment Natural
  | DepthAttachment
  deriving (Eq,Ord,Show)

fromAttachment :: (Eq a,Num a) => Attachment -> a
fromAttachment a = case a of
  ColorAttachment i -> GL_TEXTURE0 + fromIntegral i
  DepthAttachment   -> GL_DEPTH_ATTACHMENT

--------------------------------------------------------------------------------
-- Framebuffer color attachment ------------------------------------------------

class FramebufferColorAttachment c where
  addColorOutput :: (MonadIO m,MonadResource m)
                 => GLuint
                 -> Natural
                 -> Natural
                 -> Natural
                 -> Natural
                 -> proxy c
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

colorAttachmentsFromMax :: Natural -> [GLenum]
colorAttachmentsFromMax m = [fromAttachment (ColorAttachment a) | a <- [0..m-1]]

setColorBuffers :: forall m proxy rw. (FramebufferColorRW rw,MonadIO m)
                => GLuint 
                -> Natural
                -> proxy rw
                -> m ()
setColorBuffers fid colorOutputNb _ = case colorOutputNb of
  0 -> do
    -- disable color outputs
    glNamedFramebufferDrawBuffer fid GL_NONE
    glNamedFramebufferReadBuffer fid GL_NONE
  _ -> setFramebufferColorRW fid colorOutputNb (Proxy :: Proxy rw)

--------------------------------------------------------------------------------
-- Framebuffer depth attachment ------------------------------------------------

class FramebufferDepthAttachment d where
  addDepthOutput :: (MonadIO m,MonadResource m)
                 => GLuint
                 -> Natural
                 -> Natural
                 -> Natural
                 -> proxy d
                 -> m Bool

instance FramebufferDepthAttachment () where
  addDepthOutput _ _ _ _ _ = pure False

instance (Pixel (Format t (CDepth d))) => FramebufferDepthAttachment (Format t (CDepth d)) where
  addDepthOutput fid w h mipmaps proxy = addOutput fid DepthAttachment w h mipmaps proxy >> pure True

setDepthRenderbuffer :: (MonadIO m,MonadResource m)
                     => GLuint
                     -> Natural
                     -> Natural
                     -> m ()
setDepthRenderbuffer fid w h = do
  renderbuffer <- createRenderbuffer w h (Proxy :: Proxy Depth32F)
  glNamedFramebufferRenderbuffer fid (fromAttachment DepthAttachment) GL_RENDERBUFFER
    (renderbufferID renderbuffer)

--------------------------------------------------------------------------------
-- Framebuffer color read/write configuration ----------------------------------

class FramebufferColorRW rw where
  setFramebufferColorRW :: (MonadIO m) => GLuint -> Natural -> proxy rw -> m ()

instance FramebufferColorRW W where
  setFramebufferColorRW fid nb _ = liftIO $ do
    withArrayLen (colorAttachmentsFromMax nb) $ \n buffers ->
      glNamedFramebufferDrawBuffers fid (fromIntegral n) buffers

--------------------------------------------------------------------------------
-- Framebuffer read/write target configuration ---------------------------------

class FramebufferTarget rw where
  framebufferTarget :: proxy rw -> GLenum

instance FramebufferTarget W where
  framebufferTarget _ = GL_DRAW_FRAMEBUFFER


--------------------------------------------------------------------------------
-- Framebuffer outputs ---------------------------------------------------------

addOutput :: forall m p proxy. (MonadIO m,MonadResource m,Pixel p)
          => GLuint
          -> Attachment
          -> Natural
          -> Natural
          -> Natural
          -> proxy p
          -> m ()
addOutput fid ca w h mipmaps _ = do
  tex :: Texture2D p <- createTexture w h mipmaps
  liftIO $ glNamedFramebufferTexture fid (fromAttachment ca)
    (textureID tex) 0

--------------------------------------------------------------------------------
-- Special framebuffers --------------------------------------------------------

defaultFramebuffer :: Framebuffer rw c d
defaultFramebuffer = Framebuffer 0
