{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Core.Framebuffer where

import Control.Monad ( unless )
import Control.Monad.Except ( MonadError(..) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Resource ( MonadResource, register )
import Data.Bits ( (.|.) )
import Data.Proxy ( Proxy(..) )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( withArrayLen )
import Foreign.Marshal.Utils ( with )
import Foreign.Storable ( peek )
import Graphics.GL
import Graphics.Luminance.Core.Debug
import Graphics.Luminance.Core.Pixel
import Graphics.Luminance.Core.Renderbuffer ( createRenderbuffer, renderbufferID )
import Graphics.Luminance.Core.RW
import Graphics.Luminance.Core.Texture
import Graphics.Luminance.Core.Texture2D
import Graphics.Luminance.Core.Tuple
import Numeric.Natural ( Natural )

---------------------------------------------------------------------------------
-- Framebuffer ------------------------------------------------------------------

-- |A 'Framebuffer' represents two buffers: a /color/ buffer and /depth/ buffer.
-- You can select which one you want and specify the formats to use by providing 'Pixel'
-- types. If you want to mute a buffer, use '()'.
data Framebuffer rw c d = Framebuffer {
    framebufferID :: GLuint
  , framebufferOutput :: Output c d
  }

-- |A 'Framebuffer' with the /depth/ buffer muted.
type ColorFramebuffer rw c = Framebuffer rw c ()

-- |A 'Framebuffer' with the /color/ buffer muted. Can be used to implement fast pre-passes.
type DepthFramebuffer rw d = Framebuffer rw () d

-- |@'createFramebuffer' w h mipmaps@ creates a new 'Framebuffer' with dimension @w * h@ and
-- allocating spaces for @mipmaps@ level of textures. The textures are created by providing a
-- correct type.
--
-- For the color part, you can pass either:
--
-- - '()': that will mute the color buffer of the framebuffer;
-- - @'Format' t c@: that will create a single texture with the wished color format;
-- - or @a ':.' b@: that will create a chain of textures; 'a' and 'b' cannot be '()'.
--
-- For the depth part, you can pass either:
--
-- - '()': that will mute the depth buffer of the framebuffer;
-- - @'Format' t c@: that will create a single texture with the wished depth format.
--
-- Finally, the @rw@ parameter can be set to 'R', 'W' or 'RW' to specify which kind of framebuffer
-- access you’ll need.
createFramebuffer :: forall c d e m rw. (HasFramebufferError e,MonadError e m,MonadIO m,MonadResource m,FramebufferColorAttachment c,FramebufferColorRW rw,FramebufferDepthAttachment d,FramebufferTarget rw)
                  => Natural
                  -> Natural
                  -> Natural
                  -> m (Framebuffer rw c d)
#ifdef __GL45
createFramebuffer w h mipmaps = do
  fid <- liftIO . alloca $ \p -> do
    debugGL $ glCreateFramebuffers 1 p
    peek p
  (colorOutputNb,colorTexs) <- addColorOutput fid 0 w h mipmaps (Proxy :: Proxy c)
  (hasDepthOutput,depthTex) <- addDepthOutput fid w h mipmaps (Proxy :: Proxy d)
  setColorBuffers fid colorOutputNb (Proxy :: Proxy rw)
  unless hasDepthOutput $ setDepthRenderbuffer fid w h
  _ <- register . with fid $ glDeleteFramebuffers 1
  status <- glCheckNamedFramebufferStatus fid $ framebufferTarget (Proxy :: Proxy rw)
  if 
    | status == GL_FRAMEBUFFER_COMPLETE -> pure $ Framebuffer fid (Output colorTexs depthTex)
    | otherwise -> throwError . fromFramebufferError . IncompleteFramebuffer $ translateFramebufferStatus status
#elif defined(__GL33)
createFramebuffer w h mipmaps = do
  fid <- liftIO . alloca $ \p -> do
    debugGL $ glGenFramebuffers 1 p
    peek p
  glBindFramebuffer GL_FRAMEBUFFER fid
  (colorOutputNb,colorTexs) <- addColorOutput fid 0 w h mipmaps (Proxy :: Proxy c)
  (hasDepthOutput,depthTex) <- addDepthOutput fid w h mipmaps (Proxy :: Proxy d)
  setColorBuffers fid colorOutputNb (Proxy :: Proxy rw)
  unless hasDepthOutput $ setDepthRenderbuffer fid w h
  _ <- register . with fid $ glDeleteFramebuffers 1
  status <- glCheckFramebufferStatus GL_FRAMEBUFFER
  if 
    | status == GL_FRAMEBUFFER_COMPLETE -> pure $ Framebuffer fid (Output colorTexs depthTex)
    | otherwise -> throwError . fromFramebufferError . IncompleteFramebuffer $ translateFramebufferStatus status
#endif

-- Translate OpenGL framebuffer status into human readable versions.
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

-- Framebuffer attachment.
data Attachment
  = ColorAttachment Natural
  | DepthAttachment
  deriving (Eq,Ord,Show)

fromAttachment :: Attachment -> GLenum
fromAttachment a = case a of
  ColorAttachment i -> GL_COLOR_ATTACHMENT0 + fromIntegral i
  DepthAttachment   -> GL_DEPTH_ATTACHMENT

--------------------------------------------------------------------------------
-- Framebuffer color attachment ------------------------------------------------

-- |Typeclass of possible framebuffer color attachments.
class FramebufferColorAttachment c where
  addColorOutput :: (MonadIO m,MonadResource m)
                 => GLuint
                 -> Natural
                 -> Natural
                 -> Natural
                 -> Natural
                 -> proxy c
                 -> m (Natural,TexturizeFormat c)

instance FramebufferColorAttachment () where
  addColorOutput _ _ _ _ _ _ = pure (0,())

instance (ColorPixel (Format t c)) => FramebufferColorAttachment (Format t c) where
  addColorOutput fid ca w h mipmaps proxy =
    fmap (1,) $ addOutput fid (ColorAttachment ca) w h mipmaps proxy

instance (ColorPixel a,ColorPixel b,FramebufferColorAttachment a,FramebufferColorAttachment b) => FramebufferColorAttachment (a :. b) where
  addColorOutput fid ca w h mipmaps _ = do
    (d0,tex0) <- addColorOutput fid ca w h mipmaps (Proxy :: Proxy a)
    (d1,tex1) <- addColorOutput fid (succ ca) w h mipmaps (Proxy :: Proxy b)
    pure $ (d0 + d1,tex0 :. tex1)

-- Given the maximum number of attachments in a color attachment, retrieve the list of OpenGL
-- enumerations to represent all the color buffers.
colorAttachmentsFromMax :: Natural -> [GLenum]
colorAttachmentsFromMax m = [fromAttachment (ColorAttachment a) | a <- [0..m-1]]

-- Set the OpenGL color buffers.
setColorBuffers :: forall m proxy rw. (FramebufferColorRW rw,MonadIO m)
                => GLuint 
                -> Natural
                -> proxy rw
                -> m ()
#ifdef __GL45
setColorBuffers fid colorOutputNb _ = case colorOutputNb of
  0 -> do
    -- disable color outputs
    debugGL $ glNamedFramebufferDrawBuffer fid GL_NONE
    debugGL $ glNamedFramebufferReadBuffer fid GL_NONE
  _ -> setFramebufferColorRW fid colorOutputNb (Proxy :: Proxy rw)
#elif defined(__GL33)
setColorBuffers fid colorOutputNb _ = case colorOutputNb of
  0 -> do
    -- disable color outputs
    debugGL $ glDrawBuffer GL_NONE
    debugGL $ glReadBuffer GL_NONE
  _ -> setFramebufferColorRW fid colorOutputNb (Proxy :: Proxy rw)
#endif

--------------------------------------------------------------------------------
-- Framebuffer depth attachment ------------------------------------------------

-- |Typeclass of possible framebuffer depth attachments.
class FramebufferDepthAttachment d where
  addDepthOutput :: (MonadIO m,MonadResource m)
                 => GLuint
                 -> Natural
                 -> Natural
                 -> Natural
                 -> proxy d
                 -> m (Bool,TexturizeFormat d)

instance FramebufferDepthAttachment () where
  addDepthOutput _ _ _ _ _ = pure (False,())

instance (Pixel (Format t (CDepth d))) => FramebufferDepthAttachment (Format t (CDepth d)) where
  addDepthOutput fid w h mipmaps proxy = fmap (True,) $ addOutput fid DepthAttachment w h mipmaps proxy

-- Create a renderbuffer used to mute depth information and link it to a framebuffer.
setDepthRenderbuffer :: (MonadIO m,MonadResource m)
                     => GLuint
                     -> Natural
                     -> Natural
                     -> m ()
#ifdef __GL45
setDepthRenderbuffer fid w h = do
  renderbuffer <- createRenderbuffer w h (Proxy :: Proxy Depth32F)
  debugGL $ glNamedFramebufferRenderbuffer fid (fromAttachment DepthAttachment) GL_RENDERBUFFER
    (renderbufferID renderbuffer)
#elif defined(__GL33)
setDepthRenderbuffer _ w h = do
  renderbuffer <- createRenderbuffer w h (Proxy :: Proxy Depth32F)
  debugGL $ glFramebufferRenderbuffer GL_FRAMEBUFFER (fromAttachment DepthAttachment) GL_RENDERBUFFER
    (renderbufferID renderbuffer)
#endif

--------------------------------------------------------------------------------
-- Framebuffer color read/write configuration ----------------------------------

-- |Typeclass used to implement read/write operation per color attachment.
class FramebufferColorRW rw where
  setFramebufferColorRW :: (MonadIO m) => GLuint -> Natural -> proxy rw -> m ()

instance FramebufferColorRW W where
#ifdef __GL45
  setFramebufferColorRW fid nb _ = liftIO $ do
    withArrayLen (colorAttachmentsFromMax nb) $ \n buffers ->
      debugGL $ glNamedFramebufferDrawBuffers fid (fromIntegral n) buffers
#elif defined(__GL33)
  setFramebufferColorRW _ nb _ = liftIO $ do
    withArrayLen (colorAttachmentsFromMax nb) $ \n buffers ->
      debugGL $ glDrawBuffers (fromIntegral n) buffers
#endif

instance FramebufferColorRW RW where
#ifdef __GL45
  setFramebufferColorRW fid nb _ = liftIO $ do
    withArrayLen (colorAttachmentsFromMax nb) $ \n buffers ->
      debugGL $ glNamedFramebufferDrawBuffers fid (fromIntegral n) buffers
#elif defined(__GL33)
  setFramebufferColorRW _ nb _ = liftIO $ do
    withArrayLen (colorAttachmentsFromMax nb) $ \n buffers ->
      debugGL $ glDrawBuffers (fromIntegral n) buffers
#endif

--------------------------------------------------------------------------------
-- Framebuffer read/write target configuration ---------------------------------

-- |Framebuffer representation of 'R', 'W' and 'RW'.
class FramebufferTarget rw where
  framebufferTarget :: proxy rw -> GLenum

instance FramebufferTarget R where
  framebufferTarget _ = GL_READ_FRAMEBUFFER

instance FramebufferTarget W where
  framebufferTarget _ = GL_DRAW_FRAMEBUFFER

instance FramebufferTarget RW where
  framebufferTarget _ = GL_FRAMEBUFFER

--------------------------------------------------------------------------------
-- Framebuffer outputs ---------------------------------------------------------

-- Create a new texture and link it to the framebuffer.
addOutput :: forall m p proxy. (MonadIO m,MonadResource m,Pixel p)
          => GLuint
          -> Attachment
          -> Natural
          -> Natural
          -> Natural
          -> proxy p
          -> m (Texture2D p)
#ifdef __GL45
addOutput fid ca w h mipmaps _ = do
  tex :: Texture2D p <- createTexture (w,h) mipmaps defaultSampling
  debugGL . liftIO $ glNamedFramebufferTexture fid (fromAttachment ca)
    (baseTextureID $ texture2DBase tex) 0
  pure tex
#elif defined(__GL33)
addOutput _ ca w h mipmaps _ = do
  tex :: Texture2D p <- createTexture (w,h) mipmaps defaultSampling
  debugGL . liftIO $ glFramebufferTexture GL_FRAMEBUFFER (fromAttachment ca)
    (baseTextureID $ texture2DBase tex) 0
  pure tex
#endif

--------------------------------------------------------------------------------
-- Framebuffer textures accessors ----------------------------------------------

-- |Given a 'Format', inject it into a 'Texture2D' to handle it.
type family TexturizeFormat a :: * where
  TexturizeFormat ()           = ()
  TexturizeFormat (Format t c) = Texture2D (Format t c)
  TexturizeFormat (a :. b)     = TexturizeFormat a :. TexturizeFormat b

-- |Framebuffer output.
data Output c d = Output (TexturizeFormat c) (TexturizeFormat d)

--------------------------------------------------------------------------------
-- Framebuffer blitting --------------------------------------------------------

-- |Mask for framebuffer blit operation.
data FramebufferBlitMask
  = BlitColor
  | BlitDepth
  | BlitBoth
    deriving (Eq,Show)

fromFramebufferBlitMask :: FramebufferBlitMask -> GLbitfield
fromFramebufferBlitMask mask = case mask of
  BlitColor -> GL_COLOR_BUFFER_BIT
  BlitDepth -> GL_DEPTH_BUFFER_BIT
  BlitBoth  -> GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

-- Blit two framebuffers.
framebufferBlit :: (MonadIO m,Readable r,Writable w)
                => Framebuffer r c d
                -> Framebuffer w c' d'
                -> Int
                -> Int
                -> Natural
                -> Natural
                -> Int
                -> Int
                -> Natural
                -> Natural
                -> FramebufferBlitMask
                -> Filter
                -> m ()
#ifdef __GL45
framebufferBlit src dst srcX srcY srcW srcH dstX dstY dstW dstH mask flt = liftIO . debugGL $
    glBlitNamedFramebuffer (framebufferID src) (framebufferID dst) srcX0 srcY0 srcX1 srcY1 dstX0
      dstY0 dstX1 dstY1 (fromFramebufferBlitMask mask) (fromFilter flt)
#elif defined(__GL33)
framebufferBlit src dst srcX srcY srcW srcH dstX dstY dstW dstH mask flt =
    liftIO . debugGL $ do
      glBindFramebuffer GL_READ_FRAMEBUFFER (framebufferID src)
      glBindFramebuffer GL_DRAW_FRAMEBUFFER (framebufferID dst)
      glBlitFramebuffer srcX0 srcY0 srcX1 srcY1 dstX0 dstY0 dstX1 dstY1
        (fromFramebufferBlitMask mask) (fromFilter flt)
#endif
  where
    srcX0 = fromIntegral srcX
    srcY0 = fromIntegral srcY
    srcX1 = srcX0 + fromIntegral srcW
    srcY1 = srcY0 + fromIntegral srcH
    dstX0 = fromIntegral dstX
    dstY0 = fromIntegral dstY
    dstX1 = dstX0 + fromIntegral dstW
    dstY1 = dstY0 + fromIntegral dstH

--------------------------------------------------------------------------------
-- Special framebuffers --------------------------------------------------------

-- |The default 'Framebuffer' represents the screen (back buffer with double buffering).
defaultFramebuffer :: Framebuffer RW () ()
defaultFramebuffer = Framebuffer 0 (Output () ())

---------------------------------------------------------------------------------
-- Framebuffer errors -----------------------------------------------------------

newtype FramebufferError = IncompleteFramebuffer String deriving (Eq,Show)

class HasFramebufferError a where
  fromFramebufferError :: FramebufferError -> a
