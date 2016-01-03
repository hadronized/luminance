{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Core.Texture where

import Control.Monad ( when )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Resource ( MonadResource, register )
import Data.Proxy ( Proxy(..) )
import Data.Vector.Storable ( Vector )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Utils ( with )
import Foreign.Storable ( Storable(peek) )
import Graphics.GL
#if defined(__GL_BINDLESS_TEXTURES) && defined(VERSION_gl)
import Graphics.GL.Ext.ARB.BindlessTexture
#endif
import Graphics.Luminance.Core.Debug
import Numeric.Natural ( Natural )

----------------------------------------------------------------------------------------------------
-- Texture parameters ------------------------------------------------------------------------------

-- |Wrap texture parameter. Such an object is used to tell how to sampling is performed when going
-- out of the texture coordinates.
--
-- 'ClampToEdge' will clamp the texture coordinates between in '[0,1]'. If you pass '1.1' or
-- '31.456', in both cases you’ll end up with '1'. Same thing for negative values clamped to '0'.
--
-- 'Repeat' will clamp the texture in '[0,1]' after applying a 'fract' on the value, yielding a
-- a repeated '[0,1]' pattern.
data Wrap
  = ClampToEdge
  -- | ClampToBorder
  | Repeat
  | MirroredRepeat
    deriving (Eq,Show)

fromWrap :: Wrap -> GLint
fromWrap w = fromIntegral $ case w of
  ClampToEdge    -> GL_CLAMP_TO_EDGE
  -- ClampToBorder  -> GL_CLAMP_TO_BORDER
  Repeat         -> GL_REPEAT
  MirroredRepeat -> GL_MIRRORED_REPEAT

-- |Sampling filter. 'Nearest' will sample the nearest texel at the sampling coordinates whilst
-- 'Linear' will perform linear interpolation with the texels nearby.
data Filter
  = Nearest
  | Linear
    deriving (Eq,Show)

fromFilter :: Filter -> GLenum
fromFilter f = case f of
  Nearest -> GL_NEAREST
  Linear  -> GL_LINEAR

-- |For textures that might require depth comparison, that type defines all the possible cases for
-- comparison.
data CompareFunc
  = Never
  | Less
  | Equal
  | LessOrEqual
  | Greater
  | GreaterOrEqual
  | NotEqual
  | Always
    deriving (Eq,Show)

fromCompareFunc :: CompareFunc -> GLint
fromCompareFunc f = fromIntegral $ case f of
  Never          -> GL_NEVER
  Less           -> GL_LESS
  Equal          -> GL_EQUAL
  LessOrEqual    -> GL_LEQUAL
  Greater        -> GL_GREATER
  GreaterOrEqual -> GL_GEQUAL
  NotEqual       -> GL_NOTEQUAL
  Always         -> GL_ALWAYS

----------------------------------------------------------------------------------------------------
-- Textures ----------------------------------------------------------------------------------------

-- |Class of all textures.
class Texture t where
  -- |Size of a texture. This is an associated type – /type family/ – because the dimensionality of
  -- a texture relies on its type.
  type TextureSize t :: *
  -- |In order to index regions of texels in texture, we need another associated type – for the same
  -- dimensionality reason as for 'TextureSize'.
  type TextureOffset t :: *
  fromBaseTexture :: BaseTexture -> TextureSize t -> t
  toBaseTexture :: t -> BaseTexture
  textureTypeEnum :: proxy t -> GLenum
  textureSize :: t -> TextureSize t
  textureStorage :: proxy t
                 -> GLuint -- texture ID
                 -> GLint -- levels
                 -> TextureSize t -- size of the texture
                 -> IO ()
  transferTexelsSub :: (Storable a)
                    => proxy t
                    -> GLuint -- texture ID
                    -> TextureOffset t -- offset
                    -> TextureSize t -- size
                    -> Vector a
                    -> IO ()
  fillTextureSub :: (Storable a)
                 => proxy t
                 -> GLuint
                 -> TextureOffset t -- offset
                 -> TextureSize t -- size
                 -> Vector a
                 -> IO ()

-- OpenGL texture.
#if defined(__GL45) && defined(__GL_BINDLESS_TEXTURES)
data BaseTexture = BaseTexture {
    baseTextureID  :: GLuint
  , baseTextureHnd :: GLuint64
  } deriving (Eq,Show)
#elif defined(__GL33)
newtype BaseTexture = BaseTexture {
    baseTextureID  :: GLuint
  } deriving (Eq,Show)
#endif

-- |'createTexture w h levels sampling' a new 'w'*'h' texture with 'levels' levels. The format is
-- set through the type.
createTexture :: forall m t. (MonadIO m,MonadResource m,Texture t)
              => TextureSize t
              -> Natural
              -> Sampling
              -> m t
#if defined(__GL45) && defined(__GL_BINDLESS_TEXTURES)
createTexture size levels sampling = do
  (tid,texH) <- liftIO . alloca $ \p -> do
    debugGL $ glCreateTextures (textureTypeEnum (Proxy :: Proxy t)) 1 p
    tid <- peek p
    textureStorage (Proxy :: Proxy t) tid (fromIntegral levels) size
    debugGL $ glTextureParameteri tid GL_TEXTURE_BASE_LEVEL 0
    debugGL $ glTextureParameteri tid GL_TEXTURE_MAX_LEVEL (fromIntegral levels - 1)
    setTextureSampling tid sampling
    texH <- glGetTextureHandleARB tid 
    debugGL $ glMakeTextureHandleResidentARB texH
    pure (tid,texH)
  _ <- register $ do
    debugGL $ glMakeTextureHandleNonResidentARB texH
    with tid (glDeleteTextures 1)
  pure $ fromBaseTexture (BaseTexture tid texH) size
#elif defined(__GL33)
createTexture size levels sampling = do
    tid <- liftIO . alloca $ \p -> do
      debugGL $ glGenTextures 1 p
      tid <- peek p
      debugGL $ glBindTexture target tid
      debugGL $ glTexParameteri target GL_TEXTURE_BASE_LEVEL 0
      debugGL $ glTexParameteri target GL_TEXTURE_MAX_LEVEL (fromIntegral levels - 1)
      setTextureSampling target sampling
      textureStorage (Proxy :: Proxy t) tid (fromIntegral levels) size
      pure tid
    _ <- register $ with tid (glDeleteTextures 1)
    pure $ fromBaseTexture (BaseTexture tid) size
  where
    target = textureTypeEnum (Proxy :: Proxy t)
#endif

----------------------------------------------------------------------------------------------------
-- Sampling objects --------------------------------------------------------------------------------

-- |A sampling configuration type.
data Sampling = Sampling {
    samplingWrapS           :: Wrap
  , samplingWrapT           :: Wrap
  , samplingWrapR           :: Wrap
  , samplingMinFilter       :: Filter
  , samplingMagFilter       :: Filter
  , samplingCompareFunction :: Maybe CompareFunc
  } deriving (Eq,Show)

-- |Default 'Sampling' for convenience.
--
-- @
--   defaultSampling = Sampling {
--       samplingWrapS           = ClampToEdge
--     , samplingWrapT           = ClampToEdge
--     , samplingWrapR           = ClampToEdge
--     , samplingMinFilter       = Linear
--     , samplingMagFilter       = Linear
--     , samplingCompareFunction = Nothing
--     }
-- @
defaultSampling :: Sampling
defaultSampling = Sampling {
    samplingWrapS           = ClampToEdge
  , samplingWrapT           = ClampToEdge
  , samplingWrapR           = ClampToEdge
  , samplingMinFilter       = Linear
  , samplingMagFilter       = Linear
  , samplingCompareFunction = Nothing
  }

-- Apply a 'Sampling' object for a given type of object (texture, sampler, etc.).
setSampling :: (MonadIO m) => (GLenum -> GLenum -> GLint -> IO ()) -> GLenum -> Sampling -> m ()
setSampling f oid s = liftIO $ do
  -- wraps
  debugGL $ f oid GL_TEXTURE_WRAP_S . fromWrap $ samplingWrapS s
  debugGL $ f oid GL_TEXTURE_WRAP_T . fromWrap $ samplingWrapT s
  debugGL $ f oid GL_TEXTURE_WRAP_R . fromWrap $ samplingWrapR s
  -- filters
  debugGL $ f oid GL_TEXTURE_MIN_FILTER . fromIntegral . fromFilter $ samplingMinFilter s
  debugGL $ f oid GL_TEXTURE_MAG_FILTER . fromIntegral . fromFilter $ samplingMagFilter s
  -- comparison function
  case samplingCompareFunction s of
    Just cmpf -> do
      debugGL $ f oid GL_TEXTURE_COMPARE_FUNC $ fromCompareFunc cmpf
      debugGL $ f oid GL_TEXTURE_COMPARE_MODE (fromIntegral GL_COMPARE_REF_TO_TEXTURE)
    Nothing ->
      debugGL $ f oid GL_TEXTURE_COMPARE_MODE (fromIntegral GL_NONE)

setTextureSampling :: (MonadIO m) => GLenum -> Sampling -> m ()
#ifdef __GL45
setTextureSampling = setSampling glTextureParameteri
#elif defined(__GL33)
setTextureSampling = setSampling glTexParameteri
#endif

setSamplerSampling :: (MonadIO m) => GLenum -> Sampling -> m ()
setSamplerSampling = setSampling glSamplerParameteri

----------------------------------------------------------------------------------------------------
-- Samplers ----------------------------------------------------------------------------------------

{-
newtype Sampler = Sampler { samplerID :: GLuint } deriving (Eq,Show)

createSampler :: (MonadIO m,MonadResource m)
              => Sampling
              -> m Sampler
createSampler s = do
  sid <- liftIO . alloca $ \p -> do
    glCreateSamplers 1 p
    sid <- peek p
    setSamplerSampling sid s
    pure sid
  _ <- register . with sid $ glDeleteSamplers 1
  pure $ Sampler sid
-}

----------------------------------------------------------------------------------------------------
-- Texture operations ------------------------------------------------------------------------------

-- |@'uploadSub' tex offset size autolvl texels@ uploads data to a subpart of the texture’s storage.
-- The offset is given with origin at upper-left corner, and @size@ is the size of the area
-- to upload to. @autolvl@ is a 'Bool' that can be used to automatically generate mipmaps.
uploadSub :: forall a m t. (MonadIO m,Storable a,Texture t)
          => t
          -> TextureOffset t
          -> TextureSize t
          -> Bool
          -> Vector a
          -> m ()
uploadSub tex offset size autolvl texels = liftIO $ do
    transferTexelsSub (Proxy :: Proxy t) tid offset size texels
#ifdef __GL45
    debugGL . when autolvl $ glGenerateTextureMipmap tid
#elif defined(__GL33)
    debugGL . when autolvl $ glGenerateMipmap (textureTypeEnum (Proxy :: Proxy t))
#endif
  where
    tid = baseTextureID (toBaseTexture tex)

-- |Fill a subpart of the texture’s storage with a given value.
fillSub :: forall a m t. (MonadIO m,Storable a,Texture t)
        => t
        -> TextureOffset t
        -> TextureSize t
        -> Bool
        -> Vector a
        -> m ()
fillSub tex offset size autolvl filling = liftIO $ do
    fillTextureSub (Proxy :: Proxy t) tid offset size filling
#ifdef __GL45
    debugGL . when autolvl $ glGenerateTextureMipmap tid
#elif defined(__GL33)
    debugGL . when autolvl $ glGenerateMipmap (textureTypeEnum (Proxy :: Proxy t))
#endif
  where
    tid = baseTextureID (toBaseTexture tex)
