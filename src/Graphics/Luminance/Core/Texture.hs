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
import Data.Foldable ( toList )
import Data.Proxy ( Proxy(..) )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( withArray )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( castPtr )
import Foreign.Storable ( Storable(peek) )
import Graphics.GL
import Graphics.GL.Ext.ARB.BindlessTexture
import Graphics.Luminance.Core.Pixel
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
    deriving (Eq,Show)

fromWrap :: (Eq a,Num a) => Wrap -> a
fromWrap w = case w of
  ClampToEdge   -> GL_CLAMP_TO_EDGE
  -- ClampToBorder -> GL_CLAMP_TO_BORDER
  Repeat        -> GL_REPEAT

-- |Sampling filter. 'Nearest' will sample the nearest texel at the sampling coordinates whilst
-- 'Linear' will perform linear interpolation with the texels nearby.
data Filter
  = Nearest
  | Linear
    deriving (Eq,Show)

fromFilter :: (Eq a,Num a) => Filter -> a
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

fromCompareFunc :: (Eq a,Num a) => CompareFunc -> a
fromCompareFunc f = case f of
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

class Texture t where
  type TextureSize t :: *
  fromBaseTexture :: BaseTexture -> TextureSize t -> t
  toBaseTexture :: t -> BaseTexture
  textureTypeEnum :: proxy t -> GLenum
  textureStorage :: proxy t
                 -> GLuint -- texture ID
                 -> GLint -- levels
                 -> TextureSize t -- size of the texture
                 -> IO ()
  transferTexelsSub :: forall a f proxy. (Foldable f,Storable a)
                    => proxy t
                    -> GLuint -- texture ID
                    -> TextureSize t -- offset
                    -> TextureSize t -- size
                    -> f a
                    -> IO ()
  fillTextureSub :: forall a f proxy. (Foldable f,Storable a)
                 => proxy t
                 -> GLuint
                 -> TextureSize t -- offset
                 -> TextureSize t -- size
                 -> f a
                 -> IO ()

-- OpenGL texture.
data BaseTexture = BaseTexture {
    baseTextureID  :: GLuint
  , baseTextureHnd :: GLuint64
  } deriving (Eq,Show)

-- |A 2D texture.
data Texture2D f = Texture2D {
    texture2DBase :: BaseTexture
  , textureW      :: Natural
  , textureH      :: Natural
  } deriving (Eq,Show)

instance (Pixel f) => Texture (Texture2D f) where
  type TextureSize (Texture2D f) = (Natural,Natural)
  fromBaseTexture bt (w,h) = Texture2D bt w h
  toBaseTexture = texture2DBase
  textureTypeEnum _ = GL_TEXTURE_2D
  textureStorage _ tid levels (w,h) = do
    glTextureStorage2D tid levels (pixelIFormat (Proxy :: Proxy f)) (fromIntegral w) (fromIntegral h)
  transferTexelsSub _ tid (x,y) (w,h) texels = do
      withArray (toList texels) $ glTextureSubImage2D tid 0 (fromIntegral x) (fromIntegral y)
        (fromIntegral w) (fromIntegral h) fmt typ . castPtr
    where
      proxy = Proxy :: Proxy f
      fmt = pixelFormat proxy
      typ = pixelType proxy
  fillTextureSub _ tid (x,y) (w,h) filling = do
      withArray (toList filling) $ glClearTexSubImage tid 0 (fromIntegral x)
        (fromIntegral y) 0 (fromIntegral w) (fromIntegral h) 1 fmt typ . castPtr
    where
      proxy = Proxy :: Proxy f
      fmt = pixelFormat proxy
      typ = pixelType proxy

-- |'createTexture w h levels sampling' a new 'w'*'h' texture with 'levels' levels. The format is
-- set through the type.
createTexture :: forall m t. (MonadIO m,MonadResource m,Texture t)
              => TextureSize t
              -> Natural
              -> Sampling
              -> m t
createTexture size levels sampling = do
    (tid,texH) <- liftIO . alloca $ \p -> do
      glCreateTextures (textureTypeEnum (Proxy :: Proxy t)) 1 p
      tid <- peek p
      textureStorage (Proxy :: Proxy t) tid (fromIntegral levels) size
      glTextureParameteri tid GL_TEXTURE_BASE_LEVEL 0
      glTextureParameteri tid GL_TEXTURE_MAX_LEVEL (fromIntegral levels - 1)
      setTextureSampling tid sampling
      texH <- glGetTextureHandleARB tid 
      glMakeTextureHandleResidentARB texH
      pure (tid,texH)
    _ <- register $ do
      glMakeTextureHandleNonResidentARB texH
      with tid $ glDeleteTextures 1
    pure $ fromBaseTexture (BaseTexture tid texH) size

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

-- |Default 'Samplinq' for convenience.
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
setSampling :: (Eq a,Eq b,MonadIO m,Num a,Num b) => (GLenum -> a -> b -> IO ()) -> GLenum -> Sampling -> m ()
setSampling f objID s = liftIO $ do
  -- wraps
  f objID GL_TEXTURE_WRAP_S . fromWrap $ samplingWrapS s
  f objID GL_TEXTURE_WRAP_T . fromWrap $ samplingWrapT s
  f objID GL_TEXTURE_WRAP_R . fromWrap $ samplingWrapR s
  -- filters
  f objID GL_TEXTURE_MIN_FILTER . fromFilter $ samplingMinFilter s
  f objID GL_TEXTURE_MAG_FILTER . fromFilter $ samplingMagFilter s
  -- comparison function
  case samplingCompareFunction s of
    Just cmpf -> do
      f objID GL_TEXTURE_COMPARE_FUNC $ fromCompareFunc cmpf
      f objID GL_TEXTURE_COMPARE_MODE GL_COMPARE_REF_TO_TEXTURE
    Nothing ->
      f objID GL_TEXTURE_COMPARE_MODE GL_NONE

setTextureSampling :: (MonadIO m) => GLenum -> Sampling -> m ()
setTextureSampling = setSampling glTextureParameteri

setSamplerSampling :: (MonadIO m) => GLenum -> Sampling -> m ()
setSamplerSampling = setSampling glSamplerParameteri

----------------------------------------------------------------------------------------------------
-- Samplers ----------------------------------------------------------------------------------------

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

----------------------------------------------------------------------------------------------------
-- Texture operations ------------------------------------------------------------------------------

{-
-- |Upload data to the whole texture’s storage. The 'Bool' can be used to automatically generate
-- mipmaps.
uploadWhole :: (Foldable f,MonadIO m,PixelBase p ~ a,Storable a)
            => Texture2D p
            -> Bool
            -> f a
            -> m ()
uploadWhole (Texture2D tid _ w h fmt typ) autolvl dat =
  liftIO $ do
    withArray (toList dat) $ glTextureSubImage2D tid 0 0 0 w h fmt typ . castPtr
    when autolvl $ glGenerateTextureMipmap tid

-- |@'uploadSub' tex x y w h autolvl texels@ uploads data to a subpart of the texture’s storage.
-- @x@ and @y@ are offset with origin at upper-left corner, and @w@ and @h@ are the size of the area
-- to upload to. @autolvl@ is a 'Bool' that can be used to automatically generate mipmaps.
uploadSub :: (Foldable f,MonadIO m,PixelBase p ~ a,Storable a)
          => Texture2D p
          -> Int
          -> Int
          -> Natural
          -> Natural
          -> Bool
          -> f a
          -> m ()
uploadSub (Texture2D tid _ _ _ fmt typ) x y w h autolvl dat =
  liftIO $ do
    withArray (toList dat) $ glTextureSubImage2D tid 0 (fromIntegral x)
      (fromIntegral y) (fromIntegral w) (fromIntegral h) fmt typ . castPtr
    when autolvl $ glGenerateTextureMipmap tid

-- |Fill the whole texture’s storage with a given value.
fillWhole :: (Foldable f, MonadIO m,PixelBase p ~ a,Storable a)
          => Texture2D p
          -> Bool
          -> f a
          -> m ()
fillWhole tex = fillSub tex 0 0 (fromIntegral $ textureW tex) (fromIntegral $ textureH tex)

-- |Fill a subpart of the texture’s storage with a given value.
fillSub :: (Foldable f,MonadIO m,PixelBase p ~ a,Storable a)
        => Texture2D p
        -> Int
        -> Int
        -> Natural
        -> Natural
        -> Bool
        -> f a
        -> m ()
fillSub (Texture2D tid _ _ _ fmt typ) x y w h autolvl filling =
  liftIO $ do
    withArray (toList filling) $ glClearTexSubImage tid 0 (fromIntegral x)
      (fromIntegral y) 0 (fromIntegral w) (fromIntegral h) 1 fmt typ . castPtr
    when autolvl $ glGenerateTextureMipmap tid
-}
