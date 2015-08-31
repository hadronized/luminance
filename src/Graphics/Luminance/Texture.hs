-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Texture where

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
import Graphics.Luminance.Pixel
import Numeric.Natural ( Natural )

data Wrap
  = ClampToEdge
  | ClampToBorder
  | Repeat
    deriving (Eq,Show)

fromWrap :: (Eq a,Num a) => Wrap -> a
fromWrap w = case w of
  ClampToEdge   -> GL_CLAMP_TO_EDGE
  ClampToBorder -> GL_CLAMP_TO_BORDER
  Repeat        -> GL_REPEAT

data Filter
  = Nearest
  | Linear
    deriving (Eq,Show)

fromFilter :: (Eq a,Num a) => Filter -> a
fromFilter f = case f of
  Nearest -> GL_NEAREST
  Linear  -> GL_LINEAR

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
  Never -> GL_NEVER
  Less -> GL_LESS
  Equal -> GL_EQUAL
  LessOrEqual -> GL_LEQUAL
  Greater -> GL_GREATER
  GreaterOrEqual -> GL_GEQUAL
  NotEqual -> GL_NOTEQUAL
  Always -> GL_ALWAYS

-- |2D Texture.
data Texture2D f = Texture2D {
    textureID     :: GLuint
  , textureHandle :: GLuint64
  , textureW      :: GLsizei
  , textureH      :: GLsizei
  , textureFormat :: GLenum
  , textureType   :: GLenum
  } deriving (Eq,Show)

createTexture :: forall p m. (Pixel p,MonadIO m,MonadResource m)
              => Natural
              -> Natural
              -> Natural
              -> Sampling
              -> m (Texture2D p)
createTexture w h mipmaps sampling = do
    (tid,texH) <- liftIO . alloca $ \p -> do
      glCreateTextures GL_TEXTURE_2D 1 p
      tid <- peek p
      glTextureStorage2D tid (fromIntegral mipmaps) ift w' h'
      glTextureParameteri tid GL_TEXTURE_BASE_LEVEL 0
      glTextureParameteri tid GL_TEXTURE_MAX_LEVEL (fromIntegral mipmaps - 1)
      setTextureSampling tid sampling
      texH <- glGetTextureHandleARB tid 
      glMakeTextureHandleResidentARB texH
      pure (tid,texH)
    _ <- register $ do
      glMakeTextureHandleNonResidentARB texH
      with tid $ glDeleteTextures 1
    pure $ Texture2D tid texH w' h' ft typ
  where
    ft  = pixelFormat (Proxy :: Proxy p)
    ift = pixelIFormat (undefined :: Proxy p)
    typ = pixelType (Proxy :: Proxy p)
    w'  = fromIntegral w
    h'  = fromIntegral h

newtype Sampler = Sampler { samplerID :: GLuint } deriving (Eq,Show)

data Sampling = Sampling {
    samplingWrapS           :: Wrap
  , samplingWrapT           :: Wrap
  , samplingWrapR           :: Wrap
  , samplingMinFilter       :: Filter
  , samplingMagFilter       :: Filter
  , samplingCompareFunction :: Maybe CompareFunc
  } deriving (Eq,Show)

defaultSampling :: Sampling
defaultSampling = Sampling {
    samplingWrapS           = ClampToEdge
  , samplingWrapT           = ClampToEdge
  , samplingWrapR           = ClampToEdge
  , samplingMinFilter       = Linear
  , samplingMagFilter       = Linear
  , samplingCompareFunction = Just LessOrEqual
  }

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

uploadWhole :: (Foldable f,MonadIO m,PixelBase p ~ a,Storable a)
            => Texture2D p
            -> Bool
            -> f a
            -> m ()
uploadWhole (Texture2D tid _ w h fmt typ) autolvl dat =
  liftIO $ do
    withArray (toList dat) $ glTextureSubImage2D tid 0 0 0 w h fmt typ . castPtr
    when autolvl $ glGenerateTextureMipmap tid

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

fillWhole :: (MonadIO m,PixelBase p ~ a,Storable a)
          => Texture2D p
          -> Bool
          -> a
          -> m ()
fillWhole (Texture2D tid _ _ _ fmt typ) autolvl filling =
  liftIO $ do
    with filling $ glClearTexImage tid 0 fmt typ . castPtr
    when autolvl $ glGenerateTextureMipmap tid

fillSub :: (MonadIO m,PixelBase p ~ a,Storable a)
        => Texture2D p
        -> Int
        -> Int
        -> Natural
        -> Natural
        -> Bool
        -> a
        -> m ()
fillSub (Texture2D tid _ _ _ fmt typ) x y w h autolvl filling =
  liftIO $ do
    with filling $ glClearTexSubImage tid 0 (fromIntegral x)
      (fromIntegral y) 0 (fromIntegral w) (fromIntegral h) 0 fmt typ . castPtr
    when autolvl $ glGenerateTextureMipmap tid
