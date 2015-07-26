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

import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Resource ( MonadResource, register )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Utils ( with )
import Foreign.Storable ( peek )
import Graphics.GL
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

newtype Unit = Unit Natural deriving (Enum,Eq,Integral,Num,Ord,Real,Show)

fromUnit :: (Eq a,Num a) => Unit -> a
fromUnit (Unit i) = GL_TEXTURE0 + fromIntegral i

-- |2D Texture.
newtype Texture f = Texture2D { textureID :: GLuint } deriving (Eq,Show)

{-
createTexture :: (MonadIO m,MonadResource m)
              => Word32
              -> Word32
              -> m (Texture f)
createTexture w h = do
-}

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
    glGenSamplers 1 p
    sid <- peek p
    setTextureSampling sid s
    pure sid
  _ <- register . with sid $ glDeleteSamplers 1
  pure $ Sampler sid

setSampling :: (Eq a,Eq b,MonadIO m,Num a,Num b) => (GLenum -> a -> b -> IO ()) -> GLenum -> Sampling -> m ()
setSampling f target s = liftIO $ do
  -- wraps
  f target GL_TEXTURE_WRAP_S . fromWrap $ samplingWrapS s
  f target GL_TEXTURE_WRAP_T . fromWrap $ samplingWrapT s
  f target GL_TEXTURE_WRAP_R . fromWrap $ samplingWrapR s
  -- filters
  f target GL_TEXTURE_MIN_FILTER . fromFilter $ samplingMinFilter s
  f target GL_TEXTURE_MAG_FILTER . fromFilter $ samplingMagFilter s
  -- comparison function
  case samplingCompareFunction s of
    Just cmpf -> do
      f target GL_TEXTURE_COMPARE_FUNC $ fromCompareFunc cmpf
      f target GL_TEXTURE_COMPARE_MODE GL_COMPARE_REF_TO_TEXTURE
    Nothing ->
      f target GL_TEXTURE_COMPARE_MODE GL_NONE

setTextureSampling :: (MonadIO m) => GLenum -> Sampling -> m ()
setTextureSampling = setSampling glTexParameteri

setSamplerSampling :: (MonadIO m) => GLenum -> Sampling -> m ()
setSamplerSampling = setSampling glSamplerParameteri
