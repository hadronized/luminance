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

module Graphics.Luminance.Core.Cubemap where

import Data.Proxy ( Proxy(..) )
#ifdef __GL32
import Data.Foldable ( for_ )
#endif
import Data.Vector.Storable ( unsafeWith )
import Foreign.Ptr ( castPtr )
#ifdef __GL32
import Foreign.Ptr ( nullPtr )
#endif
import Graphics.Luminance.Core.Texture ( BaseTexture(..), Texture(..) )
import Graphics.Luminance.Core.Pixel ( Pixel(..) )
import Graphics.GL
import Numeric.Natural ( Natural )

-- |Face of a 'Cubemap'.
data CubeFace
  = PositiveX
  | NegativeX
  | PositiveY
  | NegativeY
  | PositiveZ
  | NegativeZ
    deriving (Eq,Show)

fromCubeFace :: CubeFace -> GLint
fromCubeFace f = case f of
  PositiveX -> GL_TEXTURE_CUBE_MAP_POSITIVE_X
  NegativeX -> GL_TEXTURE_CUBE_MAP_NEGATIVE_X
  PositiveY -> GL_TEXTURE_CUBE_MAP_POSITIVE_Y
  NegativeY -> GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
  PositiveZ -> GL_TEXTURE_CUBE_MAP_POSITIVE_Z
  NegativeZ -> GL_TEXTURE_CUBE_MAP_NEGATIVE_Z

-- |A cubemap.
data Cubemap f = Cubemap {
    cubemapBase :: BaseTexture
  , cubemapSize :: Natural
  } deriving (Eq,Show)

instance (Pixel f) => Texture (Cubemap f) where
  type TextureSize (Cubemap f) = Natural
  type TextureOffset (Cubemap f) = (Natural,Natural,CubeFace)
  fromBaseTexture = Cubemap
  toBaseTexture = cubemapBase
  textureTypeEnum _ = GL_TEXTURE_CUBE_MAP
  textureSize (Cubemap _ s) = s
#ifdef __GL45
  textureStorage _ tid levels s =
    glTextureStorage2D tid levels (pixelIFormat (Proxy :: Proxy f)) (fromIntegral s)
      (fromIntegral s)
#elif defined(__GL32)
  textureStorage _ tid levels s = do
      glBindTexture GL_TEXTURE_CUBE_MAP tid
      for_ [0..levels-1] $ \lvl -> do
        let divisor = 2 ^ lvl
        glTexImage2D GL_TEXTURE_CUBE_MAP lvl (fromIntegral $ pixelIFormat pf)
          (fromIntegral s `div` divisor) (fromIntegral s `div` divisor) 0 (pixelFormat pf)
          (pixelType pf) nullPtr
    where pf = Proxy :: Proxy f
#endif
#ifdef __GL45
  transferTexelsSub _ tid (x,y,f) s texels =
      unsafeWith texels $ glTextureSubImage3D tid 0 (fromIntegral x) (fromIntegral y)
        (fromCubeFace f) (fromIntegral s) (fromIntegral s) 1 fmt
        typ . castPtr
#elif defined(__GL32)
  transferTexelsSub _ tid (x,y,f) s texels = do
      glBindTexture GL_TEXTURE_CUBE_MAP tid
      unsafeWith texels $ glTexSubImage3D GL_TEXTURE_CUBE_MAP 0 (fromIntegral x) (fromIntegral y)
        (fromCubeFace f) (fromIntegral s) (fromIntegral s) 1 fmt
        typ . castPtr
#endif
    where
      proxy = Proxy :: Proxy f
      fmt = pixelFormat proxy
      typ = pixelType proxy
#ifdef __GL45
  fillTextureSub _ tid (x,y,f) s filling =
      unsafeWith filling $ glClearTexSubImage tid 0 (fromIntegral x) (fromIntegral y) 
        (fromCubeFace f) (fromIntegral s) (fromIntegral s) 1
        fmt typ . castPtr
    where
      proxy = Proxy :: Proxy f
      fmt = pixelFormat proxy
      typ = pixelType proxy
#elif defined(__GL32)
  fillTextureSub p tid o w filling = transferTexelsSub p tid o w filling
#endif
