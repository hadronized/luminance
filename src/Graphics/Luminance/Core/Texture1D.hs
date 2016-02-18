{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015, 2016 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Core.Texture1D where

import Data.Foldable ( for_ )
import Data.Proxy ( Proxy(..) )
import Data.Vector.Storable ( unsafeWith )
import Foreign.Ptr ( castPtr, nullPtr )
import Graphics.Luminance.Core.Texture ( BaseTexture(..), Texture(..) )
import Graphics.Luminance.Core.Pixel ( Pixel(..) )
import Graphics.GL
import Numeric.Natural ( Natural )

-- |A 1D texture.
data Texture1D f = Texture1D {
    texture1DBase :: BaseTexture
  , texture1DW    :: Natural
  } deriving (Eq,Show)

instance (Pixel f) => Texture (Texture1D f) where
  type TextureSize (Texture1D f) = Natural
  type TextureOffset (Texture1D f) = Natural
  fromBaseTexture = Texture1D
  toBaseTexture = texture1DBase
  textureTypeEnum _ = GL_TEXTURE_1D
  textureSize = texture1DW
#ifdef __GL45
  textureStorage _ tid levels w =
    glTextureStorage1D tid levels (pixelIFormat (Proxy :: Proxy f)) (fromIntegral w)
#elif defined(__GL33)
  textureStorage _ _ levels w = do
      for_ [0..levels-1] $ \lvl -> do
        let divisor = 2 ^ lvl
        glTexImage1D GL_TEXTURE_1D lvl (fromIntegral $ pixelIFormat pf) (fromIntegral w `div` divisor) 0
          (pixelFormat pf) (pixelType pf) nullPtr
    where pf = Proxy :: Proxy f
#endif
#ifdef __GL45
  transferTexelsSub _ tid x w texels =
      unsafeWith texels $ glTextureSubImage1D tid 0 (fromIntegral x) (fromIntegral w) fmt
        typ . castPtr
#elif defined(__GL33)
  transferTexelsSub _ tid x w texels = do
      glBindTexture GL_TEXTURE_1D tid
      unsafeWith texels $ glTexSubImage1D GL_TEXTURE_1D 0 (fromIntegral x) (fromIntegral w) fmt
        typ . castPtr
#endif
    where
      proxy = Proxy :: Proxy f
      fmt = pixelFormat proxy
      typ = pixelType proxy
#ifdef __GL45
  fillTextureSub _ tid x w filling =
      unsafeWith filling $ glClearTexSubImage tid 0 (fromIntegral x) 0 0 (fromIntegral w) 1 1
        fmt typ . castPtr
    where
      proxy = Proxy :: Proxy f
      fmt = pixelFormat proxy
      typ = pixelType proxy
#elif defined(__GL33)
  fillTextureSub p tid x w filling = transferTexelsSub p tid x w filling
#endif
