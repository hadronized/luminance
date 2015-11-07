{-# LANGUAGE CPP #-}
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

module Graphics.Luminance.Core.Texture2D where

import Data.Foldable ( for_ )
import Data.Proxy ( Proxy(..) )
import Data.Vector.Storable ( unsafeWith )
import Foreign.Ptr ( castPtr, nullPtr )
import Graphics.Luminance.Core.Texture ( BaseTexture(..), Texture(..) )
import Graphics.Luminance.Core.Pixel ( Pixel(..) )
import Graphics.GL
import Graphics.Luminance.Core.Debug
import Numeric.Natural ( Natural )

-- |A 2D texture.
data Texture2D f = Texture2D {
    texture2DBase :: BaseTexture
  , texture2DW    :: Natural
  , texture2DH    :: Natural
  } deriving (Eq,Show)

instance (Pixel f) => Texture (Texture2D f) where
  type TextureSize (Texture2D f) = (Natural,Natural)
  type TextureOffset (Texture2D f) = (Natural,Natural)
  fromBaseTexture bt (w,h) = Texture2D bt w h
  toBaseTexture = texture2DBase
  textureTypeEnum _ = GL_TEXTURE_2D
  textureSize (Texture2D _ w h) = (w,h)
#if defined(__GL45)
  textureStorage _ tid levels (w,h) =
    debugGL $ glTextureStorage2D tid levels (pixelIFormat (Proxy :: Proxy f)) (fromIntegral w) (fromIntegral h)
#elif defined(__GL32)
  textureStorage _ _ levels (w,h) = do
      for_ [0..levels-1] $ \lvl -> do
        let divisor = 2 ^ lvl
        debugGL $ glTexImage2D GL_TEXTURE_2D lvl (fromIntegral $ pixelIFormat pf)
          (fromIntegral w `div` divisor) (fromIntegral h `div` divisor) 0 (pixelFormat pf)
          (pixelType pf) nullPtr
    where pf = Proxy :: Proxy f
#endif
#if defined(__GL45)
  transferTexelsSub _ tid (x,y) (w,h) texels =
      debugGL . unsafeWith texels $ glTextureSubImage2D tid 0 (fromIntegral x) (fromIntegral y)
        (fromIntegral w) (fromIntegral h) fmt typ . castPtr
#elif defined(__GL32)
  transferTexelsSub _ tid (x,y) (w,h) texels = do
      debugGL $ glBindTexture GL_TEXTURE_2D tid
      debugGL . unsafeWith texels $ glTexSubImage2D GL_TEXTURE_2D 0 (fromIntegral x) (fromIntegral y)
        (fromIntegral w) (fromIntegral h) fmt typ . castPtr
#endif
    where
      proxy = Proxy :: Proxy f
      fmt = pixelFormat proxy
      typ = pixelType proxy
#if defined(__GL45)
  fillTextureSub _ tid (x,y) (w,h) filling =
      debugGL . unsafeWith filling $ glClearTexSubImage tid 0 (fromIntegral x)
        (fromIntegral y) 0 (fromIntegral w) (fromIntegral h) 1 fmt typ . castPtr
    where
      proxy = Proxy :: Proxy f
      fmt = pixelFormat proxy
      typ = pixelType proxy
#elif defined(__GL32)
  fillTextureSub p tid o w filling = transferTexelsSub p tid o w filling
#endif
