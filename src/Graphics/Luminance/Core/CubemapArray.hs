{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
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

module Graphics.Luminance.Core.CubemapArray where

import Data.Proxy ( Proxy(..) )
import Data.Vector.Storable ( unsafeWith )
import Foreign.Ptr ( castPtr )
import GHC.TypeLits ( KnownNat, Nat, natVal )
import Graphics.Luminance.Core.Cubemap ( CubeFace, fromCubeFace )
import Graphics.Luminance.Core.Texture ( BaseTexture(..), Texture(..) )
import Graphics.Luminance.Core.Pixel ( Pixel(..) )
import Graphics.GL
import Numeric.Natural ( Natural )

-- |A cubemap array.
data CubemapArray (n :: Nat) (f :: *) = CubemapArray {
    cubemapArrayBase :: BaseTexture
  , cubemapArrayW    :: Natural
  , cubemapArrayH    :: Natural
  } deriving (Eq,Show)

instance (KnownNat n, Pixel f) => Texture (CubemapArray n f) where
  -- |(w,h)
  type TextureSize (CubemapArray n f) = (Natural,Natural)
  -- |(layer,x,y,face)
  type TextureOffset (CubemapArray n f) = (Natural,Natural,Natural,CubeFace)
  fromBaseTexture bt (w,h) = CubemapArray bt w h
  toBaseTexture = cubemapArrayBase
  textureTypeEnum _ = GL_TEXTURE_CUBE_MAP_ARRAY
  textureSize (CubemapArray _ w h) = (w,h)
  textureStorage _ tid levels (w,h) =
    glTextureStorage3D tid levels (pixelIFormat (Proxy :: Proxy f)) (fromIntegral w)
      (fromIntegral h) (fromIntegral $ natVal (Proxy :: Proxy n))
  transferTexelsSub _ tid (layer,x,y,f) (w,h) texels =
      unsafeWith texels $ glTextureSubImage3D tid 0 (fromIntegral x) (fromIntegral y)
        (fromCubeFace f + fromIntegral layer*6) (fromIntegral w) (fromIntegral h) 1 fmt
        typ . castPtr
    where
      proxy = Proxy :: Proxy f
      fmt = pixelFormat proxy
      typ = pixelType proxy
  fillTextureSub _ tid (layer,x,y,f) (w,h) filling =
      unsafeWith filling $ glClearTexSubImage tid 0 (fromIntegral x) (fromIntegral y) 
        (fromCubeFace f + fromIntegral layer*6) (fromIntegral w) (fromIntegral h) 1
        fmt typ . castPtr
    where
      proxy = Proxy :: Proxy f
      fmt = pixelFormat proxy
      typ = pixelType proxy
