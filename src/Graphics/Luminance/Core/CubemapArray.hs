{-# LANGUAGE CPP #-}
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

import Data.Foldable ( for_ )
import Data.Proxy ( Proxy(..) )
import Data.Vector.Storable as V ( concat, unsafeWith )
import Foreign.Ptr ( castPtr, nullPtr )
import GHC.TypeLits ( KnownNat, Nat, natVal )
import Graphics.Luminance.Core.Cubemap ( CubeFace, fromCubeFace )
import Graphics.Luminance.Core.Texture ( BaseTexture(..), Texture(..) )
import Graphics.Luminance.Core.Pixel ( Pixel(..) )
import Graphics.GL
import Numeric.Natural ( Natural )

-- |A cubemap array.
data CubemapArray (n :: Nat) (f :: *) = CubemapArray {
    cubemapArrayBase :: BaseTexture
  , cubemapArraySize :: Natural
  } deriving (Eq,Show)

instance (KnownNat n, Pixel f) => Texture (CubemapArray n f) where
  -- |(w,h)
  type TextureSize (CubemapArray n f) = Natural
  -- |(layer,x,y,face)
  type TextureOffset (CubemapArray n f) = (Natural,Natural,Natural,CubeFace)
  fromBaseTexture bt s = CubemapArray bt s
  toBaseTexture = cubemapArrayBase
  textureTypeEnum _ = GL_TEXTURE_CUBE_MAP_ARRAY
  textureSize (CubemapArray _ s) = s
#ifdef __GL45
  textureStorage _ tid levels s =
    glTextureStorage3D tid levels (pixelIFormat (Proxy :: Proxy f)) (fromIntegral s)
      (fromIntegral s) (fromIntegral $ natVal (Proxy :: Proxy n))
#elif defined(__GL32)
  textureStorage _ _ _ _ = pure ()
#endif
#ifdef __GL45
  transferTexelsSub _ tid (layer,x,y,f) s texels =
      unsafeWith texels $ glTextureSubImage3D tid 0 (fromIntegral x) (fromIntegral y)
        (fromCubeFace f + fromIntegral layer*6) (fromIntegral s) (fromIntegral s) 1 fmt
        typ . castPtr
    where
      proxy = Proxy :: Proxy f
      fmt = pixelFormat proxy
      typ = pixelType proxy
#elif defined(__GL32)
  transferTexelsSub _ _ _ _ _ = pure ()
#endif
#ifdef __GL45
  fillTextureSub _ tid (layer,x,y,f) s filling =
      unsafeWith filling $ glClearTexSubImage tid 0 (fromIntegral x) (fromIntegral y) 
        (fromCubeFace f + fromIntegral layer*6) (fromIntegral s) (fromIntegral s) 1
        fmt typ . castPtr
    where
      proxy = Proxy :: Proxy f
      fmt = pixelFormat proxy
      typ = pixelType proxy
#elif defined(__GL32)
  fillTextureSub _ _ _ _ _ = pure ()
#endif
