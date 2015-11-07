{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
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

module Graphics.Luminance.Core.Texture2DArray where

import Data.Foldable ( for_ )
import Data.Proxy ( Proxy(..) )
import Data.Vector.Storable ( unsafeWith )
import Foreign.Ptr ( castPtr, nullPtr )
import GHC.TypeLits ( KnownNat, Nat, natVal )
import Graphics.Luminance.Core.Texture ( BaseTexture(..), Texture(..) )
import Graphics.Luminance.Core.Pixel ( Pixel(..) )
import Graphics.GL
import Numeric.Natural ( Natural )

-- |A 2D texture array.
data Texture2DArray (n :: Nat) (f :: *) = Texture2DArray {
    texture2DArrayBase :: BaseTexture
  , texture2DArrayW    :: Natural
  , texture2DArrayH    :: Natural
  } deriving (Eq,Show)

instance (KnownNat n,Pixel f) => Texture (Texture2DArray n f) where
  -- |(w,h)
  type TextureSize (Texture2DArray n f) = (Natural,Natural)
  -- |(layer,x,y)
  type TextureOffset (Texture2DArray n f) = (Natural,Natural,Natural)
  fromBaseTexture bt (w,h) = Texture2DArray bt w h
  toBaseTexture = texture2DArrayBase
  textureTypeEnum _ = GL_TEXTURE_2D_ARRAY
  textureSize (Texture2DArray _ w h) = (w,h)
#ifdef __GL45
  textureStorage _ tid levels (w,h) =
    glTextureStorage3D tid levels (pixelIFormat (Proxy :: Proxy f)) (fromIntegral w)
      (fromIntegral h) (fromIntegral $ natVal (Proxy :: Proxy n))
#elif defined(__GL32)
  textureStorage _ tid levels (w,h) = do
      glBindTexture GL_TEXTURE_2D_ARRAY tid
      for_ [0..levels-1] $ \lvl -> glTexImage3D GL_TEXTURE_2D_ARRAY lvl
        (fromIntegral $ pixelIFormat pf) (fromIntegral w) (fromIntegral h)
        (fromIntegral $ natVal (Proxy :: Proxy n)) 0 (pixelFormat pf) (pixelType pf) nullPtr
    where pf = Proxy :: Proxy f
#endif
#ifdef __GL45
  transferTexelsSub _ tid (layer,x,y) (w,h) texels =
      unsafeWith texels $ glTextureSubImage3D tid 0 (fromIntegral x) (fromIntegral y)
        (fromIntegral layer) (fromIntegral w) (fromIntegral h) 1 fmt typ . castPtr
#elif defined(__GL32)
  transferTexelsSub _ tid (layer,x,y) (w,h) texels = do
      glBindTexture GL_TEXTURE_2D_ARRAY tid
      unsafeWith texels $ glTexSubImage3D GL_TEXTURE_2D_ARRAY 0 (fromIntegral x) (fromIntegral y)
        (fromIntegral layer) (fromIntegral w) (fromIntegral h) 1 fmt typ . castPtr
#endif
    where
      proxy = Proxy :: Proxy f
      fmt = pixelFormat proxy
      typ = pixelType proxy
#ifdef __GL45
  fillTextureSub _ tid (layer,x,y) (w,h) filling =
      unsafeWith filling $ glClearTexSubImage tid 0 (fromIntegral x)
        (fromIntegral y) (fromIntegral layer) (fromIntegral w) (fromIntegral h) (fromIntegral $ natVal (Proxy :: Proxy n)) fmt typ . castPtr
    where
      proxy = Proxy :: Proxy f
      fmt = pixelFormat proxy
      typ = pixelType proxy
#elif defined(__GL32)
  fillTextureSub p tid o w filling = transferTexelsSub p tid o w filling
#endif
