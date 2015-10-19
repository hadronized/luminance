-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Core.Texture1DArray where

import Data.Proxy ( Proxy(..) )
import Data.Vector.Storable ( unsafeWith )
import Foreign.Ptr ( castPtr )
import GHC.TypeLits ( KnownNat, Nat, natVal )
import Graphics.Luminance.Core.Texture ( BaseTexture(..), Texture(..) )
import Graphics.Luminance.Core.Pixel ( Pixel(..) )
import Graphics.GL
import Numeric.Natural ( Natural )

-- |A 1D texture array.
data Texture1DArray (n :: Nat) (f :: *) = Texture1DArray {
    texture1DArrayBase :: BaseTexture
  , texture1DArrayW    :: Natural
  } deriving (Eq,Show)

instance (KnownNat n,Pixel f) => Texture (Texture1DArray n f) where
  type TextureSize (Texture1DArray n f) = Natural
  -- |(layer,offset)
  type TextureOffset (Texture1DArray n f) = (Natural,Natural)
  fromBaseTexture = Texture1DArray
  toBaseTexture = texture1DArrayBase
  textureTypeEnum _ = GL_TEXTURE_1D_ARRAY
  textureSize = texture1DArrayW
  textureStorage _ tid levels w =
    glTextureStorage2D tid levels (pixelIFormat (Proxy :: Proxy f)) (fromIntegral w)
      (fromIntegral $ natVal (Proxy :: Proxy n))
  transferTexelsSub _ tid (layer,x) w texels =
      unsafeWith texels $ glTextureSubImage2D tid 0 (fromIntegral x) (fromIntegral w) (fromIntegral layer) 1 fmt
        typ . castPtr
    where
      proxy = Proxy :: Proxy f
      fmt = pixelFormat proxy
      typ = pixelType proxy
  fillTextureSub _ tid (layer,x) w filling =
      unsafeWith filling $ glClearTexSubImage tid 0 (fromIntegral x) (fromIntegral layer) 0 (fromIntegral w) 1 1
        fmt typ . castPtr
    where
      proxy = Proxy :: Proxy f
      fmt = pixelFormat proxy
      typ = pixelType proxy
