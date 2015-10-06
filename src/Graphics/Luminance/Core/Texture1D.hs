-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Core.Texture1D where

import Data.Foldable ( toList )
import Data.Proxy ( Proxy(..) )
import Foreign.Marshal.Array ( withArray )
import Foreign.Ptr ( castPtr )
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
  textureStorage _ tid levels w =
    glTextureStorage1D tid levels (pixelIFormat (Proxy :: Proxy f)) (fromIntegral w)
  transferTexelsSub _ tid x w texels =
      withArray (toList texels) $ glTextureSubImage1D tid 0 (fromIntegral x) (fromIntegral w) fmt
        typ . castPtr
    where
      proxy = Proxy :: Proxy f
      fmt = pixelFormat proxy
      typ = pixelType proxy
  fillTextureSub _ tid x w filling =
      withArray (toList filling) $ glClearTexSubImage tid 0 (fromIntegral x) 0 0 (fromIntegral w) 1 1
        fmt typ . castPtr
    where
      proxy = Proxy :: Proxy f
      fmt = pixelFormat proxy
      typ = pixelType proxy
