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

import Data.Foldable ( toList )
import Data.Proxy ( Proxy(..) )
import Foreign.Marshal.Array ( withArray )
import Foreign.Ptr ( castPtr )
import Graphics.Luminance.Core.Texture ( BaseTexture(..), Texture(..) )
import Graphics.Luminance.Core.Pixel ( Pixel(..) )
import Graphics.GL
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
  textureStorage _ tid levels (w,h) =
    glTextureStorage2D tid levels (pixelIFormat (Proxy :: Proxy f)) (fromIntegral w) (fromIntegral h)
  transferTexelsSub _ tid (x,y) (w,h) texels =
      withArray (toList texels) $ glTextureSubImage2D tid 0 (fromIntegral x) (fromIntegral y)
        (fromIntegral w) (fromIntegral h) fmt typ . castPtr
    where
      proxy = Proxy :: Proxy f
      fmt = pixelFormat proxy
      typ = pixelType proxy
  fillTextureSub _ tid (x,y) (w,h) filling =
      withArray (toList filling) $ glClearTexSubImage tid 0 (fromIntegral x)
        (fromIntegral y) 0 (fromIntegral w) (fromIntegral h) 1 fmt typ . castPtr
    where
      proxy = Proxy :: Proxy f
      fmt = pixelFormat proxy
      typ = pixelType proxy
