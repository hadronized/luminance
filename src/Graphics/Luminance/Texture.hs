-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Texture (
    -- * Texture information and creation
    Texture(TextureSize, TextureOffset)
  , createTexture
    -- * Sampling
  , Sampling(..)
  , defaultSampling
    -- * Texture sampler customization
  , Filter(..)
  , Wrap(..)
  , CompareFunc(..)
    -- * Texture operations
  , uploadSub
  , fillSub
    -- * Available textures
    -- ** 1D textures
  , Texture1D
  , texture1DW
    -- *** Array texture
  , Texture1DArray
  , texture1DArrayW
    -- ** 2D textures
  , Texture2D
  , texture2DW
  , texture2DH
    -- *** Array texture
  , Texture2DArray
  , texture2DArrayW
    -- ** 3D textures
  , Texture3D
  , texture3DW
  , texture3DH
  , texture3DD
    -- ** Cubemaps
  , Cubemap
  , CubeFace(..)
  , cubemapW
  , cubemapH
    -- ** Array textures
  , CubemapArray
  , cubemapArrayW
  , cubemapArrayH
  ) where
  
import Graphics.Luminance.Core.Cubemap
import Graphics.Luminance.Core.CubemapArray
import Graphics.Luminance.Core.Texture
import Graphics.Luminance.Core.Texture1D
import Graphics.Luminance.Core.Texture1DArray
import Graphics.Luminance.Core.Texture2D
import Graphics.Luminance.Core.Texture2DArray
import Graphics.Luminance.Core.Texture3D
