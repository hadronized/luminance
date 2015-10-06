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
    Texture
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
    -- ** 2D textures
  , Texture2D
  , texture2DW
  , texture2DH
  ) where
  
import Graphics.Luminance.Core.Texture
import Graphics.Luminance.Core.Texture2D
