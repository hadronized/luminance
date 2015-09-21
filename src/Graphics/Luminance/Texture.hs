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
    Texture2D
  , textureID
  , textureHandle
  , textureW
  , textureH
  , textureFormat
  , textureType
  , createTexture
    -- * Sampling
  , Sampling(..)
  , defaultSampling
    -- * Texture sampler customization
  , Filter(..)
  , Wrap(..)
  , CompareFunc(..)
    -- * Texture operations
  , uploadWhole
  , uploadSub
  , fillWhole
  , fillSub
  ) where
  
import Graphics.Luminance.Core.Texture
