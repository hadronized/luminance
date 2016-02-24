{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015, 2016 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-----------------------------------------------------------------------------

module Graphics.Luminance.TextureDriver where

import Data.Vector.Storable ( Vector )
import GHC.Exts ( Constraint )
import Graphics.Luminance.Texture ( Sampling )
import Numeric.Natural ( Natural )

-- |A driver to implement to provide texture features.
class (Monad m) => TextureDriver m where
  -- |All possible texture types.
  type Texture m :: * -> Constraint
  -- |Associate a texture size type to a texture.
  type TextureSize m :: * -> *
  -- |Associate a texture offset type to a texture.
  type TextureOffset m :: * -> *
  -- |Type of 1D textures.
  type Texture1D m :: *
  -- |Type of 2D textures.
  type Texture2D m :: *
  -- |Type of 3D textures.
  type Texture3D m :: *
  -- |Type of cubemap textures.
  type Cubemap m :: *
  -- |Type of cubemaps’ faces.
  type CubeFace m :: *
  -- |Type of texture arrays.
  type TextureArray m :: * -> *
  -- |@'createTexture' size lvl smpl@ creates a new texture with @lvl@ levels and which size is
  -- @size@. The format is set through the type. @smpl@ is the 'Sampling' parameter.
  createTexture :: (Texture m t) => TextureSize m t -> Natural -> Sampling -> m t
  -- |@'uploadTexture tex offset size autolvl texels@ uploads data to a subpart of the texture’s
  -- storage. The offset is given with origin at upper-left corner, and @size@ is the size of the
  -- area to upload to. @autolvl@ is a 'Bool' that can be used to automatically generate mipmaps.
  uploadSub :: (Texture m t)
            => t
            -> TextureOffset m t
            -> TextureSize m t
            -> Bool
            -> Vector a
            -> m ()
  -- |Fill a subpart of the texture’s storage with a given value.
  fillTexture :: (Texture m t)
              => t
              -> TextureOffset m t
              -> TextureSize m t
              -> Bool
              -> Vector a
              -> m ()
