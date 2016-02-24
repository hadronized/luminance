{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
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

module Graphics.Luminance.Driver where

import Control.Monad.Except ( MonadError )
import Data.Semigroup ( Semigroup )
import Data.Vector.Storable ( Vector )
import Data.Word ( Word32 )
import GHC.Exts ( Constraint )
import Graphics.Luminance.Core.Framebuffer ( FramebufferBlitMask, HasFramebufferError )
import Graphics.Luminance.Core.Geometry ( GeometryMode )
import Graphics.Luminance.Core.RW ( Readable, RW, Writable )
import Graphics.Luminance.Core.Shader.Program ( HasProgramError )
import Graphics.Luminance.Core.Shader.Stage ( HasStageError, StageType )
import Graphics.Luminance.BufferDriver
import Numeric.Natural ( Natural )

-- |A driver to implement to be considered as a luminance backend.
class (BufferDriver m) => Driver m where
  -- buffers

  -- textures
  -- |All possible texture types.
  type Texture m :: * -> Constraint
  -- |Associate a texture size type to a texture.
  type TextureSize m :: * -> *
  -- |Associate a texture offset type to a texture.
  type TextureOffset m :: * -> *
  -- |Minification and magnification filter.
  type Filter m :: *
  -- |Sampling parameters.
  type Sampling m :: *
  -- |Wrap filter.
  type Wrap m :: *
  -- |Depth comparison function.
  type CompareFunc m :: *
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
  -- |Default 'Sampling' if you’re juste lazy.
  defaultSampling :: Sampling m
  -- |@'createTexture' size lvl smpl@ creates a new texture with @lvl@ levels and which size is
  -- @size@. The format is set through the type. @smpl@ is the 'Sampling' parameter.
  createTexture :: (Texture m t) => TextureSize m t -> Natural -> Sampling m -> m t
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

  -- pixel formats
  -- |All possible pixel formats.
  type Pixel m :: * -> Constraint

  -- framebuffers
  -- |A 'Framebuffer' represents two buffers: a /color/ buffer and /depth/ buffer.
  -- You can select which one you want and specify the formats to use by providing 'Pixel'
  -- types. If you want to mute a buffer, use '()'.
  type Framebuffer m :: * -> * -> * -> *
  -- |All possible framebuffer color attachments.
  type FramebufferColorAttachment m :: * -> Constraint
  -- |All possible framebuffer depth attachments.
  type FramebufferDepthAttachment m :: * -> Constraint
  -- |@'createFramebuffer' w h mipmaps@ creates a new 'Framebuffer' with dimension @w * h@ and
  -- allocating spaces for @mipmaps@ level of textures. The textures are created by providing a
  -- correct type.
  --
  -- For the color part, you can pass either:
  --
  -- - '()': that will mute the color buffer of the framebuffer;
  -- - @'Format' t c@: that will create a single texture with the wished color format;
  -- - or @a ':.' b@: that will create a chain of textures; 'a' and 'b' cannot be '()'.
  --
  -- For the depth part, you can pass either:
  --
  -- - '()': that will mute the depth buffer of the framebuffer;
  -- - @'Format' t c@: that will create a single texture with the wished depth format.
  --
  -- Finally, the @rw@ parameter can be set to 'R', 'W' or 'RW' to specify which kind of framebuffer
  -- access you’ll need.
  createFramebuffer  :: (FramebufferColorAttachment m c,FramebufferDepthAttachment m d,HasFramebufferError e,MonadError e m)
                     => Natural
                     -> Natural
                     -> Natural
                     -> m (Framebuffer m rw c d)
  -- |The default 'Framebuffer' represents the screen (back buffer with double buffering).
  defaultFramebuffer :: m (Framebuffer m RW () ())
  -- Blit two framebuffers.
  framebufferBlit    :: (Readable r,Writable w)
                     => Framebuffer m r c d
                     -> Framebuffer m w c' d'
                     -> Int
                     -> Int
                     -> Natural
                     -> Natural
                     -> Int
                     -> Int
                     -> Natural
                     -> Natural
                     -> FramebufferBlitMask
                     -> Filter m
                     -> m ()

  -- geometries
  -- |A 'Geometry' represents a GPU version of a mesh; that is, vertices attached with indices and a
  -- geometry mode. 
  --
  -- - /direct geometry/: doesn’t require any indices as all vertices are unique and in the right
  --   order to connect vertices between each other ;
  -- - /indexed geometry/: requires indices to know how to connect and share vertices between each
  --   other.
  type Geometry m :: *
  -- |All accepted types to build up vertices.
  type Vertex m :: * -> Constraint
  -- |This function is the single one to create 'Geometry'. It takes a 'Foldable' type of vertices
  -- used to provide the 'Geometry' with vertices and might take a 'Foldable' of indices ('Word32').
  -- If you don’t pass indices ('Nothing'), you end up with a /direct geometry/. Otherwise, you get an
  -- /indexed geometry/. You also have to provide a 'GeometryMode' to state how you want the vertices
  -- to be connected with each other.
  createGeometry :: (Foldable f,Vertex m v)
                 => f v
                 -> Maybe (f Word32)
                 -> GeometryMode
                 -> m (Geometry m)

  -- shader stages
  -- |A shader stage.
  type Stage m :: *
  -- |Create a shader stage from a 'String' representation of its source code and its type.
  --
  -- Note: on some hardware and backends, /tessellation shaders/ aren’t available. That function
  -- throws 'UnsupportedStage' error in such cases.
  createStage :: (HasStageError e,MonadError e m)
              => StageType
              -> String
              -> m (Stage m)
  -- |Shader program.
  type Program m :: * -> *
  -- |Encode all possible ways to name uniform values.
  type UniformName m :: * -> *
  -- |A special closed, monadic type in which one can create new uniforms.
  type UniformInterface m :: * -> *
  -- |A shader uniform. @'U' a@ doesn’t hold any value. It’s more like a mapping between the host
  -- code and the shader the uniform was retrieved from.
  type U m :: * -> *
  -- |Type-erased 'U'. Used to update uniforms with the 'updateUniforms' function.
  type U' m :: *
  -- |Create a new shader 'Program'.
  --
  -- That function takes a list of 'Stage's and a uniform interface builder function and yields a
  -- 'Program' and the interface.
  --
  -- The builder function takes a function you can use to retrieve uniforms. You can pass
  -- values of type 'UniformName' to identify the uniform you want to retrieve. If the uniform can’t
  -- be retrieved, throws 'InactiveUniform'.
  --
  -- In the end, you get the new 'Program' and a polymorphic value you can choose the type of in
  -- the function you pass as argument. You can use that value to gather uniforms for instance.
  createProgram :: (HasProgramError e,MonadError e m)
                => [Stage m]
                -> ((forall a. UniformName m a -> UniformInterface m (U m a)) -> UniformInterface m i)
                -> m (Program m i)
  -- |Update uniforms in a 'Program'. That function enables you to update only the uniforms you want
  -- and not necessarily the whole.
  --
  -- If you want to update several uniforms (not only one), you can use the 'Semigroup' instance
  -- (use '(<>)' or 'sconcat' for instance).
  updateUniforms :: (Semigroup (U' m)) => Program m a -> (a -> U' m) -> m ()

  -- draw
  -- |Draw output.
  type Output m :: * -> * -> *
  type RenderCommand m :: * -> *
  -- |Issue a draw command to the GPU. Don’t be afraid of the type signature. Let’s explain it.
  --
  -- The first parameter is the framebuffer you want to perform the rendering in. It must be
  -- writable.
  --
  -- The second parameter is a list of /shading commands/. A shading command is composed of three
  -- parts:
  --
  -- * a 'Program' used for shading;
  -- * a @(a -> 'U'')@ uniform sink used to update uniforms in the program passed as first value;
  --   this is useful if you want to update uniforms only once per draw or for all render
  --   commands, like time, user event, etc.;
  -- * a list of /render commands/ function; that function enables you to update uniforms via the
  --   @(a -> 'U'')@ uniform sink for each render command that follows.
  --
  -- This function outputs yields a value of type @'Output' m c d'@, which represents the output of
  -- the render – typically, textures or '()'.
  draw :: (Monoid (U' m),Writable w) => Framebuffer m w c d -> [(Program m a,a -> U' m,[a -> (U' m,RenderCommand m (Geometry m))])] -> m (Output m c d)
