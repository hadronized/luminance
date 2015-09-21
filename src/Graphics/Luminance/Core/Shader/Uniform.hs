-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

module Graphics.Luminance.Core.Shader.Uniform where

import Data.Functor.Contravariant ( Contravariant(..) )
import Data.Functor.Contravariant.Divisible ( Decidable(..), Divisible(..) )
import Data.Int ( Int32 )
import Data.Semigroup ( Semigroup(..) )
import Data.Void ( absurd )
import Data.Word ( Word32 )
import Foreign.Marshal.Array ( withArrayLen )
import Graphics.GL
import Graphics.GL.Ext.ARB.BindlessTexture ( glProgramUniformHandleui64ARB )
import Graphics.Luminance.Core.Texture ( Texture2D(textureHandle) )

--------------------------------------------------------------------------------
-- Uniform ---------------------------------------------------------------------

-- |Class of types that can be sent down to shaders. That class is closed because shaders cannot
-- handle a lot of uniform types. However, you should have a look at the 'U' documentation for
-- further information about how to augment the scope of the types you can send down to shaders.
class Uniform a where
  toU :: GLuint -> GLint -> U a

-- |A shader uniform. @'U' a@ doesn’t hold any value. It’s more like a mapping between the host
-- code and the shader the uniform was retrieved from.
--
-- 'U' is contravariant in its argument. That means that you can use 'contramap' to build more
-- interesting uniform types. It’s also a divisible contravariant functor, then you can divide
-- structures to take advantage of divisible contravariant properties and then glue several 'U'
-- with different types. That can be useful to build a uniform type by gluing its fields.
--
-- Another interesting part is the fact that 'U' is also monoidal. You can accumulate several of
-- them with '(<>)' if they have the same type. That means that you can join them so that when you
-- pass an actual value, it gets shared inside the resulting value.
--
-- The '()' instance doesn’t do anything and doesn’t even use its argument ('()').
newtype U a = U { runU :: a -> IO () }

instance Contravariant U where
  contramap f u = U $ runU u . f

instance Decidable U where
  lose f = U $ absurd . f
  choose f p q = U $ either (runU p) (runU q) . f

instance Divisible U where
  divide f p q = U $ \a -> do
    let (b,c) = f a
    runU p b
    runU q c
  conquer = mempty

instance Monoid (U a) where
  mempty = U . const $ pure ()
  mappend = (<>)

instance Semigroup (U a) where
  u <> v = U $ \a -> runU u a >> runU v a

--------------------------------------------------------------------------------
-- Unit instance ---------------------------------------------------------------

instance Uniform () where
  toU _ _ = mempty

--------------------------------------------------------------------------------
-- Int32 instances -------------------------------------------------------------

-- scalar
instance Uniform Int32 where
  toU prog l = U $ glProgramUniform1i prog l

-- D2
instance Uniform (Int32,Int32) where
  toU prog l = U $ \(x,y) -> glProgramUniform2i prog l x y

-- D3
instance Uniform (Int32,Int32,Int32) where
  toU prog l = U $ \(x,y,z) -> glProgramUniform3i prog l x y z

-- D4
instance Uniform (Int32,Int32,Int32,Int32) where
  toU prog l = U $ \(x,y,z,w) -> glProgramUniform4i prog l x y z w

-- scalar array
instance Uniform [Int32] where
  toU prog l = U $ \v -> withArrayLen v $ glProgramUniform1iv prog l . fromIntegral

-- D2 array
instance Uniform [(Int32,Int32)] where
  toU prog l = U $ \v -> withArrayLen (concatMap unPair v) $
    glProgramUniform2iv prog l . fromIntegral

-- D3 array
instance Uniform [(Int32,Int32,Int32)] where
  toU prog l = U $ \v -> withArrayLen (concatMap unTriple v) $
    glProgramUniform3iv prog l . fromIntegral

-- D4 array
instance Uniform [(Int32,Int32,Int32,Int32)] where
  toU prog l = U $ \v -> withArrayLen (concatMap unQuad v) $
    glProgramUniform4iv prog l . fromIntegral

--------------------------------------------------------------------------------
-- Word32 instances ------------------------------------------------------------

-- scalar
instance Uniform Word32 where
  toU prog l = U $ glProgramUniform1ui prog l

-- D2
instance Uniform (Word32,Word32) where
  toU prog l = U $ \(x,y) -> glProgramUniform2ui prog l x y

-- D3
instance Uniform (Word32,Word32,Word32) where
  toU prog l = U $ \(x,y,z) -> glProgramUniform3ui prog l x y z

-- D4
instance Uniform (Word32,Word32,Word32,Word32) where
  toU prog l = U $ \(x,y,z,w) -> glProgramUniform4ui prog l x y z w

-- scalar array
instance Uniform [Word32] where
  toU prog l = U $ \v -> withArrayLen v $
    glProgramUniform1uiv prog l . fromIntegral

-- D2 array
instance Uniform [(Word32,Word32)] where
  toU prog l = U $ \v -> withArrayLen (concatMap unPair v) $
    glProgramUniform2uiv prog l . fromIntegral

-- D3 array
instance Uniform [(Word32,Word32,Word32)] where
  toU prog l = U $ \v -> withArrayLen (concatMap unTriple v) $
    glProgramUniform3uiv prog l . fromIntegral

-- D4 array
instance Uniform [(Word32,Word32,Word32,Word32)] where
  toU prog l = U $ \v -> withArrayLen (concatMap unQuad v) $
    glProgramUniform4uiv prog l . fromIntegral

--------------------------------------------------------------------------------
-- Float instances -------------------------------------------------------------

-- scalar
instance Uniform Float where
  toU prog l = U $ glProgramUniform1f prog l

-- D2
instance Uniform (Float,Float) where
  toU prog l = U $ \(x,y) -> glProgramUniform2f prog l x y

-- D3
instance Uniform (Float,Float,Float) where
  toU prog l = U $ \(x,y,z) -> glProgramUniform3f prog l x y z

-- D4
instance Uniform (Float,Float,Float,Float) where
  toU prog l = U $ \(x,y,z,w) -> glProgramUniform4f prog l x y z w

-- scalar array
instance Uniform [Float] where
  toU prog l = U $ \v -> withArrayLen v $
    glProgramUniform1fv prog l . fromIntegral

-- D2 array
instance Uniform [(Float,Float)] where
  toU prog l = U $ \v -> withArrayLen (concatMap unPair v) $
    glProgramUniform2fv prog l . fromIntegral

-- D3 array
instance Uniform [(Float,Float,Float)] where
  toU prog l = U $ \v -> withArrayLen (concatMap unTriple v) $
    glProgramUniform3fv prog l . fromIntegral

-- D4 array
instance Uniform [(Float,Float,Float,Float)] where
  toU prog l = U $ \v -> withArrayLen (concatMap unQuad v) $
    glProgramUniform4fv prog l . fromIntegral

--------------------------------------------------------------------------------
-- Texture2D -------------------------------------------------------------------
instance Uniform (Texture2D f) where
  toU prog l = U $ glProgramUniformHandleui64ARB prog l . textureHandle

--------------------------------------------------------------------------------
-- Untuple functions -----------------------------------------------------------

unPair :: (a,a) -> [a]
unPair (x,y) = [x,y]

unTriple :: (a,a,a) -> [a]
unTriple (x,y,z) = [x,y,z]

unQuad :: (a,a,a,a) -> [a]
unQuad (x,y,z,w) = [x,y,z,w]