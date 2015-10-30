{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

module Graphics.Luminance.Core.Vertex (
    VertexAttribute(..)
  , Vertex(..)
#if GL45_BACKEND
  , vertexBindingIndex
#endif
  , module Linear.V
  , vec2
  , vec3
  , vec4
  ) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Int ( Int32 )
import Data.Proxy ( Proxy(..) )
import Data.Word ( Word32 )
import Foreign.Storable ( Storable(sizeOf) )
#if GL32_BACKEND
import Foreign.Ptr ( nullPtr )
#endif
import GHC.TypeLits ( KnownNat, natVal )
import Graphics.GL
import Graphics.Luminance.Core.Tuple
import Linear.V

-- |Create a new @'V' 2@.
vec2 :: a -> a -> V 2 a
vec2 x y = V [x,y]

-- |Create a new @'V' 3@.
vec3 :: a -> a -> a -> V 3 a
vec3 x y z = V [x,y,z]

-- |Create a new @'V' 4@.
vec4 :: a -> a -> a -> a -> V 4 a
vec4 x y z w = V [x,y,z,w]

-- |A vertex might have several attributes. The types of those attributes have to implement the
-- 'VertexAttribute' typeclass in order to be used as vertex attributes.
class VertexAttribute a where
  vertexGLType :: proxy a -> GLenum

instance VertexAttribute Float where
  vertexGLType _ = GL_FLOAT

instance VertexAttribute Int32 where
  vertexGLType _ = GL_INT

instance VertexAttribute Word32 where
  vertexGLType _ = GL_UNSIGNED_INT

-- |A vertex has to implement 'Vertex' in order to be used as-is. That typeclass is closed, so you
-- cannot add anymore instances. However, you shouldn’t need to since you can use the already
-- provided types to build up your vertex type.
class Vertex v where
  -- @setFormatV vid index offset proxy@ sets the format of a vertex type. The returned value is the
  -- next index that can be used – and is used to chain index creation – along with the next offset
  -- to use.
  setFormatV :: (MonadIO m) => GLuint -> GLuint -> GLuint -> proxy v -> m (GLuint,GLuint)

instance (KnownNat n,Storable a,VertexAttribute a) => Vertex (V n a) where
#if GL45_BACKEND
  setFormatV vid index offset _ = do
    glVertexArrayAttribFormat vid index (fromIntegral $ natVal (Proxy :: Proxy n)) (vertexGLType (Proxy :: Proxy a)) GL_FALSE offset
    glVertexArrayAttribBinding vid index vertexBindingIndex
    glEnableVertexArrayAttrib vid index
#elif GL32_BACKEND
  setFormatV _ index offset _ = do
    glVertexAttribPointer index (fromIntegral $ natVal (Proxy :: Proxy n)) (vertexGLType (Proxy :: Proxy a)) GL_FALSE (fromIntegral offset) nullPtr
    glEnableVertexAttribArray index
#endif
    pure (succ index,offset + fromIntegral (sizeOf (undefined :: V n a)))

instance (Vertex a,Vertex b) => Vertex (a :. b) where
  setFormatV vid index offset _ = do
    (nextIndex,nextOffset) <- setFormatV vid index offset (Proxy :: Proxy a)
    setFormatV vid nextIndex nextOffset (Proxy :: Proxy b)

#if GL45_BACKEND
-- Used to connect vertex attribute to the vertex buffer binding point. Should be 0.
vertexBindingIndex :: GLuint
vertexBindingIndex = 0
#endif
