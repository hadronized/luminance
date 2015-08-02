-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.VertexArray where

import Control.Monad ( (>=>) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Resource ( MonadResource, register )
import Data.Int ( Int32 )
import Data.Proxy ( Proxy )
import Data.Word ( Word32 )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( pokeArray )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr, castPtr, plusPtr )
import Foreign.Storable ( Storable(..) )
import GHC.TypeLits ( Nat, natVal )
import Graphics.GL
import Graphics.Luminance.Buffer
import Graphics.Luminance.GPU
import Graphics.Luminance.Tuple
import Numeric.Natural ( Natural )

newtype VertexArray = VertexArray { vertexArrayID :: GLuint } deriving (Eq,Show)

{-
createVertexArray :: (Traversable t,Foldable f,MonadIO m,MonadResource m)
                  => t v
                  -> f Word32
                  -> m ()
createVertexArray vertices indices = do
    -- create the vertex array object (OpenGL-side)
    vid <- liftIO . alloca $ \p -> do
      glCreateVertexArrays 1 p
      peek p
    register . with vid $ glDeleteVertexArrays 1
    -- vertex buffer
    (vreg,vbo :: Region W _,vbo) <- createBuffer_ $ newRegion vertNb
    -- element buffer
    (ireg :: Region W Word32,ibo) <- createBuffer_ $ newRegion (fromIntegral vertNb)
    writeWhole ireg indices
    liftIO $ glVertexArrayElementBuffer vid (bufferID ibo)
  where
    vertNb = length vertices
    indNb  = length indices
-}

data V :: Nat -> * -> * where
  V1 :: !a -> V 1 a
  V2 :: !a -> !a -> V 2 a
  V3 :: !a -> !a -> !a -> V 3 a
  V4 :: !a -> !a -> !a -> !a -> V 4 a

class Vertex v where
  writeV :: (MonadIO m) => v -> Ptr () -> m (Ptr ())
  setFormatV :: (MonadIO m) => GLuint -> GLuint -> v -> m ()

-- TODO: relative offset?
instance (GPU a,Storable a) => Vertex (V 1 a) where
  writeV (V1 x) p = liftIO $ do
    poke (castPtr p) x
    pure $ p `plusPtr` sizeOf x
  setFormatV vao index _ = do
    glVertexArrayAttribFormat vao index 1 (glType (undefined :: a)) GL_FALSE 0
    glVertexArrayAttribBinding vao index 0
    glEnableVertexArrayAttrib vao index

instance (GPU a,Storable a) => Vertex (V 2 a) where
  writeV (V2 x y) p = liftIO $ do
    pokeArray (castPtr p) [x,y]
    pure $ p `plusPtr` (sizeOf x * 2)
  setFormatV vao index _ = do
    glVertexArrayAttribFormat vao index 2 (glType (undefined :: a)) GL_FALSE 0
    glVertexArrayAttribBinding vao index 0
    glEnableVertexArrayAttrib vao index

instance (GPU a,Storable a) => Vertex (V 3 a) where
  writeV (V3 x y z) p = liftIO $ do
    pokeArray (castPtr p) [x,y,z]
    pure $ p `plusPtr` (sizeOf x * 3)
  setFormatV vao index _ = do
    glVertexArrayAttribFormat vao index 3 (glType (undefined :: a)) GL_FALSE 0
    glVertexArrayAttribBinding vao index 0
    glEnableVertexArrayAttrib vao index

instance (GPU a,Storable a) => Vertex (V 4 a) where
  writeV (V4 x y z w) p = liftIO $ do
    pokeArray (castPtr p) [x,y,z,w]
    pure $ p `plusPtr` (sizeOf x * 4)
  setFormatV vao index _ = do
    glVertexArrayAttribFormat vao index 4 (glType (undefined :: a)) GL_FALSE 0
    glVertexArrayAttribBinding vao index 0
    glEnableVertexArrayAttrib vao index

{-
instance (Vertex a,Vertex b) => Vertex (a :. b) where
  writeV (a :. b) = writeV a >=> writeV b

instance (Vertex a,Vertex b) => Vertex (a,b) where
  writeV (a,b) = writeV a >=> writeV b

instance (Vertex a,Vertex b,Vertex c) => Vertex (a,b,c) where
  writeV (a,b,c) = writeV a >=> writeV b >=> writeV c

instance (Vertex a,Vertex b,Vertex c,Vertex d) => Vertex (a,b,c,d) where
  writeV (a,b,c,d) = writeV a >=> writeV b >=> writeV c >=> writeV d

instance (Vertex a,Vertex b,Vertex c,Vertex d,Vertex e) => Vertex (a,b,c,d,e) where
  writeV (a,b,c,d,e) = writeV a >=> writeV b >=> writeV c >=> writeV d >=> writeV e

instance (Vertex a,Vertex b,Vertex c,Vertex d,Vertex e,Vertex f) => Vertex (a,b,c,d,e,f) where
  writeV (a,b,c,d,e,f) = writeV a >=> writeV b >=> writeV c >=> writeV d >=> writeV e >=> writeV f

instance (Vertex a,Vertex b,Vertex c,Vertex d,Vertex e,Vertex f,Vertex g) => Vertex (a,b,c,d,e,f,g) where
  writeV (a,b,c,d,e,f,g) = writeV a >=> writeV b >=> writeV c >=> writeV d >=> writeV e >=> writeV f >=> writeV g

writeV1 :: (MonadIO m,Storable a) => V 1 a -> Ptr () -> m (Ptr ())
writeV1 (V1 x) p = liftIO $ do
  poke (castPtr p) x
  pure $ p `plusPtr` sizeOf x

writeV2 :: (MonadIO m,Storable a) => V 2 a -> Ptr () -> m (Ptr ())
writeV2 (V2 x y) p = liftIO $ do
  pokeArray (castPtr p) [x,y]
  pure $ p `plusPtr` (sizeOf x * 2)

writeV3 :: (MonadIO m,Storable a) => V 3 a -> Ptr () -> m (Ptr ())
writeV3 (V3 x y z) p = liftIO $ do
  pokeArray (castPtr p) [x,y,z]
  pure $ p `plusPtr` (sizeOf x * 3)

writeV4 :: (MonadIO m,Storable a) => V 4 a -> Ptr () -> m (Ptr ())
writeV4 (V4 x y z w) p = liftIO $ do
  pokeArray (castPtr p) [x,y,z,w]
  pure $ p `plusPtr` (sizeOf x * 4)
-}
