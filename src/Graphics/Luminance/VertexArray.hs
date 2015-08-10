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

import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Resource ( MonadResource, register )
import Data.Proxy ( Proxy )
import Data.Word ( Word32 )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Utils ( with )
import Foreign.Storable ( Storable(..) )
import GHC.TypeLits ( KnownNat, Nat, natVal )
import Graphics.GL
import Graphics.Luminance.Buffer
import Graphics.Luminance.GPU
import Graphics.Luminance.RW ( W )
import Graphics.Luminance.Tuple

vertexBindingIndex :: GLuint
vertexBindingIndex = 0

newtype VertexArray = VertexArray { vertexArrayID :: GLuint } deriving (Eq,Show)

createVertexArray :: forall f m t v. (Foldable f,MonadIO m,MonadResource m,Storable v,Traversable t,Vertex v)
                  => t v
                  -> f Word32
                  -> m VertexArray
createVertexArray vertices indices = do
    -- create the vertex array object (OpenGL-side)
    vid <- liftIO . alloca $ \p -> do
      glCreateVertexArrays 1 p
      peek p
    _ <- register . with vid $ glDeleteVertexArrays 1
    -- vertex buffer
    (vreg :: Region W v,vbo) <- createBuffer_ $ newRegion (fromIntegral vertNb)
    writeWhole vreg vertices
    -- element buffer
    (ireg :: Region W Word32,ibo) <- createBuffer_ $ newRegion (fromIntegral indNb)
    writeWhole ireg indices
    -- connect the buffers to the VAO
    liftIO $ do
      glVertexArrayVertexBuffer vid vertexBindingIndex (bufferID vbo) 0 (fromIntegral $ sizeOf (undefined :: v))
      glVertexArrayElementBuffer vid (bufferID ibo)
      setFormatV vid 0 (undefined :: v)
    pure $ VertexArray vid
  where
    vertNb = length vertices
    indNb  = length indices

data V :: Nat -> * -> * where
  V1 :: !a -> V 1 a
  V2 :: !a -> !a -> V 2 a
  V3 :: !a -> !a -> !a -> V 3 a
  V4 :: !a -> !a -> !a -> !a -> V 4 a

class Vertex v where
  setFormatV :: (MonadIO m) => GLuint -> GLuint -> v -> m ()

instance (GPU a,KnownNat n,Storable a) => Vertex (V n a) where
  setFormatV vid index _ = do
    glVertexArrayAttribFormat vid index (fromIntegral $ natVal (undefined :: Proxy n)) (glType (undefined :: a)) GL_FALSE 0
    glVertexArrayAttribBinding vid index vertexBindingIndex
    glEnableVertexArrayAttrib vid index

instance (Vertex a,Vertex b) => Vertex (a :. b) where
  setFormatV vid index (a :. b) = do
    setFormatV vid index a
    setFormatV vid index b

