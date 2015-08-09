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
import GHC.TypeLits ( KnownNat, Nat, natVal )
import Graphics.GL
import Graphics.Luminance.Buffer
import Graphics.Luminance.GPU
import Graphics.Luminance.Tuple
import Numeric.Natural ( Natural )

newtype VertexArray = VertexArray { vertexArrayID :: GLuint } deriving (Eq,Show)

{-
createVertexArray :: (Foldable f,MonadIO m,MonadResource m,Traversable t)
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
  setFormatV :: (MonadIO m) => GLuint -> GLuint -> v -> m ()

-- TODO: relative offset?
instance (GPU a,KnownNat n,Storable a) => Vertex (V n a) where
  setFormatV vao index _ = do
    glVertexArrayAttribFormat vao index (fromIntegral $ natVal (undefined :: Proxy n)) (glType (undefined :: a)) GL_FALSE 0
    glVertexArrayAttribBinding vao index 0
    glEnableVertexArrayAttrib vao index

instance (Vertex a,Vertex b) => Vertex (a :. b) where
  setFormatV vao index (a :. b) = do
    setFormatV vao index a
    setFormatV vao index b

instance (Vertex a,Vertex b) => Vertex (a,b) where
  setFormatV vao index (a,b) = setFormatV vao index $ a :. b

instance (Vertex a,Vertex b,Vertex c) => Vertex (a,b,c) where
  setFormatV vao index (a,b,c) = setFormatV vao index $ a :. b :. c

instance (Vertex a,Vertex b,Vertex c,Vertex d) => Vertex (a,b,c,d) where
  setFormatV vao index (a,b,c,d) = setFormatV vao index $ a :. b :. c :. d

instance (Vertex a,Vertex b,Vertex c,Vertex d,Vertex e) => Vertex (a,b,c,d,e) where
  setFormatV vao index (a,b,c,d,e) = setFormatV vao index $ a :. b :. c :. d :. e

instance (Vertex a,Vertex b,Vertex c,Vertex d,Vertex e,Vertex f) => Vertex (a,b,c,d,e,f) where
  setFormatV vao index (a,b,c,d,e,f) = setFormatV vao index $ a :. b :. c :. d :. e :. f

instance (Vertex a,Vertex b,Vertex c,Vertex d,Vertex e,Vertex f,Vertex g) => Vertex (a,b,c,d,e,f,g) where
  setFormatV vao index (a,b,c,d,e,f,g) = setFormatV vao index $ a :. b :. c :. d :. e :. f :. g
