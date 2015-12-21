{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Core.Geometry where

import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Resource ( MonadResource, register )
import Data.DList as DL ( empty, snoc )
import Data.Foldable ( toList )
import Data.Map as M ( empty, insert, lookup, size )
import Data.Proxy ( Proxy(..) )
import Data.Word ( Word32 )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Utils ( with )
import Foreign.Storable ( Storable(..) )
import Graphics.GL
import Graphics.Luminance.Core.Buffer
import Graphics.Luminance.Core.RW ( W )
import Graphics.Luminance.Core.Vertex

-- OpenGL vertex array. Used as a shared type for embedding in most complex 'Geometry' type.
data VertexArray = VertexArray {
    vertexArrayID :: GLuint
  , vertexArrayMode :: GLenum
  , vertexArrayCount :: GLsizei
  } deriving (Eq,Show)
  
-- |A 'Geometry' represents a GPU version of a mesh; that is, vertices attached with indices and a
-- geometry mode. You can have 'Geometry' in two flavours:
--
-- - /direct geometry/: doesn’t require any indices as all vertices are unique and in the right
--   order to connect vertices between each other ;
-- - /indexed geometry/: requires indices to know how to connect and share vertices between each
--   other.
data Geometry
  = DirectGeometry VertexArray
  | IndexedGeometry VertexArray
    deriving (Eq,Show)

-- |The 'GeometryMode' is used to specify how vertices should be connected between each other.
--
-- A 'Point' mode won’t connect vertices at all and will leave them as a vertices cloud.
--
-- A 'Line' mode will connect vertices two-by-two. You then have to provide pairs of indices to
-- correctly connect vertices and form lines.
--
-- A 'Triangle' mode will connect vertices three-by-three. You then have to provide triplets of
-- indices to correctly connect vertices and form triangles.
data GeometryMode
  = Point
  | Line
  | Triangle
    deriving (Eq,Show)

fromGeometryMode :: GeometryMode -> GLenum
fromGeometryMode m = case m of
  Point    -> GL_POINTS
  Line     -> GL_LINES
  Triangle -> GL_TRIANGLES

-- |This function is the single one to create 'Geometry'. It takes a 'Foldable' type of vertices
-- used to provide the 'Geometry' with vertices and might take a 'Foldable' of indices ('Word32').
-- If you don’t pass indices ('Nothing'), you end up with a /direct geometry/. Otherwise, you get an
-- /indexed geometry/. You also have to provide a 'GeometryMode' to state how you want the vertices
-- to be connected with each other.
createGeometry :: forall f m v. (Foldable f,MonadResource m,Storable v,Vertex v)
               => f v
               -> Maybe (f Word32)
               -> GeometryMode
               -> m Geometry
#ifdef __GL45
createGeometry vertices indices mode = do
    -- create the vertex array object (OpenGL-side)
    vid <- liftIO . alloca $ \p -> do
      glCreateVertexArrays 1 p
      peek p
    _ <- register . with vid $ glDeleteVertexArrays 1
    -- vertex buffer
    (vreg :: Region W v,vbo) <- createBuffer_ $ newRegion (fromIntegral vertNb)
    writeWhole vreg vertices
    liftIO $ glVertexArrayVertexBuffer vid vertexBindingIndex (bufferID vbo) 0 (fromIntegral $ sizeOf (undefined :: v))
    _ <- setFormatV vid 0 0 (Proxy :: Proxy v)
    -- element buffer, if required
    case indices of
      Just indices' -> do
        let ixNb = length indices'
        (ireg :: Region W Word32,ibo) <- createBuffer_ $ newRegion (fromIntegral ixNb)
        writeWhole ireg indices'
        glVertexArrayElementBuffer vid (bufferID ibo)
        pure . IndexedGeometry $ VertexArray vid mode' (fromIntegral ixNb)
      Nothing -> pure . DirectGeometry $ VertexArray vid mode' (fromIntegral vertNb)
  where
    vertNb = length vertices
    mode'  = fromGeometryMode mode
#elif defined(__GL33)
createGeometry vertices indices mode = do
    -- create the vertex array object (OpenGL-side)
    vid <- liftIO . alloca $ \p -> do
      glGenVertexArrays 1 p
      peek p
    _ <- register . with vid $ glDeleteVertexArrays 1
    glBindVertexArray vid
    -- vertex buffer
    (vreg :: Region W v,vbo) <- createBuffer_ $ newRegion (fromIntegral vertNb)
    writeWhole vreg vertices
    liftIO $ glBindBuffer GL_ARRAY_BUFFER (bufferID vbo)
    _ <- setFormatV vid 0 0 (Proxy :: Proxy v)
    -- element buffer, if required
    case indices of
      Just indices' -> do
        let ixNb = length indices'
        (ireg :: Region W Word32,ibo) <- createBuffer_ $ newRegion (fromIntegral ixNb)
        writeWhole ireg indices'
        glBindBuffer GL_ELEMENT_ARRAY_BUFFER(bufferID ibo)
        glBindVertexArray 0
        pure . IndexedGeometry $ VertexArray vid mode' (fromIntegral ixNb)
      Nothing -> do
        glBindVertexArray 0
        pure . DirectGeometry $ VertexArray vid mode' (fromIntegral vertNb)
  where
    vertNb = length vertices
    mode'  = fromGeometryMode mode
#endif

-- |/O (n log n)/
--
-- Turn /direct geometry data/ into /indirect data/. This function removes duplicate vertices from
-- the data you pass in and registers indices in consequence.
nubDirect :: (Foldable f,Ord a,Integral i) => f a -> ([a],[i])
nubDirect dvertices = (toList uvertices,toList indices)
  where
    (uvertices,indices,_) = foldl rmDup (DL.empty,DL.empty,M.empty) dvertices
    rmDup (uverts,inds,seen) v =
      case M.lookup v seen of
        Just i -> (uverts,inds `snoc` i,seen)
        Nothing ->
          let s = fromIntegral (size seen)
          in (uverts `snoc` v,inds `snoc` s,insert v s seen)
