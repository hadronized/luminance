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

module Graphics.Luminance.GeometryDriver where

import Data.Word ( Word32 )
import GHC.Exts ( Constraint )
import Graphics.Luminance.Geometry ( GeometryMode )

-- |A driver to implement to provide geometry features.
class (Monad m) => GeometryDriver m where
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
