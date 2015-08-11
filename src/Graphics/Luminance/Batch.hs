-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Batch where

import Control.Monad.IO.Class ( MonadIO )
import Graphics.GL
import Graphics.Luminance.Geometry ( Geometry(..), VertexArray(..) )

drawGeometry :: (MonadIO m) => Geometry -> m ()
drawGeometry g = case g of
  DirectGeometry (VertexArray vid mode vbNb) -> do
    glBindVertexArray vid
    glDrawArrays mode 0 vbNb
  IndexedGeometry (VertexArray vid mode ixNb) -> do
    glBindVertexArray vid
    glDrawElements mode ixNb GL_UNSIGNED_INT nullPtr
