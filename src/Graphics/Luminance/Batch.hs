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

import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Foldable ( traverse_ )
import Foreign.Ptr ( nullPtr )
import Graphics.GL
import Graphics.Luminance.Framebuffer ( Framebuffer(..) )
import Graphics.Luminance.Geometry ( Geometry(..), VertexArray(..) )
import Graphics.Luminance.Shader.Program ( Program(..) )

data FBBatch = FBBatch {
    fbBatchFramebuffer :: forall c d rw. Framebuffer rw c d
  , fbBatchSPBatch     :: [SPBatch]
  }

data SPBatch = SPBatch {
    spBatchShaderProgram :: Program
  , spBatchGeometries    :: [Geometry]
  }

drawGeometry :: (MonadIO m) => Geometry -> m ()
drawGeometry g = case g of
  DirectGeometry (VertexArray vid mode vbNb) -> do
    glBindVertexArray vid
    glDrawArrays mode 0 vbNb
  IndexedGeometry (VertexArray vid mode ixNb) -> do
    glBindVertexArray vid
    glDrawElements mode ixNb GL_UNSIGNED_INT nullPtr

treatSPBatch :: (MonadIO m) => SPBatch -> m ()
treatSPBatch (SPBatch prog geometries) = do
  liftIO $ glUseProgram (programID prog)
  traverse_ drawGeometry geometries

treatFBBatch :: (MonadIO m) => FBBatch -> m ()
treatFBBatch (FBBatch fb spbs) = do
  liftIO $ glBindFramebuffer GL_DRAW_FRAMEBUFFER (fromIntegral $ framebufferID fb)
  traverse_ treatSPBatch spbs
