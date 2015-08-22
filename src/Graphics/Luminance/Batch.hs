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

import Control.Monad ( void )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Foldable ( traverse_ )
import Foreign.Ptr ( nullPtr )
import Graphics.GL
import Graphics.Luminance.Blending ( setBlending )
import Graphics.Luminance.Framebuffer ( Framebuffer(..) )
import Graphics.Luminance.Geometry ( Geometry(..), VertexArray(..) )
import Graphics.Luminance.Shader.Program ( Program(..) )
import Graphics.Luminance.RenderCmd ( RenderCmd(..) )
import Graphics.Luminance.Shader.Uniform ( Uniformed(..) )

-- FIXME: TEST ONLY
import Data.Bits

data FBBatch rw c d = FBBatch {
    fbBatchFramebuffer :: Framebuffer rw c d
  , fbBatchSPBatch     :: [SPBatch rw c d]
  }

data SPBatch rw c d = SPBatch {
    spBatchShaderProgram :: Program
  , spBatchUniformed     :: Uniformed ()
  , spBatchGeometries    :: [RenderCmd rw c d Geometry]
  }

drawGeometry :: (MonadIO m) => RenderCmd rw c d Geometry -> m ()
drawGeometry (RenderCmd blending depthTest g) = do
  traverse_ (\(mode,src,dst) -> setBlending mode src dst) blending
  (if depthTest then glEnable else glDisable) GL_DEPTH_TEST
  geometry <- liftIO (runUniformed g)
  case geometry of
    DirectGeometry (VertexArray vid mode vbNb) -> do
      glBindVertexArray vid
      glDrawArrays mode 0 vbNb
    IndexedGeometry (VertexArray vid mode ixNb) -> do
      glBindVertexArray vid
      glDrawElements mode ixNb GL_UNSIGNED_INT nullPtr

treatSPBatch :: (MonadIO m) => SPBatch rw c d -> m ()
treatSPBatch (SPBatch prog uniformed geometries) = do
  liftIO $ do
    glUseProgram (programID prog)
    void $ runUniformed uniformed
  traverse_ drawGeometry geometries

treatFBBatch :: (MonadIO m) => FBBatch rw c d -> m ()
treatFBBatch (FBBatch fb spbs) = do
  liftIO $ glBindFramebuffer GL_DRAW_FRAMEBUFFER (fromIntegral $ framebufferID fb)
  -- FIXME: TEST ONLY
  liftIO $ do
    glEnable GL_DEPTH_TEST
    glClear $ GL_DEPTH_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT
  traverse_ treatSPBatch spbs
