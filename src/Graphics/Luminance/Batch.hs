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
import Graphics.Luminance.Blending ( setBlending )
import Graphics.Luminance.Framebuffer ( Framebuffer(..) )
import Graphics.Luminance.Geometry ( Geometry(..), VertexArray(..) )
import Graphics.Luminance.Shader.Program ( Program(..) )
import Graphics.Luminance.RenderCmd ( RenderCmd(..) )
import Graphics.Luminance.Shader.Uniform ( U(..) )

-- FIXME: TEST ONLY
import Data.Bits

--------------------------------------------------------------------------------
-- Framebuffer batch -----------------------------------------------------------

-- |'Framebuffer' batch.
--
-- A 'FBBatch' is used to expose a 'Framebuffer' and share it between several shader program
-- batches.
data FBBatch rw c d = FBBatch {
    fbBatchFramebuffer :: Framebuffer rw c d
  , fbBatchSPBatch     :: [AnySPBatch rw c d]
  }

-- |Run a 'FBBatch'.
runFBBatch :: (MonadIO m) => FBBatch rw c d -> m ()
runFBBatch (FBBatch fb spbs) = do
  liftIO $ glBindFramebuffer GL_DRAW_FRAMEBUFFER (fromIntegral $ framebufferID fb)
  -- FIXME: TEST ONLY
  liftIO $ glClear $ GL_DEPTH_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT
  traverse_ (\(AnySPBatch spb) -> runSPBatch spb) spbs

-- |Share a 'Framebuffer' between several shader program batches.
framebufferBatch :: Framebuffer rw c d -> [AnySPBatch rw c d] -> FBBatch rw c d
framebufferBatch = FBBatch

--------------------------------------------------------------------------------
-- Shader program batch --------------------------------------------------------

data SPBatch rw c d u v = SPBatch {
    spBatchShaderProgram :: Program
  , spBatchUniform       :: U u
  , spBatchUniformValue  :: u
  , spBatchGeometries    :: [RenderCmd rw c d v Geometry]
  }

data AnySPBatch rw c d = forall u v. AnySPBatch (SPBatch rw c d u v)

anySPBatch :: SPBatch rw c d u v -> AnySPBatch rw c d
anySPBatch = AnySPBatch

runSPBatch :: (MonadIO m) => SPBatch rw c d u v -> m ()
runSPBatch (SPBatch prog uni u geometries) = do
  liftIO $ do
    glUseProgram (programID prog)
    runU uni u
  traverse_ drawGeometry geometries

shaderProgramBatch :: Program -> U u -> u -> [RenderCmd rw c d v Geometry] -> SPBatch rw c d u v
shaderProgramBatch = SPBatch

--------------------------------------------------------------------------------
-- Geometry draw function ------------------------------------------------------

drawGeometry :: (MonadIO m) => RenderCmd rw c d u Geometry -> m ()
drawGeometry (RenderCmd blending depthTest uni u geometry) = do
  setBlending blending
  (if depthTest then glEnable else glDisable) GL_DEPTH_TEST
  liftIO (runU uni u)
  case geometry of
    DirectGeometry (VertexArray vid mode vbNb) -> do
      glBindVertexArray vid
      glDrawArrays mode 0 vbNb
    IndexedGeometry (VertexArray vid mode ixNb) -> do
      glBindVertexArray vid
      glDrawElements mode ixNb GL_UNSIGNED_INT nullPtr
