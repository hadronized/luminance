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
import Graphics.Luminance.Framebuffer ( Framebuffer(..) )
import Graphics.Luminance.Geometry ( Geometry(..), VertexArray(..) )
import Graphics.Luminance.Shader.Program ( Program(..) )
import Graphics.Luminance.Shader.Uniform ( Uniformed(..) )

-- FIXME: TEST ONLY
import Data.Bits

data FBBatch rw c d = FBBatch {
    fbBatchFramebuffer :: Framebuffer rw c d
  , fbBatchSPBatch     :: [SPBatch]
  }

data SPBatch = SPBatch {
    spBatchShaderProgram :: Program
  , spBatchUniformed     :: Uniformed ()
  , spBatchGeometries    :: [Uniformed Geometry]
  }

drawGeometry :: (MonadIO m) => Uniformed Geometry -> m ()
drawGeometry g = do
  geometry <- liftIO (runUniformed g)
  case geometry of
    DirectGeometry (VertexArray vid mode vbNb) -> do
      glBindVertexArray vid
      glDrawArrays mode 0 vbNb
    IndexedGeometry (VertexArray vid mode ixNb) -> do
      glBindVertexArray vid
      glDrawElements mode ixNb GL_UNSIGNED_INT nullPtr

treatSPBatch :: (MonadIO m) => SPBatch -> m ()
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
