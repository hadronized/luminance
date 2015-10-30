{-# LANGUAGE ExistentialQuantification #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Core.Batch where

import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Bits
import Data.Foldable ( traverse_ )
import Foreign.Ptr ( nullPtr )
import Graphics.GL
import Graphics.Luminance.Core.Blending ( setBlending )
import Graphics.Luminance.Core.Framebuffer ( Framebuffer(..) )
import Graphics.Luminance.Core.Geometry ( Geometry(..), VertexArray(..) )
import Graphics.Luminance.Core.Shader.Program ( Program(..) )
import Graphics.Luminance.Core.RenderCmd ( RenderCmd(..) )
import Graphics.Luminance.Core.Shader.Uniform ( U(..) )

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
  liftIO $ glClear $ GL_DEPTH_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT
  traverse_ (\(AnySPBatch spb) -> runSPBatch spb) spbs

-- |Share a 'Framebuffer' between several shader program batches.
framebufferBatch :: Framebuffer rw c d -> [AnySPBatch rw c d] -> FBBatch rw c d
framebufferBatch = FBBatch

--------------------------------------------------------------------------------
-- Shader program batch --------------------------------------------------------

-- |Shader 'Program' batch.
--
-- Such a batch is used to share a 'Program' between several 'RenderCmd'. It also
-- gathers a uniform @'U' u@ and a 'u' value to send to the uniform.
--
-- The 'u' type can be used to send uniforms for the whole batch. It can be useful
-- for cold values – that won’t change very often for a given frame – like the resolution of the
-- screen, the mouse cursor coordinates, the time, and so on and so forth.
--
-- The 'v' type variable is used to add uniforms per-'RenderCmd'.
data SPBatch rw c d u v = SPBatch {
    spBatchShaderProgram :: Program
  , spBatchUniform       :: U u
  , spBatchUniformValue  :: u
  , spBatchGeometries    :: [RenderCmd rw c d v Geometry]
  }

-- |Abstract 'SPBatch' over uniform interface.
data AnySPBatch rw c d = forall u v. AnySPBatch (SPBatch rw c d u v)

-- FIXME: should we call this function 'abstractSPBatch'?
-- |Abstract 'SPBatch'.
anySPBatch :: SPBatch rw c d u v -> AnySPBatch rw c d
anySPBatch = AnySPBatch

-- Run a 'SPBatch' in 'MonadIO'.
runSPBatch :: (MonadIO m) => SPBatch rw c d u v -> m ()
runSPBatch (SPBatch prog uni u geometries) = do
  liftIO $ do
    glUseProgram (programID prog)
    runU uni u
  traverse_ drawGeometry geometries

-- |Create a new 'SPBatch'.
shaderProgramBatch :: Program -> U u -> u -> [RenderCmd rw c d v Geometry] -> SPBatch rw c d u v
shaderProgramBatch = SPBatch

-- |Create a new 'SPBatch' with no uniform interface.
shaderProgramBatch_ :: Program -> [RenderCmd rw c d v Geometry] -> SPBatch rw c d () v
shaderProgramBatch_ p = SPBatch p mempty ()

--------------------------------------------------------------------------------
-- Geometry draw function ------------------------------------------------------

-- Draw the 'Geometry' held by a 'RenderCmd'.
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
