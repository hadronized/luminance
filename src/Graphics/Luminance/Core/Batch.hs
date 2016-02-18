{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015, 2016 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Core.Batch where

import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans ( MonadTrans(..) )
import Control.Some ( Some(..) )
import Data.Bits
import Foreign.Ptr ( nullPtr )
import GHC.Exts ( Constraint )
import Graphics.GL
import Graphics.Luminance.Core.Blending ( setBlending )
import Graphics.Luminance.Core.Debug
import Graphics.Luminance.Core.Framebuffer ( Framebuffer(..) )
import Graphics.Luminance.Core.Geometry ( Geometry(..), VertexArray(..) )
import Graphics.Luminance.Core.Shader.Program ( Program(..), U(..) )
import Graphics.Luminance.Core.RenderCmd ( RenderCmd(..) )

newtype Region r m a = Region { runRegion :: m a} deriving (Applicative,Functor,Monad,MonadIO)

instance MonadTrans (Region r) where
  lift = Region

--------------------------------------------------------------------------------
-- Regions ---------------------------------------------------------------------

newFrame :: (MonadIO m) => Framebuffer rw c d -> Region Framebuffer m a -> m a
newFrame fb fbRegion = do
  liftIO . debugGL $ glBindFramebuffer GL_DRAW_FRAMEBUFFER (fromIntegral $ framebufferID fb)
  liftIO . debugGL $ glClear $ GL_DEPTH_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT
  runRegion fbRegion

newShading :: (MonadIO m) => Some Program -> Region Program m a -> Region Framebuffer m a
newShading (Some prog) progRegion = do
  liftIO . debugGL $ glUseProgram (programID prog)
  lift (runRegion progRegion)

-- Draw the 'Geometry' held by a 'RenderCmd'.
drawGeometry :: RenderCmd rw c d Geometry -> IO ()
drawGeometry (RenderCmd blending depthTest geometry) = do
  setBlending blending
  (if depthTest then glEnable else glDisable) GL_DEPTH_TEST
  case geometry of
    DirectGeometry (VertexArray vid mode vbNb) -> do
      debugGL $ glBindVertexArray vid
      debugGL $ glDrawArrays mode 0 vbNb
    IndexedGeometry (VertexArray vid mode ixNb) -> do
      debugGL $ glBindVertexArray vid
      debugGL $ glDrawElements mode ixNb GL_UNSIGNED_INT nullPtr
