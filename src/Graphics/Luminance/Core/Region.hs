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

module Graphics.Luminance.Core.Region where

import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans ( MonadTrans(..) )
import Control.Some ( Some(..) )
import Data.Bits
import Foreign.Ptr ( nullPtr )
import Graphics.GL
import Graphics.Luminance.Core.Blending ( setBlending )
import Graphics.Luminance.Core.Debug
import Graphics.Luminance.Core.Framebuffer ( Framebuffer(..) )
import Graphics.Luminance.Core.Geometry ( Geometry(..), VertexArray(..) )
import Graphics.Luminance.Core.Shader.Program ( Program(..), U', updateUniforms )
import Graphics.Luminance.Core.RenderCmd ( RenderCmd(..) )

-- |A 'Region' is a monad transformer used to create relationships between two monadic layers
-- and ensure GPU safety.
newtype Region r m a = Region { runRegion :: m a } deriving (Applicative,Functor,Monad)

-- |The /GPU/ main 'Region'. This 'Region' is the highest and more general you can find. You’ll need
-- to enter it if you want to enter any /GPU/ specific regions.
gpuRegion :: Region () m a -> m a
gpuRegion = runRegion

-- |The 'Framebuffer' 'Region'. This 'Region' binds a 'Framebuffer' for all children regions.
newFrame :: (MonadIO m) => Framebuffer rw c d -> Region Framebuffer m a -> Region () m ()
newFrame fb fbRegion = do
  Region . debugGL $ glBindFramebuffer GL_DRAW_FRAMEBUFFER (fromIntegral $ framebufferID fb)
  Region . debugGL $ glClear $ GL_DEPTH_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT
  Region (runRegion fbRegion)
  pure ()

-- |The 'Program' 'Region'. This 'Region' binds a 'Program' for all children regions.
newShading :: (MonadIO m) => Program a -> (((a -> U') -> Region Program m ()) -> Region Program m b) -> Region Framebuffer m ()
newShading prog progRegion = do
  Region . debugGL $ glUseProgram (programID prog)
  Region $ runRegion (progRegion $ Region . updateUniforms prog)
  pure ()

-- |Draw the 'Geometry' held by a 'RenderCmd'.
drawGeometry :: (MonadIO m) => RenderCmd rw c d Geometry -> Region Program m ()
drawGeometry (RenderCmd blending depthTest geometry) = Region $ do
  setBlending blending
  (if depthTest then glEnable else glDisable) GL_DEPTH_TEST
  case geometry of
    DirectGeometry (VertexArray vid mode vbNb) -> do
      debugGL $ glBindVertexArray vid
      debugGL $ glDrawArrays mode 0 vbNb
    IndexedGeometry (VertexArray vid mode ixNb) -> do
      debugGL $ glBindVertexArray vid
      debugGL $ glDrawElements mode ixNb GL_UNSIGNED_INT nullPtr
