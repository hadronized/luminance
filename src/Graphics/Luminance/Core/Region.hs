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
import Graphics.Luminance.Core.Shader.Program ( Program(..), )
import Graphics.Luminance.Core.RenderCmd ( RenderCmd(..) )

-- |A 'Region' is a monad transformer used to create relationships between two monadic layers
-- and ensure GPU safety.
newtype Region r m a = Region { runRegion :: m a } deriving (Applicative,Functor,Monad,MonadIO)

instance MonadTrans (Region r) where
  lift = Region

-- |The /GPU/ main 'Region'. This 'Region' is the highest and more general you can find. You’ll need
-- to enter it if you want to enter any /GPU/ specific regions.
gpuRegion :: Region () m a -> m a
gpuRegion = runRegion

-- |The 'Framebuffer' 'Region'. This 'Region' binds a 'Framebuffer' for all children regions.
newFrame :: (MonadIO m) => Framebuffer rw c d -> Region Framebuffer m a -> Region () m a
newFrame fb fbRegion = do
  liftIO . debugGL $ glBindFramebuffer GL_DRAW_FRAMEBUFFER (fromIntegral $ framebufferID fb)
  liftIO . debugGL $ glClear $ GL_DEPTH_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT
  lift (runRegion fbRegion)

-- |The 'Program' 'Region'. This 'Region' binds a 'Program' for all children regions.
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
