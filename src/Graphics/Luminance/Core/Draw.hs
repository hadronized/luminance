-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015, 2016 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Core.Draw where

import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Bits ( (.|.) )
import Data.Foldable ( traverse_ )
import Foreign.Ptr ( nullPtr )
import Graphics.GL
import Graphics.Luminance.Core.Blending ( setBlending )
import Graphics.Luminance.Core.Debug ( debugGL )
import Graphics.Luminance.Core.Framebuffer ( Framebuffer(..), Output )
import Graphics.Luminance.Core.Geometry ( Geometry(..), VertexArray(..) )
import Graphics.Luminance.Core.RW ( Writable )
import Graphics.Luminance.Core.RenderCmd ( RenderCmd(..) )
import Graphics.Luminance.Core.Shader.Program ( Program(..), U'(..) )

-- |Issue a draw command to the GPU. Don’t be afraid of the type signature. Let’s explain it.
--
-- The first parameter is the framebuffer you want to perform the rendering in. It must be
-- writable.
--
-- The second parameter is a list of /shading commands/. A shading command is composed of three
-- parts:
--
-- * a 'Program' used for shading;
-- * a @(a -> 'U'')@ uniform sink used to update uniforms in the program passed as first value;
--   this is useful if you want to update uniforms only once per draw or for all render
--   commands, like time, user event, etc.;
-- * a list of /render commands/ function; that function enables you to update uniforms via the
--   @(a -> 'U'')@ uniform sink for each render command that follows.
--
-- This function yields a value of type @'Output' m c d'@, which represents the output of the render
-- – typically, textures or '()'.
draw :: (MonadIO m,Writable w)
     => Framebuffer w c d
     -> [(Program a,a -> U',[a -> (U',RenderCmd w c d Geometry)])]
     -> m (Output c d)
draw fb shcmds = do
  debugGL $ glBindFramebuffer GL_DRAW_FRAMEBUFFER (fromIntegral $ framebufferID fb)
  debugGL $ glClear $ GL_DEPTH_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT
  traverse_ (\(p,u,f) -> shade p u f) shcmds
  pure (framebufferOutput fb)

shade :: (MonadIO m) => Program a -> (a -> U') -> [a -> (U',RenderCmd w c d Geometry)] -> m ()
shade prog setUniforms rdrfuns = do
    debugGL $ glUseProgram (programID prog)
    liftIO . runU' $ setUniforms iface
    traverse_ (\f -> uncurry render $ f iface) rdrfuns
  where
    iface = programInterface prog

render :: (MonadIO m) => U' -> RenderCmd w c d Geometry -> m ()
render u (RenderCmd blending depthTest geometry)= do
  liftIO (runU' u)
  setBlending blending
  (if depthTest then glEnable else glDisable) GL_DEPTH_TEST
  case geometry of
    DirectGeometry (VertexArray vid mode vbNb) -> do
      debugGL $ glBindVertexArray vid
      debugGL $ glDrawArrays mode 0 vbNb
    IndexedGeometry (VertexArray vid mode ixNb) -> do
      debugGL $ glBindVertexArray vid
      debugGL $ glDrawElements mode ixNb GL_UNSIGNED_INT nullPtr

