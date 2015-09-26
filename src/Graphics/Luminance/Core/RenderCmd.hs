-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Core.RenderCmd where

import Graphics.Luminance.Core.Blending
import Graphics.Luminance.Core.Shader.Uniform ( U(..) )

-- FIXME: we need to make a tighter link between c and blending and between d and the depth test.
-- FIXME: is the 'rw' type parameter still useful?

-- |A /GPU/ render command. That type exists to implement a stateless way to issue draw commands
-- to the /GPU/. You can set several hints for a given draw command:
--
--   - /blending/: the blending mode is represented by
--     @'Maybe' ('BlendingMode','BlendingFactor','BlendingFactor')@. If you pass
--     'Nothing', /blending/ is disabled for that draw command. If you want to enable it, you have
--     to pass @'Just' (mode,srcK,dstK)@, where @mode@ is the 'BlendingMode' and @srcK@ and @dstK@
--     are both 'BlendingFactor' representing the source and destination factors.
--   - /depth test/: the depth test can be enabled by passing 'True' and disabled with 'False'.
--   - a /per draw command uniform interface and uniform value/: that is the way you can customize
--     the draw calls. You just have to pass the uniform interface and the value to send down to the
--     shader.
--
-- Finally, a 'RenderCmd' holds a value. That value will be consumed later by other functions. In
-- general, it’ll be 'Geometry'.
data RenderCmd rw c d u a = RenderCmd (Maybe (BlendingMode,BlendingFactor,BlendingFactor)) Bool (U u) u a

instance Functor (RenderCmd rw c d u) where
  fmap f (RenderCmd blending depthTest uni u a) = RenderCmd blending depthTest uni u (f a)

-- |@'renderCmd' blending depthTest uniformInterface u a@ constructs a new 'RenderCmd'.
renderCmd :: Maybe (BlendingMode,BlendingFactor,BlendingFactor)
          -> Bool
          -> U u
          -> u 
          -> a 
          -> RenderCmd rw c d u a
renderCmd = RenderCmd

-- |A standard 'RenderCmd' builder.
--
--   - no /blending/
--   - /depth test/ enabled
stdRenderCmd :: U u -> u -> a -> RenderCmd rw c d u a
stdRenderCmd = RenderCmd Nothing True

-- |A standard 'RenderCmd' builder with no uniform interface.
--
--   - no /blending/
--   - /depth test/ enabled
stdRenderCmd_ :: a -> RenderCmd rw c d () a
stdRenderCmd_ = stdRenderCmd mempty ()
