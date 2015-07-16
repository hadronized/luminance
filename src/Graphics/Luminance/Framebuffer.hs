-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Framebuffer where

import Graphics.Luminance.Memory ( GC )

newtype Framebuffer = Framebuffer { framebufferID :: GC GLuint }

-- TODO: think about the semantics of framebuffer and the relationship between
-- framebuffers and textures
