{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015, 2016 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-----------------------------------------------------------------------------

module Graphics.Luminance.FramebufferDriver where

import Control.Monad.Except ( MonadError )
import GHC.Exts ( Constraint )
import Graphics.Luminance.Framebuffer ( FramebufferBlitMask, HasFramebufferError )
import Graphics.Luminance.RW ( Readable, RW, Writable )
import Graphics.Luminance.Texture ( Filter )
import Numeric.Natural ( Natural )

-- |A driver to implement to provide pixel format features.
class (Monad m) => FramebufferDriver m where
  -- |A 'Framebuffer' represents two buffers: a /color/ buffer and /depth/ buffer.
  -- You can select which one you want and specify the formats to use by providing 'Pixel'
  -- types. If you want to mute a buffer, use '()'.
  type Framebuffer m :: * -> * -> * -> *
  -- |All possible framebuffer color attachments.
  type FramebufferColorAttachment m :: * -> Constraint
  -- |All possible framebuffer depth attachments.
  type FramebufferDepthAttachment m :: * -> Constraint
  -- |@'createFramebuffer' w h mipmaps@ creates a new 'Framebuffer' with dimension @w * h@ and
  -- allocating spaces for @mipmaps@ level of textures. The textures are created by providing a
  -- correct type.
  --
  -- For the color part, you can pass either:
  --
  -- - '()': that will mute the color buffer of the framebuffer;
  -- - @'Format' t c@: that will create a single texture with the wished color format;
  -- - or @a ':.' b@: that will create a chain of textures; 'a' and 'b' cannot be '()'.
  --
  -- For the depth part, you can pass either:
  --
  -- - '()': that will mute the depth buffer of the framebuffer;
  -- - @'Format' t c@: that will create a single texture with the wished depth format.
  --
  -- Finally, the @rw@ parameter can be set to 'R', 'W' or 'RW' to specify which kind of framebuffer
  -- access you’ll need.
  createFramebuffer  :: (FramebufferColorAttachment m c,FramebufferDepthAttachment m d,HasFramebufferError e,MonadError e m)
                     => Natural
                     -> Natural
                     -> Natural
                     -> m (Framebuffer m rw c d)
  -- |The default 'Framebuffer' represents the screen (back buffer with double buffering).
  defaultFramebuffer :: m (Framebuffer m RW () ())
  -- Blit two framebuffers.
  framebufferBlit    :: (Readable r,Writable w)
                     => Framebuffer m r c d
                     -> Framebuffer m w c' d'
                     -> Int
                     -> Int
                     -> Natural
                     -> Natural
                     -> Int
                     -> Int
                     -> Natural
                     -> Natural
                     -> FramebufferBlitMask
                     -> Filter
                     -> m ()
