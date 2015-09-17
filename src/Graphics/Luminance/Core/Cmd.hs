-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Core.Cmd where

import Control.Monad.IO.Class ( MonadIO(..) )
import Graphics.Luminance.Core.Batch
import Graphics.Luminance.Core.Framebuffer
import Graphics.Luminance.Core.RW ( Readable, Writable )
import Graphics.Luminance.Core.Texture ( Filter )
import Numeric.Natural ( Natural )

-- |Command type. Used to accumulate GPU commands. Use 'runCmd' to execute
-- the whole chain of commands.
newtype Cmd a = Cmd (IO a) deriving (Applicative,Functor,Monad)

runCmd :: (MonadIO m) => Cmd a -> m a
runCmd (Cmd a) = liftIO a

-- |Draw a framebuffer batch and return the framebuffer’s output.
draw :: FBBatch rw c d -> Cmd (Output c d)
draw fbb = Cmd $ do
  runFBBatch fbb
  pure . framebufferOutput $ fbBatchFramebuffer fbb

-- |Blit a framebuffer batch onto a framebuffer and return the framebuffer’s output of the write
-- framebuffer.
blit :: (Readable r,Writable w)
     => Framebuffer r c0 d0
     -> Framebuffer w c1 d1
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
     -> Cmd (Output c1 d1)
blit r w rx ry rwidth rheight wx wy wwidth wheight mask flt = Cmd $ do
  framebufferBlit r w rx ry rwidth rheight wx wy wwidth wheight mask flt
  pure (framebufferOutput w)
