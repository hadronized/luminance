-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Cmd where

import Graphics.Luminance.Batch
import Graphics.Luminance.Framebuffer
import Graphics.Luminance.RW ( Readable, Writable )
import Graphics.Luminance.Texture ( Filter )
import Numeric.Natural ( Natural )

-- |Command type. Used to accumulate GPU commands. Use 'runCmd' to execute
-- the whole chain of commands.
newtype Cmd a = Cmd { runCmd :: IO a } deriving (Applicative,Functor,Monad)

-- |Draw a framebuffer batch and return the framebuffer’s output.
draw :: FBBatch rw c d -> Cmd (Output c d)
draw fbb = Cmd $ do
  runFBBatch fbb
  pure . framebufferOutput $ fbBatchFramebuffer fbb

-- |Blit a framebuffer batch onto another and return the framebuffer’s output of the write
-- framebuffer.
blit :: (Readable r,Writable w)
     => FBBatch r c d
     -> FBBatch w c d
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
     -> Cmd (Output c d)
blit r w rx ry rwidth rheight wx wy wwidth wheight mask flt = do
  _ <- draw r
  _ <- draw w
  Cmd $ do
    framebufferBlit (fbBatchFramebuffer r) (fbBatchFramebuffer w) rx ry rwidth rheight wx wy wwidth wheight mask flt
    pure . framebufferOutput $ fbBatchFramebuffer w
