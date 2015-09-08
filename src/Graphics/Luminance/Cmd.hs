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

data Cmd a = Cmd { runCmd :: IO a } deriving (Applicative,Functor,Monad)

draw :: FBBatch rw c d -> Cmd (Output c d)
draw = Cmd . runFBBatch

blit :: FBBatch r c d -> FBBatch w c d -> Cmd (Output c d)
blit r w = Cmd (framebufferBlit r w)
