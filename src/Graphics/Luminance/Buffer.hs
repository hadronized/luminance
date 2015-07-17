-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Buffer where

import Control.Monad.IO.Class ( MonadIO )
import Data.Word ( Word32 )
import Graphics.GL
import Graphics.Luminance.Memory

newtype Buffer a = Buffer { bufferID :: GC GLuint }

-- WRONG! We cannot call glDeleteBuffers that way…
mkBuffers :: (MonadIO m)
          => GLenum
          -> [Word32]
          -> m [Buffer a]
mkBuffers target sizes = liftIO $ do
    p <- mallocArray nb
    glGenBuffers nb p
    buffers <- peekArray p nb
    traverse (createStorageNBuffer p) $ zip buffers sizes
  where
    nb = length sizes
    createStorageNBuffer p (bid,size) = do
      createStorage bid target size
      Buffer <$> embedGC bid (glDeleteBuffers nb p)

createStorage :: GLuint -> GLenum -> Word32 -> IO ()
createStorage bid target size = do
  glBindBuffer target bid
  glBufferStorage target size nullPtr flags
  glBindBuffer target 0
