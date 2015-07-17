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

import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.State ( State, get, put, runState )
import Data.Bits ( (.|.) )
import Data.Word ( Word8, Word32 )
import Foreign.Ptr ( castPtr, plusPtr )
import Foreign.Storable ( sizeOf )
import Graphics.GL
import Graphics.Luminance.Memory

newtype Buffer rw = Buffer { bufferID :: GC GLuint }

type ReadBuffer = Buffer ReadOnly
type WriteBuffer = Buffer WriteOnly

data ReadOnly = ReadOnly deriving (Eq,Ord,Show)

data WriteOnly = WriteOnly deriving (Eq,Ord,Show)

{-
readBuffer :: (MonadIO m)
           => GLenum
           -> Word32
-}
mkBuffer :: (MonadIO m)
         => rw
         -> GLenum
         -> GLbitfield
         -> Word32
         -> m (Buffer rw)
mkBuffer _ target flags size = liftIO $ do
  p <- malloc
  glGenBuffers 1 p
  peek p >>= \bid -> do
    createStorage bid target flags size
    Buffer <$> embedGC bid (glDeleteBuffers 1 p)

mkBufferWithRegions :: (MonadIO m)
                    => rw
                    -> GLenum
                    -> GLbitfield
                    -> BuildRegion a
                    -> m (Buffer rw,a)
mkBufferWithRegions rw target flags buildRegions =
    (,)
      <$> mkBuffer rw target flags bytes
      <*> pure a
  where
    (a,bytes) = runState (runBuildRegion buildRegions) 0

createStorage :: GLuint -> GLenum -> GLbitfield -> Word32 -> IO ()
createStorage bid target flags size = do
  glBindBuffer target bid
  glBufferStorage target (fromIntegral size) nullPtr flags
  glBindBuffer target 0

data Region a = Region {
    regionPtr :: Ptr a
  , regionSize :: RegionSize a
  } deriving (Eq,Show)

newtype RegionSize a = RegionSize Word32 deriving (Eq,Show)

sizeOfR :: forall a. (Storable a) => RegionSize a -> Word32
sizeOfR (RegionSize size) = fromIntegral (sizeOf (undefined :: a)) * size

newtype BuildRegion a = BuildRegion { runBuildRegion :: State Word32 a }
  deriving (Applicative,Functor,Monad)

newRegion :: forall a. (Storable a) => Word32 -> BuildRegion (Region a)
newRegion size = BuildRegion $ do
    offset <- get
    put $ offset + sizeOfR sizeR
    pure $ Region (nullPtr `plusPtr` fromIntegral offset) sizeR
  where
    sizeR :: RegionSize a
    sizeR = RegionSize size
