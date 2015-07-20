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
import Data.Word ( Word32 )
import Foreign.Marshal.Array ( peekArray, pokeArray )
import Foreign.Ptr ( plusPtr )
import Foreign.Storable ( Storable(..) )
import Graphics.GL
import Graphics.Luminance.Memory

newtype Buffer rw = Buffer { bufferID :: GC GLuint }

type ReadBuffer = Buffer ReadOnly
type WriteBuffer = Buffer WriteOnly

data ReadOnly = ReadOnly deriving (Eq,Ord,Show)

data WriteOnly = WriteOnly deriving (Eq,Ord,Show)

readBuffer :: (MonadIO m)
           => BuildRegion a
           -> m (ReadBuffer,a)
readBuffer = mkBufferWithRegions $
  GL_MAP_READ_BIT .|. GL_MAP_PERSISTENT_BIT .|. GL_MAP_COHERENT_BIT

writeBuffer :: (MonadIO m)
            => BuildRegion a
            -> m (WriteBuffer,a)
writeBuffer = mkBufferWithRegions $
  GL_MAP_WRITE_BIT .|. GL_MAP_PERSISTENT_BIT .|. GL_MAP_COHERENT_BIT

mkBuffer :: (MonadIO m)
         => GLbitfield
         -> Word32
         -> m (Buffer rw)
mkBuffer flags size = liftIO $ do
  p <- malloc
  glGenBuffers 1 p
  peek p >>= \bid -> do
    createStorage bid flags size
    Buffer <$> embedGC bid (glDeleteBuffers 1 p)

mkBufferWithRegions :: (MonadIO m)
                    => GLbitfield
                    -> BuildRegion a
                    -> m (Buffer rw,a)
mkBufferWithRegions flags buildRegions =
    (,)
      <$> mkBuffer flags bytes
      <*> pure a
  where
    (a,bytes) = runState (runBuildRegion buildRegions) 0

createStorage :: GLuint -> GLbitfield -> Word32 -> IO ()
createStorage bid flags size = do
  glBindBuffer universalTarget bid
  glBufferStorage universalTarget (fromIntegral size) nullPtr flags
  glBindBuffer universalTarget 0

universalTarget :: GLenum
universalTarget = GL_COPY_WRITE_BUFFER

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
    put $ offset + sizeOfR regionS
    pure $ Region (nullPtr `plusPtr` fromIntegral offset) regionS
  where
    regionS :: RegionSize a
    regionS = RegionSize size

readWhole :: (MonadIO m,Storable a) => Region a -> m [a]
readWhole (Region p (RegionSize nb)) = liftIO $
  peekArray (fromIntegral nb) p

writeWhole :: (MonadIO m,Storable a) => Region a -> [a] -> m ()
writeWhole (Region p (RegionSize nb)) values =
  liftIO . pokeArray p $ take (fromIntegral nb) values

clear :: (MonadIO m,Storable a) => Region a -> a -> m ()
clear (Region p (RegionSize nb)) a =
  liftIO . pokeArray p $ replicate (fromIntegral nb) a

(!?) :: (MonadIO m,Storable a) => Region a -> Word32 -> m (Maybe a)
Region p (RegionSize nb) !? i
  | i >= nb = pure Nothing
  | otherwise = liftIO $ Just <$> peekElemOff p (fromIntegral i)

(!) :: (MonadIO m,Storable a) => Region a -> Word32 -> m (Maybe a)
Region p _ ! i = liftIO $ Just <$> peekElemOff p (fromIntegral i)

writeAt :: (MonadIO m,Storable a) => Region a -> Word32 -> a -> m ()
writeAt (Region p (RegionSize nb)) i a
  | i >= nb = pure ()
  | otherwise = liftIO $ pokeElemOff p (fromIntegral i) a

writeAt' :: (MonadIO m,Storable a) => Region a -> Word32 -> a -> m ()
writeAt' (Region p _) i a = liftIO $ pokeElemOff p (fromIntegral i) a
