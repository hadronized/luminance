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
import Control.Monad.RWS ( RWS, ask, get, evalRWS, execRWS, put )
import Control.Monad.Trans.Resource ( MonadResource, register )
import Data.Bits ( (.|.) )
import Data.Word ( Word32 )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( peekArray, pokeArray )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr, castPtr, nullPtr, plusPtr )
import Foreign.Storable ( Storable(..) )
import Graphics.GL
import Graphics.Luminance.RW

newtype Buffer = Buffer { bufferID :: GLuint } deriving (Eq,Show)

mkBuffer :: (MonadIO m,MonadResource m)
         => GLbitfield
         -> Word32
         -> m (Buffer,Ptr ())
mkBuffer flags size = do
  (bid,mapped) <- liftIO . alloca $ \p -> do
    glGenBuffers 1 p
    bid <- peek p
    mapped <- createStorage bid flags size
    pure (bid,mapped)
  _ <- register . with bid $ glDeleteBuffers 1
  pure (Buffer bid,mapped)

createStorage :: GLuint -> GLbitfield -> Word32 -> IO (Ptr ())
createStorage bid flags size = do
    glBindBuffer universalTarget bid
    glBufferStorage universalTarget bytes nullPtr flags
    ptr <- glMapBufferRange universalTarget 0 bytes flags
    glBindBuffer universalTarget 0
    pure ptr
  where
    bytes = fromIntegral size

mkBufferWithRegions :: (MonadIO m,MonadResource m)
                    => GLbitfield
                    -> BuildRegion rw a
                    -> m a
mkBufferWithRegions flags buildRegions = do
    (_,mapped) <- mkBuffer flags bytes
    pure . fst $ evalRWS built mapped 0
  where
    built = runBuildRegion buildRegions
    (bytes,_) = execRWS built nullPtr 0

readBuffer :: (MonadIO m,MonadResource m,Readable r)
           => BuildRegion r a
           -> m a
readBuffer = mkBufferWithRegions $
  GL_MAP_READ_BIT .|. GL_MAP_PERSISTENT_BIT .|. GL_MAP_COHERENT_BIT

writeBuffer :: (MonadIO m,MonadResource m,Writable w)
            => BuildRegion w a
            -> m a
writeBuffer = mkBufferWithRegions $
  GL_MAP_WRITE_BIT .|. GL_MAP_PERSISTENT_BIT .|. GL_MAP_COHERENT_BIT

readWriteBuffer :: (MonadIO m,MonadResource m)
                => BuildRegion RW a
                -> m a
readWriteBuffer = mkBufferWithRegions $
  GL_MAP_READ_BIT .|. GL_MAP_WRITE_BIT .|. GL_MAP_PERSISTENT_BIT .|. GL_MAP_COHERENT_BIT

data Region rw a = Region {
    regionPtr :: Ptr a
  , regionSize :: RegionSize a
  } deriving (Eq,Show)

newtype RegionSize a = RegionSize Word32 deriving (Eq,Num,Show)

sizeOfR :: forall a. (Storable a) => RegionSize a -> Word32
sizeOfR (RegionSize size) = fromIntegral (sizeOf (undefined :: a)) * size

newtype BuildRegion rw a = BuildRegion {
    runBuildRegion :: RWS (Ptr ()) () Word32 a
  } deriving (Applicative,Functor,Monad)

newRegion :: forall rw a. (Storable a) => Word32 -> BuildRegion rw (Region rw a)
newRegion size = BuildRegion $ do
    offset <- get
    put $ offset + sizeOfR regionS
    ptr <- ask
    pure $ Region (castPtr $ ptr `plusPtr` fromIntegral offset) regionS
  where
    regionS :: RegionSize a
    regionS = RegionSize size

readWhole :: (MonadIO m,Readable r,Storable a) => Region r a -> m [a]
readWhole (Region p (RegionSize nb)) = liftIO $
  peekArray (fromIntegral nb) p

writeWhole :: (MonadIO m,Storable a,Writable w) => Region w a -> [a] -> m ()
writeWhole (Region p (RegionSize nb)) values =
  liftIO . pokeArray p $ take (fromIntegral nb) values

clear :: (MonadIO m,Storable a,Writable w) => Region w a -> a -> m ()
clear (Region p (RegionSize nb)) a =
  liftIO . pokeArray p $ replicate (fromIntegral nb) a

(@?) :: (MonadIO m,Storable a,Readable r) => Region r a -> Word32 -> m (Maybe a)
Region p (RegionSize nb) @? i
  | i >= nb = pure Nothing
  | otherwise = liftIO $ Just <$> peekElemOff p (fromIntegral i)

(@!) :: (MonadIO m,Storable a,Readable r) => Region r a -> Word32 -> m a
Region p _ @! i = liftIO $ peekElemOff p (fromIntegral i)

writeAt :: (MonadIO m,Storable a,Writable w) => Region w a -> Word32 -> a -> m ()
writeAt (Region p (RegionSize nb)) i a
  | i >= nb = pure ()
  | otherwise = liftIO $ pokeElemOff p (fromIntegral i) a

writeAt' :: (MonadIO m,Storable a,Writable w) => Region w a -> Word32 -> a -> m ()
writeAt' (Region p _) i a = liftIO $ pokeElemOff p (fromIntegral i) a

universalTarget :: GLenum
universalTarget = GL_COPY_WRITE_BUFFER
