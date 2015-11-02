{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Core.Buffer where

import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.RWS ( RWS, ask, get, evalRWS, execRWS, put )
import Control.Monad.Trans.Resource ( MonadResource, register )
import Data.Bits ( (.|.) )
import Data.Foldable ( toList )
import Data.Proxy ( Proxy(..) )
import Data.Word ( Word32 )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( peekArray, pokeArray )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr, castPtr, nullPtr )
#ifdef __GL45
import Foreign.Ptr ( plusPtr )
#endif
import Foreign.Storable ( Storable(..) )
import Graphics.GL
import Graphics.Luminance.Core.RW

-- |A 'Buffer' is an opaque and untyped region of abstract GPU memory. You cannot do much with it
-- and you might even not see the type in the user interface as it’s not really needed. It’s shown
-- for informational purposes only.
newtype Buffer = Buffer { bufferID :: GLuint } deriving (Eq,Show)

-- Create a new 'Buffer' and return its GPU address by mapping it to a @Ptr ()@.
mkBuffer :: (MonadIO m,MonadResource m)
         => GLbitfield
         -> Int
         -> m (Buffer,Ptr ())
#ifdef __GL45
mkBuffer flags size = do
  (bid,mapped) <- liftIO . alloca $ \p -> do
    glCreateBuffers 1 p
    bid <- peek p
    mapped <- createStorage bid flags size
    pure (bid,mapped)
  _ <- register . with bid $ glDeleteBuffers 1
  pure (Buffer bid,mapped)
#elif defined(__GL32)
mkBuffer flags size = do
  (bid,mapped) <- liftIO . alloca $ \p -> do
    glGenBuffers 1 p
    bid <- peek p
    mapped <- createStorage bid flags size
    pure (bid,mapped)
  _ <- register . with bid $ glDeleteBuffers 1
  pure (Buffer bid,mapped)
#endif

-- Create the required OpenGL storage for a 'Buffer'.
createStorage :: GLuint -> GLbitfield -> Int -> IO (Ptr ())
#ifdef __GL45
createStorage bid flags size = do
    glNamedBufferStorage bid bytes nullPtr flags
    ptr <- glMapNamedBufferRange bid 0 bytes flags
    pure ptr
  where
    bytes = fromIntegral size
#elif defined(__GL32)
createStorage bid _ size = do
    glBindBuffer GL_ARRAY_BUFFER bid
    glBufferData GL_ARRAY_BUFFER bytes nullPtr GL_STREAM_DRAW
    pure nullPtr
  where
    bytes = fromIntegral size
#endif

-- |Create a new 'Buffer' using regions through the 'BuildRegion' monadic type. The 'Buffer' is
-- returned as well as the computed 'a' value  in the 'BuildRegion'.
--
-- Typically, the user will wrap the 'Region' in the 'a' type.
mkBufferWithRegions :: (MonadIO m,MonadResource m)
                    => GLbitfield
                    -> BuildRegion rw a
                    -> m (a,Buffer)
mkBufferWithRegions flags buildRegions = do
    (buffer,mapped) <- mkBuffer flags bytes
    pure (fst $ evalRWS built (buffer,mapped) 0,buffer)
  where
    built = runBuildRegion buildRegions
    (bytes,_) = execRWS built (Buffer 0,nullPtr) 0

-- |'Buffer'’s 'Region's can have reads and writes. That typeclass makes implements all possible
-- cases.
class BufferRW rw where
  bufferFlagsFromRW :: proxy rw -> GLenum

instance BufferRW R where
  bufferFlagsFromRW _ = GL_MAP_READ_BIT

instance BufferRW RW where
  bufferFlagsFromRW _ = GL_MAP_READ_BIT .|. GL_MAP_WRITE_BIT

instance BufferRW W where
  bufferFlagsFromRW _ = GL_MAP_WRITE_BIT

-- |Create a new 'Buffer' and expose 'Region's. Through the 'BuildRegion' type, you can yield new
-- regions and embed them in the type of your choice. The function returns that type.
createBuffer :: forall a m rw. (BufferRW rw,MonadIO m,MonadResource m)
             => BuildRegion rw a
             -> m a
createBuffer = fmap fst . mkBufferWithRegions (bufferFlagsFromRW (Proxy :: Proxy rw) .|. persistentCoherentBits)

-- Special case of 'createBuffer', returning the 'Buffer' in addition for internal uses.
createBuffer_ :: forall a m rw. (BufferRW rw,MonadIO m,MonadResource m)
              => BuildRegion rw a
              -> m (a,Buffer)
createBuffer_ = mkBufferWithRegions $
  bufferFlagsFromRW (Proxy :: Proxy rw) .|. persistentCoherentBits

persistentCoherentBits :: GLbitfield
#ifdef __GL45
persistentCoherentBits = GL_MAP_PERSISTENT_BIT .|. GL_MAP_COHERENT_BIT
#elif defined(__GL32)
persistentCoherentBits = 0
#endif

-- |A 'Region' is a GPU typed memory area. It can be pictured as a GPU array.
#ifdef __GL45
data Region rw a = Region {
    regionPtr :: Ptr a -- mapped pointer (into GPU memory)
  , regionOffset :: Int
  , regionSize :: Int -- number of elements living in that region
  , regionBuffer :: Buffer -- buffer the region lays in
  } deriving (Eq,Show)
#elif defined(__GL32)
-- OpenGL 3.2 doesn’t have any support for persistent/coherent buffers, so we don’t need to store
-- the pointer, as we will ask for it each time we want to do something with the buffer.
data Region rw a = Region {
    regionOffset :: Int
  , regionSize :: Int -- number of elements living in that region
  , regionBuffer :: Buffer -- buffer the region lays in
  } deriving (Eq,Show)
#endif

-- |Convenient type to build 'Region's.
newtype BuildRegion rw a = BuildRegion {
    runBuildRegion :: RWS (Buffer,Ptr ()) () Int a
  } deriving (Applicative,Functor,Monad)

-- |Create a new 'Region' by providing the number of wished elements.
newRegion :: forall rw a. (Storable a) => Word32 -> BuildRegion rw (Region rw a)
newRegion size = BuildRegion $ do
    offset <- get
    put $ offset + fromIntegral size * sizeOf (undefined :: a)
#ifdef __GL45
    (buffer,ptr) <- ask
    pure $ Region {
        regionPtr = (castPtr $ ptr `plusPtr` fromIntegral offset)
      , regionOffset = offset
      , regionSize = fromIntegral size
      , regionBuffer = buffer
      }
#elif defined(__GL32)
    (buffer,_) <- ask
    pure $ Region {
        regionOffset = offset
      , regionSize = fromIntegral size
      , regionBuffer = buffer
      }
#endif

-- |Read a whole 'Region'.
readWhole :: (MonadIO m,Readable r,Storable a) => Region r a -> m [a]
#ifdef __GL45
readWhole r = liftIO $ peekArray (regionSize r) (regionPtr r)
#elif defined(__GL32)
readWhole r = liftIO $ do
  glBindBuffer GL_ARRAY_BUFFER (bufferID $ regionBuffer r)
  p <- glMapBufferRange GL_ARRAY_BUFFER (fromIntegral $ regionOffset r) (fromIntegral $ regionSize r) GL_MAP_READ_BIT
  a <- peekArray (regionSize r) (castPtr p)
  _ <- glUnmapBuffer GL_ARRAY_BUFFER
  pure a
#endif

-- |Write the whole 'Region'. If value are missing, only the provided values will replace the
-- existing ones. If there are more values than the size of the 'Region', they are ignored.
writeWhole :: (Foldable f,MonadIO m,Storable a,Writable w)
           => Region w a
           -> f a
           -> m ()
#ifdef __GL45
writeWhole r values = liftIO . pokeArray (regionPtr r) . take (regionSize r) $ toList values
#elif defined(__GL32)
writeWhole r values = liftIO $ do
  glBindBuffer GL_ARRAY_BUFFER (bufferID $ regionBuffer r)
  p <- glMapBufferRange GL_ARRAY_BUFFER (fromIntegral $ regionOffset r) (fromIntegral $ regionSize r) GL_MAP_WRITE_BIT
  pokeArray (castPtr p) . take (regionSize r) $ toList values
  () <$ glUnmapBuffer GL_ARRAY_BUFFER
#endif

-- |Fill a 'Region' with a value.
fill :: (MonadIO m,Storable a,Writable w) => Region w a -> a -> m ()
fill r a = writeWhole r (replicate (regionSize r) a)

-- |Index getter. Bounds checking is performed and returns 'Nothing' if out of bounds.
(@?) :: (MonadIO m,Storable a,Readable r) => Region r a -> Word32 -> m (Maybe a)
r @? i
  | i >= fromIntegral (regionSize r) = pure Nothing
  | otherwise = fmap Just (r @! i)

-- |Index getter. Unsafe version of '(@?)'.
(@!) :: (MonadIO m,Storable a,Readable r) => Region r a -> Word32 -> m a
#ifdef __GL45
r @! i = liftIO $ peekElemOff (regionPtr r) (fromIntegral i)
#elif defined(__GL32)
r @! i = liftIO $ do
  glBindBuffer GL_ARRAY_BUFFER (bufferID $ regionBuffer r)
  p <- glMapBufferRange GL_ARRAY_BUFFER (fromIntegral $ regionOffset r) (fromIntegral $ regionSize r) GL_MAP_READ_BIT
  a <- peekElemOff (castPtr p) (fromIntegral i)
  _ <- glUnmapBuffer GL_ARRAY_BUFFER
  pure a
#endif

-- |Index setter. Bounds checking is performed and nothing is done if out of bounds.
writeAt :: (MonadIO m,Storable a,Writable w) => Region w a -> Word32 -> a -> m ()
writeAt r i a
  | i >= fromIntegral (regionSize r) = pure ()
  | otherwise = writeAt' r i a

-- |Index setter. Unsafe version of 'writeAt''.
writeAt' :: (MonadIO m,Storable a,Writable w) => Region w a -> Word32 -> a -> m ()
#ifdef __GL45
writeAt' r i a = liftIO $ pokeElemOff (regionPtr r) (fromIntegral i) a
#elif defined(__GL32)
writeAt' r i a = liftIO $ do
  glBindBuffer GL_ARRAY_BUFFER (bufferID $ regionBuffer r)
  p <- glMapBufferRange GL_ARRAY_BUFFER (fromIntegral $ regionOffset r) (fromIntegral $ regionSize r) GL_MAP_WRITE_BIT
  pokeElemOff (castPtr p) (fromIntegral i) a
  () <$ glUnmapBuffer GL_ARRAY_BUFFER
#endif
