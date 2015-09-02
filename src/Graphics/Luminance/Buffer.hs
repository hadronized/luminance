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
import Data.Foldable ( toList )
import Data.Word ( Word32 )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( peekArray, pokeArray )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr, castPtr, nullPtr, plusPtr )
import Foreign.Storable ( Storable(..) )
import Graphics.GL
import Graphics.Luminance.RW

-- |A 'Buffer' is an opaque and untyped region of abstract GPU memory. You cannot do much with it
-- and you might even not see the type in the user interface as it’s not really needed. It’s shown
-- for informational purposes only.
newtype Buffer = Buffer { bufferID :: GLuint } deriving (Eq,Show)

-- Create a new 'Buffer' and return its GPU address by mapping it to a @Ptr ()@.
mkBuffer :: (MonadIO m,MonadResource m)
         => GLbitfield
         -> Word32
         -> m (Buffer,Ptr ())
mkBuffer flags size = do
  (bid,mapped) <- liftIO . alloca $ \p -> do
    glCreateBuffers 1 p
    bid <- peek p
    mapped <- createStorage bid flags size
    pure (bid,mapped)
  _ <- register . with bid $ glDeleteBuffers 1
  pure (Buffer bid,mapped)

-- Create the required OpenGL storage for a 'Buffer'.
createStorage :: GLuint -> GLbitfield -> Word32 -> IO (Ptr ())
createStorage bid flags size = do
    glNamedBufferStorage bid bytes nullPtr flags
    ptr <- glMapNamedBufferRange bid 0 bytes flags
    pure ptr
  where
    bytes = fromIntegral size

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
    pure (fst $ evalRWS built mapped 0,buffer)
  where
    built = runBuildRegion buildRegions
    (bytes,_) = execRWS built nullPtr 0

-- |'Buffer'’s 'Region's can have reads and writes. That typeclass makes implements all possible
-- cases.
class BufferRW rw where
  bufferFlagsFromRW :: rw -> GLenum

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
createBuffer = fmap fst . mkBufferWithRegions (bufferFlagsFromRW (undefined :: rw) .|. GL_MAP_PERSISTENT_BIT .|. GL_MAP_COHERENT_BIT)

-- Special case of 'createBuffer', returning the 'Buffer' in addition for internal uses.
createBuffer_ :: forall a m rw. (BufferRW rw,MonadIO m,MonadResource m)
              => BuildRegion rw a
              -> m (a,Buffer)
createBuffer_ = mkBufferWithRegions $
  bufferFlagsFromRW (undefined :: rw) .|. GL_MAP_PERSISTENT_BIT .|. GL_MAP_COHERENT_BIT

-- |A 'Region' is a GPU typed memory area. It can be pictured as a GPU array.
data Region rw a = Region {
    regionPtr :: Ptr a
  , regionSize :: RegionSize a
  } deriving (Eq,Show)

-- FIXME: do we really need that?!
-- |Size of a region, in elements. You don’t have to worry about bytes consideration. If you want
-- a @Region rw Float@ with 10 elements in it, use @RegionSize 10@. That’s as simple as that.
newtype RegionSize a = RegionSize Word32 deriving (Eq,Num,Show)

-- Get the size in bytes of a 'RegionSize'.
bytesOfR :: forall a. (Storable a) => RegionSize a -> Word32
bytesOfR (RegionSize size) = fromIntegral (sizeOf (undefined :: a)) * size

-- |Convenient type to build 'Region's.
newtype BuildRegion rw a = BuildRegion {
    runBuildRegion :: RWS (Ptr ()) () Word32 a
  } deriving (Applicative,Functor,Monad)

-- |Create a new 'Region' by providing the number of wished elements.
newRegion :: forall rw a. (Storable a) => Word32 -> BuildRegion rw (Region rw a)
newRegion size = BuildRegion $ do
    offset <- get
    put $ offset + bytesOfR regionS
    ptr <- ask
    pure $ Region (castPtr $ ptr `plusPtr` fromIntegral offset) regionS
  where
    regionS :: RegionSize a
    regionS = RegionSize size

-- |Read a whole 'Region'.
readWhole :: (MonadIO m,Readable r,Storable a) => Region r a -> m [a]
readWhole (Region p (RegionSize nb)) = liftIO $ peekArray (fromIntegral nb) p

-- |Write the whole 'Region'. If value are missing, only the provided values will replace the
-- existing ones. If there are more values than the size of the 'Region', they are ignored.
writeWhole :: (Foldable f,MonadIO m,Storable a,Writable w)
           => Region w a
           -> f a
           -> m ()
writeWhole (Region p (RegionSize nb)) values =
  liftIO . pokeArray p . take (fromIntegral nb) $ toList values

-- |Fill a 'Region' with a value.
fill :: (MonadIO m,Storable a,Writable w) => Region w a -> a -> m ()
fill (Region p (RegionSize nb)) a =
  liftIO . pokeArray p $ replicate (fromIntegral nb) a

-- |Index getter. Bounds checking is performed and returns 'Nothing' if out of bounds.
(@?) :: (MonadIO m,Storable a,Readable r) => Region r a -> Word32 -> m (Maybe a)
Region p (RegionSize nb) @? i
  | i >= nb = pure Nothing
  | otherwise = liftIO $ Just <$> peekElemOff p (fromIntegral i)

-- |Index getter. Unsafe version of '(@?)'.
(@!) :: (MonadIO m,Storable a,Readable r) => Region r a -> Word32 -> m a
Region p _ @! i = liftIO $ peekElemOff p (fromIntegral i)

-- |Index setter. Bounds checking is performed and nothing is done if out of bounds.
writeAt :: (MonadIO m,Storable a,Writable w) => Region w a -> Word32 -> a -> m ()
writeAt (Region p (RegionSize nb)) i a
  | i >= nb = pure ()
  | otherwise = liftIO $ pokeElemOff p (fromIntegral i) a

-- |Index setter. Unsafe version of 'writeAt''.
writeAt' :: (MonadIO m,Storable a,Writable w) => Region w a -> Word32 -> a -> m ()
writeAt' (Region p _) i a = liftIO $ pokeElemOff p (fromIntegral i) a
