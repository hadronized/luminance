-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

module Graphics.Luminance.Memory (
    -- * Garbage collector handled values
    GC(..)
  , unGC
  , embedGC
  , embedGCPtr
  , withGC
  , withGCPtr
    -- * Re-exported
  , module X
  ) where

import Control.Monad ( (>=>) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Concurrent ( newForeignPtr )
import Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import Foreign.Marshal.Alloc as X ( malloc, free )
import Foreign.Ptr as X ( Ptr, nullPtr )
import Foreign.Storable as X ( Storable, peek, poke )
import GHC.IO ( unsafePerformIO )

newtype GC a = GC (ForeignPtr a)

embedGC :: (MonadIO m,Storable a) => a -> IO () -> m (GC a)
embedGC a finalizer = liftIO $ do
  p <- malloc
  poke p a
  GC <$> newForeignPtr p (finalizer >> free p)

embedGCPtr :: (MonadIO m) => Ptr a -> IO () -> m (GC a)
embedGCPtr p finalizer = liftIO $ GC <$> newForeignPtr p (finalizer >> free p)

withGCPtr :: (MonadIO m) => GC a -> (Ptr a -> IO b) -> m b
withGCPtr (GC fp) f = liftIO $ withForeignPtr fp f

withGC :: (MonadIO m,Storable a) => GC a -> (a -> IO b) -> m b
withGC (GC fp) f = liftIO . withForeignPtr fp $ peek >=> f

unGC :: (Storable a) => GC a -> a
unGC (GC fp) = unsafePerformIO $ withForeignPtr fp peek
