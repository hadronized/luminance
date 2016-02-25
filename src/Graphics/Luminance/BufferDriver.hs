{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015, 2016 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-----------------------------------------------------------------------------

module Graphics.Luminance.BufferDriver where

import Foreign.Storable ( Storable )
import Graphics.Luminance.RW ( Readable, Writable )
import Numeric.Natural ( Natural )

-- |A driver to implement to provide buffer features.
class (Monad m) => BufferDriver m where
  -- |Convenient type to build 'Buffer's.
  type BuildBuffer m :: * -> * -> *
  -- |A 'Buffer' is a GPU typed memory area. It can be pictured as a GPU array.
  type Buffer m :: * -> * -> *
  -- |Create a new 'Buffer' by providing the number of wished elements.
  createRegion :: (Storable a) => Natural -> BuildBuffer m rw (Buffer m rw a)
  -- |Create a new 'Buffer'. Through the 'BuildBuffer' type, you can yield new buffers and embed
  -- them in the type of your choice. The function returns that type.
  createBuffer :: BuildBuffer m rw a -> m a
  -- |Read a whole 'Buffer'.
  readWhole    :: (Readable r,Storable a) => Buffer m r a -> m [a]
  -- |Write the whole 'Buffer'. If values are missing, only the provided values will replace the
  -- existing ones. If there are more values than the size of the 'Buffer', they are ignored.
  writeWhole   :: (Foldable f,Storable a,Writable w) => Buffer m w a -> f a -> m ()
  -- |Fill a 'Buffer' with a value.
  fill         :: (Storable a,Writable w) => Buffer m w a -> a -> m ()
  -- |Index getter. Bounds checking is performed and returns 'Nothing' if out of bounds.
  (@?)         :: (Readable r,Storable a) => Buffer m r a -> Natural -> m (Maybe a)
  -- |Index getter. Unsafe version of '(@?)'.
  (@!)         :: (Readable r,Storable a) => Buffer m r a -> Natural -> m a
  -- |Index setter. Bounds checking is performed and nothing is done if out of bounds.
  writeAt      :: (Storable a,Writable w) => Buffer m w a -> Natural -> a -> m ()
  -- |Index setter. Unsafe version of 'writeAt'.
  writeAt'     :: (Storable a,Writable w) => Buffer m w a -> Natural -> a -> m ()
