-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Tuple where

import Foreign.Storable ( Storable(..) )
import Foreign.Ptr ( castPtr, plusPtr )

-- |A tuple of types, right-associated. Used for foreign packing, so that the
-- Storable instance is possible.
data a :. b = a :. b deriving (Eq,Functor,Ord,Show)

infixr 6 :.

instance (Storable a,Storable b) => Storable (a :. b) where
  sizeOf (a :. b) = sizeOf a + sizeOf b
  alignment _ = 1
  peek p = do
    a <- peek $ castPtr p
    b <- peek . castPtr $ p `plusPtr` sizeOf (undefined :: a)
    pure $ a :. b
  poke p (a :. b) = do
    poke (castPtr p) a
    poke (castPtr $ p `plusPtr` sizeOf (undefined :: a)) b
