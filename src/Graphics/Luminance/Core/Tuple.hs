{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Core.Tuple where

import Foreign.Storable ( Storable(..) )
import Foreign.Ptr ( castPtr, plusPtr )
import GHC.Generics ( Generic )

-- |A tuple of types, right-associated.
--
-- The 'Storable' instance is used for foreign packing on 32-bit.
data a :. b = a :. b deriving (Eq,Functor,Generic,Ord,Show)

infixr 6 :.

instance (Storable a,Storable b) => Storable (a :. b) where
  sizeOf _ = sizeOf (undefined :: a) + sizeOf (undefined :: b)
  alignment _ = 4 -- packed data
  peek p = do
    a <- peek $ castPtr p
    b <- peek . castPtr $ p `plusPtr` sizeOf (undefined :: a)
    pure $ a :. b
  poke p (a :. b) = do
    poke (castPtr p) a
    poke (castPtr $ p `plusPtr` sizeOf (undefined :: a)) b
