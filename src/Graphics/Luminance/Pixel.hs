-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Pixel where

-- |A 8-bit channel.
data C8 = C8 deriving (Eq,Ord,Show)

-- |A 16-bit channel.
data C16 = C16 deriving (Eq,Ord,Show)

-- |A 32-bit channel.
data C32 = C32 deriving (Eq,Ord,Show)

-- |Channels are integral values.
data CInts = CInts deriving (Eq,Ord,Show)

-- |Channels are unsigned integral values.
data CUInts = CUInts deriving (Eq,Ord,Show)

-- |Channels are floating values.
data CFloats = CFloats deriving (Eq,Ord,Show)

-- |A red channel.
data CR = CR deriving (Eq,Ord,Show)

-- |A green channel.
data CG = CG deriving (Eq,Ord,Show)

-- |A blue channel.
data CB = CB deriving (Eq,Ord,Show)

-- |An alpha channel.
data CA = CA deriving (Eq,Ord,Show)
