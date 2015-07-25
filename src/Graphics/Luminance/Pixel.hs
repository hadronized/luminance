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

-- |A red channel only.
data CR a = CR deriving (Eq,Ord,Show)

-- |Rd and green channels.
data CRG a b = CRG deriving (Eq,Ord,Show)

-- |Red, green and blue channels.
data CRGB a b c= CRGB deriving (Eq,Ord,Show)

-- |Red, green, blue and alpha channels.
data CRGBA a b c d = CRGBA deriving (Eq,Ord,Show)

-- |A depth channel.
data CDepth a = CDepth deriving (Eq,Ord,Show)

-- |A pixel format.
data Format t c = Format deriving (Eq,Ord,Show)

type RGB8F    = Format CFloats (CRGB C8  C8  C8)
type RGB16F   = Format CFloats (CRGB C16 C16 C16)
type RGB32F   = Format CFloats (CRGB C32 C32 C32)
type Depth32F = Format CFloats (CDepth C32)
