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
data C8

-- |A 16-bit channel.
data C16

-- |A 32-bit channel.
data C32

-- |Channels are integral values.
data CInts

-- |Channels are unsigned integral values.
data CUInts

-- |Channels are floating values.
data CFloats

-- |A color channel.
data CColor

-- |A depth channel.
data CDepth

-- |A red channel.
data CR

-- |A green channel.
data CG

-- |A blue channel.
data CB

-- |An alpha channel.
data CA
