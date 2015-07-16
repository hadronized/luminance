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

-- |A pixel format.
data Format b t
