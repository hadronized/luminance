{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

-- FIXME: #13
module Graphics.Luminance.Core.Pixel where

import Data.Proxy ( Proxy(..) )
import Data.Word ( Word8 )
import Graphics.GL

--------------------------------------------------------------------------------
-- Channel size ----------------------------------------------------------------

class ChannelSize c where
  channelSize :: (Num size) => proxy c -> size

-- |A 8-bit channel.
data C8 = C8 deriving (Eq,Ord,Show)

instance ChannelSize C8 where
  channelSize _ = 8

-- |A 16-bit channel.
data C16 = C16 deriving (Eq,Ord,Show)

instance ChannelSize C16 where
  channelSize _ = 16

-- |A 32-bit channel.
data C32 = C32 deriving (Eq,Ord,Show)

instance ChannelSize C32 where
  channelSize _ = 32

--------------------------------------------------------------------------------
-- Channel type ----------------------------------------------------------------

class ChannelType t where
  channelType :: proxy t -> GLenum

-- |Channels are integral values.
data CInts = CInts deriving (Eq,Ord,Show)

instance ChannelType CInts where
  channelType _ = GL_INT

-- |Channels are unsigned integral values.
data CUInts = CUInts deriving (Eq,Ord,Show)

instance ChannelType CUInts where
  channelType _ = GL_UNSIGNED_INT

-- |Channels are floating values.
data CFloats = CFloats deriving (Eq,Ord,Show)

instance ChannelType CFloats where
  channelType _ = GL_FLOAT

--------------------------------------------------------------------------------
-- Channel shape ---------------------------------------------------------------

-- |A red channel only.
data CR a = CR deriving (Eq,Ord,Show)

-- |Rd and green channels.
data CRG a b = CRG deriving (Eq,Ord,Show)

-- |Red, green and blue channels.
data CRGB a b c = CRGB deriving (Eq,Ord,Show)

-- |Red, green, blue and alpha channels.
data CRGBA a b c d = CRGBA deriving (Eq,Ord,Show)

-- |A depth channel.
data CDepth a = CDepth deriving (Eq,Ord,Show)

--------------------------------------------------------------------------------
-- Pixel format ----------------------------------------------------------------

-- |A pixel format.
data Format t c = Format deriving (Eq,Ord,Show)

instance (ChannelType t) => ChannelType (Format t c) where
  channelType _ = channelType (Proxy :: Proxy t)

type RGB8UI   = Format CUInts  (CRGB C8 C8 C8)
type RGBA8UI  = Format CUInts  (CRGBA C8 C8 C8 C8)

type RGBA8F   = Format CFloats (CRGBA C8 C8 C8 C8)

type RGB32F   = Format CFloats (CRGB C32 C32 C32)
type RGBA32F  = Format CFloats (CRGBA C32 C32 C32 C32)
type Depth32F = Format CFloats (CDepth C32)

--------------------------------------------------------------------------------
-- OpenGL pixels ---------------------------------------------------------------

class Pixel f where
  type PixelBase f :: *
  pixelFormat  :: p f -> GLenum
  pixelIFormat :: p f -> GLenum
  pixelType    :: p f -> GLenum

instance Pixel RGB8UI where
  type PixelBase RGB8UI = Word8
  pixelFormat  _ = GL_RGB_INTEGER
  pixelIFormat _ = GL_RGB8UI
  pixelType    _ = GL_UNSIGNED_BYTE

instance Pixel RGBA8UI where
  type PixelBase RGBA8UI = Word8
  pixelFormat  _ = GL_RGBA_INTEGER
  pixelIFormat _ = GL_RGBA8UI
  pixelType    _ = GL_UNSIGNED_BYTE

instance Pixel RGB32F where
  type PixelBase RGB32F = Float
  pixelFormat  _ = GL_RGB
  pixelIFormat _ = GL_RGB32F
  pixelType    _ = GL_FLOAT

instance Pixel RGBA32F where
  type PixelBase RGBA32F = Float
  pixelFormat  _ = GL_RGBA
  pixelIFormat _ = GL_RGBA32F
  pixelType    _ = GL_FLOAT

instance Pixel Depth32F where
  type PixelBase Depth32F = Float
  pixelFormat  _ = GL_DEPTH_COMPONENT
  pixelIFormat _ = GL_DEPTH_COMPONENT32F
  pixelType    _ = GL_FLOAT

--------------------------------------------------------------------------------
-- Pixel kind ------------------------------------------------------------------

class (Pixel p) => ColorPixel p

instance (Pixel (Format t (CR r))) => ColorPixel (Format t (CR r))
instance (Pixel (Format t (CRG r g))) => ColorPixel (Format t (CRG r g))
instance (Pixel (Format t (CRGB r g b))) => ColorPixel (Format t (CRGB r g b))
instance (Pixel (Format t (CRGBA r g b a))) => ColorPixel (Format t (CRGBA r g b a))
