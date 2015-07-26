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

import Graphics.GL

class ChannelSize c where
  channelSize :: (Num size) => c -> size

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

class ChannelType t where
  channelType :: t -> GLenum

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

-- |A pixel format.
data Format t c = Format deriving (Eq,Ord,Show)

instance (ChannelType t) => ChannelType (Format t c) where
  channelType _ = channelType (undefined :: t)

type RGB8F    = Format CFloats (CRGB C8  C8  C8)
type RGB16F   = Format CFloats (CRGB C16 C16 C16)
type RGB32F   = Format CFloats (CRGB C32 C32 C32)
type Depth32F = Format CFloats (CDepth C32)

class GLFormats f where
  glFormat  :: f -> GLenum
  glIFormat :: f -> GLenum
  glType    :: f -> GLenum

instance GLFormats RGB8F where
  glFormat  _ = GL_RGB
  glIFormat _ = GL_RGB32F
  glType    _ = GL_FLOAT

instance GLFormats RGB16F where
  glFormat  _ = GL_RGB
  glIFormat _ = GL_RGB16F
  glType    _ = GL_FLOAT

instance GLFormats RGB32F where
  glFormat  _ = GL_RGB
  glIFormat _ = GL_RGB32F
  glType    _ = GL_FLOAT

instance GLFormats Depth32F where
  glFormat  _ = GL_DEPTH_COMPONENT
  glIFormat _ = GL_DEPTH_COMPONENT32F
  glType    _ = GL_FLOAT
