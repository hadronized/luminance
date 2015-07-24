-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Texture where

import Numeric.Natural ( Natural )

data Wrap
  = ClampToEdge
  | ClampToBorder
  | Repeat
    deriving (Eq,Show)

data Filter
  = Nearest
  | Linear
    deriving (Eq,Show)

data CompareFunc
  = Never
  | Less
  | Equal
  | LessOrEqual
  | Greater
  | GreaterOrEqual
  | NotEqual
  | Always
    deriving (Eq,Show)

newtype Unit = Unit Natural deriving (Enum,Eq,Integral,Num,Ord,Real,Show)

-- |Texture.
--
--            +-- Underlying channel type. Could be 'CInts', 'CUInts'
--            |   or 'CFloats'.
--            |
--            | +-- Channel format. Should be composed of 'CR', 'CG' and/or
--            | |   'CB' and channel sizes, like 'C8', 'C16' and/or 'C32'.
--            | |
--            | | +-- Dimension of the 'Texture'. Either 'D1', 'D2', 'D3' or
--            | | |   'Cube'.
--            | | |
--            | | | +-- Is the texture 'Layered' or 'Unlayered'?
--            | | | |
--            v v v v
-- @'Texture' t c d l@.
newtype Texture t c d l


Texture Floats ((CR,C8),(CG,C8),(CB,C8)) D2 Unlayered
