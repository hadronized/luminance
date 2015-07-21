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
