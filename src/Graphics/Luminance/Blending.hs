-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Blending where

data BlendingMode
  = Additive
  | Substract
  | ReverseSubstract
  | Min
  | Max
    deriving (Eq,Show)

data BlendingFactor
  = One
  | Zero
  | SrcColor
  | NegativeSrcColor
  | DestColor
  | NegativeDestColor
  | SrcAlpha
  | NegativeSrcAlpha
  | DstAlpha
  | NegativeDstAlpha
  | ConstantColor
  | NegativeConstantColor
  | ConstantAlpha
  | NegativeConstantAlpha
  | SrcAlphaSaturate
    deriving (Eq,Show)
