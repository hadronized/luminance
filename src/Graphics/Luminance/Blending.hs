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

import Graphics.GL

data BlendingMode
  = Additive
  | Subtract
  | ReverseSubtract
  | Min
  | Max
    deriving (Eq,Show)

fromBlendingMode :: BlendingMode -> GLenum
fromBlendingMode m = case m of
  Additive        -> GL_FUNC_ADD
  Subtract        -> GL_FUNC_SUBTRACT
  ReverseSubtract -> GL_FUNC_REVERSE_SUBTRACT
  Min             -> GL_MIN
  Max             -> GL_MAX

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

fromBlendingFactor :: BlendingFactor -> GLenum
fromBlendingFactor f = case f of
  One                   -> GL_ONE
  Zero                  -> GL_ZERO
  SrcColor              -> GL_SRC_COLOR
  NegativeSrcColor      -> GL_ONE_MINUS_SRC_COLOR
  DestColor             -> GL_DST_COLOR
  NegativeDestColor     -> GL_ONE_MINUS_DST_COLOR
  SrcAlpha              -> GL_SRC_ALPHA
  NegativeSrcAlpha      -> GL_ONE_MINUS_SRC_ALPHA
  DstAlpha              -> GL_DST_ALPHA
  NegativeDstAlpha      -> GL_ONE_MINUS_DST_ALPHA
  ConstantColor         -> GL_CONSTANT_COLOR
  NegativeConstantColor -> GL_ONE_MINUS_CONSTANT_COLOR
  ConstantAlpha         -> GL_CONSTANT_ALPHA
  NegativeConstantAlpha -> GL_ONE_MINUS_CONSTANT_ALPHA
  SrcAlphaSaturate      -> GL_SRC_ALPHA_SATURATE

