-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- That module exports blending-related types and functions.
--
-- Given two pixels @src@ and @dst@ – source and destination, we associate
-- each pixel a /blending factor/ – respectively, @srcK@ and @dstK@. @src@ is
-- the pixel being computed, and @dst@ is the pixel that is already stored in
-- the framebuffer.
--
-- The pixels can be blended in several ways. See the documentation of
-- 'BlendingMode' for further details.
--
-- The factors are encoded with 'BlendingFactor'.
-----------------------------------------------------------------------------

module Graphics.Luminance.Core.Blending where

import Control.Monad.IO.Class ( MonadIO(..) )
import Graphics.GL

-- |All different blending modes.
--
-- 'Additive' represents the following blending equation:
--
-- @blended = src * srcK + dst * dstK@
--
-- 'Subtract' represents the following blending equation:
--
-- @blended = src * srcK - dst * dstK@
--
-- Because subtracting is not commutating, 'ReverseSubtract' represents the following additional
-- blending equation:
--
-- @blended = dst * dstK - src * srcK@
--
-- 'Min' represents the following blending equation:
--
-- @blended = 'min' src dst@
--
-- 'Max' represents the following blending equation:
--
-- @blended = 'max' src dst@
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

-- |Blending factors.
data BlendingFactor
  = One               -- ^ 1 * color = factor
  | Zero              -- ^ 0 * color = 0
  | SrcColor          -- ^ src * color
  | NegativeSrcColor  -- ^ (1 - src) * color
  | DestColor         -- ^ dst * color
  | NegativeDestColor -- ^ (1 - dst) * color
  | SrcAlpha          -- ^ srcA * color
  | NegativeSrcAlpha  -- ^ (1 - src) * color
  | DstAlpha          -- ^ dstA * color
  | NegativeDstAlpha  -- ^ (1 - dstA) * color
  {-
  | ConstantColor
  | NegativeConstantColor
  | ConstantAlpha
  | NegativeConstantAlpha
  -}
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
  -- ConstantColor         -> GL_CONSTANT_COLOR
  -- NegativeConstantColor -> GL_ONE_MINUS_CONSTANT_COLOR
  -- ConstantAlpha         -> GL_CONSTANT_ALPHA
  -- NegativeConstantAlpha -> GL_ONE_MINUS_CONSTANT_ALPHA
  SrcAlphaSaturate      -> GL_SRC_ALPHA_SATURATE

-- Sets blending mode and both factors after having initialized the blending if something is
-- passed. If 'Nothing', that function disables the blending.
setBlending :: (MonadIO m) => Maybe (BlendingMode,BlendingFactor,BlendingFactor) -> m ()
setBlending blending = liftIO $ case blending of
  Just (mode,src,dst) -> do
    glEnable GL_BLEND
    glBlendEquation (fromBlendingMode mode)
    glBlendFunc (fromBlendingFactor src) (fromBlendingFactor dst)
  Nothing -> glDisable GL_BLEND
