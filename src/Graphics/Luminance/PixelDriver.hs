{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015, 2016 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-----------------------------------------------------------------------------

module Graphics.Luminance.PixelDriver where

import GHC.Exts ( Constraint )

-- |A driver to implement to provide pixel format features.
class (Monad m) => PixelDriver m where
  -- |All possible pixel formats.
  type Pixel m :: * -> Constraint
