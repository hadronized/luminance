{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
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

module Graphics.Luminance.Driver where

import Graphics.Luminance.BufferDriver
import Graphics.Luminance.DrawDriver
import Graphics.Luminance.PixelDriver
import Graphics.Luminance.TextureDriver

-- |A driver to implement to be considered as a luminance backend.
type Driver m = (BufferDriver m, DrawDriver m,PixelDriver m,TextureDriver m)
