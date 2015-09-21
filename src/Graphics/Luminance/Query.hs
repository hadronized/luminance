-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Query (
    -- * Available queries
    getGLVendor
  , getGLRenderer
  , getGLVersion
  , getGLSLVersion
  , getGLExtensions
  ) where

import Graphics.Luminance.Core.Query
