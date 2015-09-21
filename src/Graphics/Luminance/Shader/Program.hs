-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

module Graphics.Luminance.Shader.Program (
    -- * Shader program creation
    Program
  , programID
  , createProgram
  , createProgram_
    -- * Error handling
  , ProgramError(..)
  , HasProgramError(..)
  ) where

import Graphics.Luminance.Core.Shader.Program
