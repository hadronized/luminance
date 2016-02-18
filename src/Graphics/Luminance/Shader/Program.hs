-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015, 2016 Dimitri Sabadie
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
    -- * Uniform
  , Uniform
  , U
  , UniformInterface
  , UniformName(..)
  , SomeUniformName(..)
    -- * Uniform block
  , UniformBlock
  , UB(..)
    -- * Error handling
  , ProgramError(..)
  , HasProgramError(..)
  ) where

import Graphics.Luminance.Core.Shader.Program
import Graphics.Luminance.Core.Shader.UniformBlock
