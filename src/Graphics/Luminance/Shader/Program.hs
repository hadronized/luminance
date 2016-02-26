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
  , U'
  , (.=)
  , updateUniforms
  , UniformInterface
  , UniformName(..)
  , SomeUniformName(..)
    -- * Uniform block
  , UniformBlock
  , UB(..)
    -- * Error handling
  , ProgramError(..)
  , HasProgramError(..)
    -- * Re-exported
  , (<>)
  , sconcat
  ) where

import Graphics.Luminance.Core.Shader.Program
import Graphics.Luminance.Core.Shader.UniformBlock
import Data.Semigroup
