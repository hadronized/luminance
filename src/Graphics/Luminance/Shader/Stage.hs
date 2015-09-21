-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

module Graphics.Luminance.Shader.Stage (
    -- * Shader stage creation
    Stage
  , stageID
  , createTessCtrlShader
  , createTessEvalShader
  , createVertexShader
  , createGeometryShader
  , createFragmentShader
  , createComputeShader
    -- * Error handling
  , StageError(..)
  , HasStageError(..)
  ) where

import Graphics.Luminance.Core.Shader.Stage
