-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.GPU where

import Data.Int ( Int32 )
import Data.Proxy ( Proxy )
import Data.Word ( Word32 )
import Graphics.GL

class GPU a where
  glType :: Proxy a -> GLenum

instance GPU Float where
  glType _ = GL_FLOAT

instance GPU Int32 where
  glType _ = GL_INT

instance GPU Word32 where
  glType _ = GL_UNSIGNED_INT
