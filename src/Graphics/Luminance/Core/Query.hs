-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015, 2016 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Core.Query where

import Control.Monad ( (>=>) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Traversable ( for )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Storable ( peek )
import Foreign.C.String ( peekCString )
import Foreign.Ptr ( castPtr )
import Graphics.GL

-- Nicer version of 'glGetString'.
getString :: (MonadIO m) => GLenum -> m String
getString name = liftIO $ glGetString name >>= peekCString . castPtr

-- |Get the OpenGL vendor 'String'.
getGLVendor :: (MonadIO m) => m String
getGLVendor = getString GL_VENDOR

-- |Get the OpenGL renderer 'String'.
getGLRenderer :: (MonadIO m) => m String
getGLRenderer = getString GL_RENDERER

-- |Get the OpenGL version 'String'.
getGLVersion :: (MonadIO m) => m String
getGLVersion = getString GL_VERSION

-- |Get the GLSL version 'String'.
getGLSLVersion :: (MonadIO m) => m String
getGLSLVersion = getString GL_SHADING_LANGUAGE_VERSION

-- |Retrieve all the supported OpenGL extensions as 'String's.
getGLExtensions :: (MonadIO m) => m [String]
getGLExtensions = liftIO $ do
  num <- alloca $ \num -> glGetIntegerv GL_NUM_EXTENSIONS num >> peek num
  for [0..fromIntegral num - 1] $ glGetStringi GL_EXTENSIONS  >=> peekCString . castPtr
