-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Query where

import Control.Monad.IO.Class ( MonadIO(..) )
import Data.List.Split ( splitOn )
import Foreign.C.String ( peekCString )
import Foreign.Ptr ( castPtr )
import Graphics.GL

getString :: (MonadIO m) => GLenum -> m String
getString name = liftIO $ glGetString name >>= peekCString . castPtr

getGLVendor :: (MonadIO m) => m String
getGLVendor = getString GL_VENDOR

getGLRenderer :: (MonadIO m) => m String
getGLRenderer = getString GL_RENDERER

getGLVersion :: (MonadIO m) => m String
getGLVersion = getString GL_VERSION

getGLSLVersion :: (MonadIO m) => m String
getGLSLVersion = getString GL_SHADING_LANGUAGE_VERSION

-- TODO: implement that with glGetStringi
--getGLExtensions :: (MonadIO m) => m [String]
--getGLExtensions = fmap (splitOn ",") $ getString GL_EXTENSIONS
