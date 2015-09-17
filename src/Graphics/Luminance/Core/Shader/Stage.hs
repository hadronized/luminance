-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

module Graphics.Luminance.Core.Shader.Stage where

import Control.Applicative ( liftA2 )
import Control.Monad.Except ( MonadError(throwError) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Resource ( MonadResource, register )
import Graphics.GL
import Foreign.C.String ( peekCString, withCString )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( allocaArray )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( castPtr, nullPtr )
import Foreign.Storable ( peek )

--------------------------------------------------------------------------------
-- Shader stages ---------------------------------------------------------------

-- |A shader 'Stage'.
newtype Stage = Stage { stageID :: GLuint }

-- |Create a new tessellation control shader from a 'String' representation of its source code.
createTessCtrlShader :: (HasStageError e,MonadError e m,MonadIO m,MonadResource m) => String -> m Stage
createTessCtrlShader = mkShader GL_TESS_CONTROL_SHADER

-- |Create a new tessellation evaluation shader from a 'String' representation of its source code.
createTessEvalShader :: (HasStageError e,MonadError e m,MonadIO m,MonadResource m) => String -> m Stage
createTessEvalShader = mkShader GL_TESS_EVALUATION_SHADER

-- |Create a new vertex shader from a 'String' representation of its source code.
createVertexShader :: (HasStageError e,MonadError e m,MonadIO m,MonadResource m) => String -> m Stage
createVertexShader = mkShader GL_VERTEX_SHADER

-- |Create a new geometry shader from a 'String' representation of its source code.
createGeometryShader :: (HasStageError e,MonadError e m,MonadIO m,MonadResource m) => String -> m Stage
createGeometryShader = mkShader GL_GEOMETRY_SHADER

-- |Create a new fragment shader from a 'String' representation of its source code.
createFragmentShader :: (HasStageError e,MonadError e m,MonadIO m,MonadResource m) => String -> m Stage
createFragmentShader = mkShader GL_FRAGMENT_SHADER

-- |Create a new compute shader from a 'String' representation of its source code.
createComputeShader :: (HasStageError e,MonadError e m,MonadIO m,MonadResource m) => String -> m Stage
createComputeShader = mkShader GL_COMPUTE_SHADER

-- Create a shader from the kind of shader and its source code 'String' representation.
mkShader :: (HasStageError e,MonadError e m,MonadIO m,MonadResource m)
         => GLenum
         -> String
         -> m Stage
mkShader target src = do
  (sid,compiled,cl) <- liftIO $ do
    sid <- glCreateShader target
    withCString src $ \cstr -> do
      with cstr $ \pcstr -> glShaderSource sid 1 pcstr nullPtr
      glCompileShader sid
      compiled <- isCompiled sid
      ll <- clogLength sid
      cl <- clog ll sid
      pure (sid,compiled,cl)
  if
    | compiled  -> do
        _ <- register $ glDeleteShader sid
        pure $ Stage sid
    | otherwise -> throwError . fromStageError $ CompilationFailed cl

-- Is a shader compiled?
isCompiled :: GLuint -> IO Bool
isCompiled sid = do
  ok <- alloca $ liftA2 (*>) (glGetShaderiv sid GL_COMPILE_STATUS) peek
  pure $ ok == GL_TRUE

-- Shader compilation log’s length.
clogLength :: GLuint -> IO Int
clogLength sid =
  fmap fromIntegral . alloca $
    liftA2 (*>) (glGetShaderiv sid GL_INFO_LOG_LENGTH) peek

-- Shader compilation log.
clog :: Int -> GLuint -> IO String
clog l sid =
  allocaArray l $
    liftA2 (*>) (glGetShaderInfoLog sid (fromIntegral l) nullPtr)
      (peekCString . castPtr)

--------------------------------------------------------------------------------
-- Shader stage errors ---------------------------------------------------------

-- |Error type of shaders.
--
-- 'CompilationFailed reason' occurs when a shader fails to compile, and the 'String' 'reason'
-- contains a description of the failure.
newtype StageError = CompilationFailed String deriving (Eq,Show)

-- |Types that can handle 'StageError'.
class HasStageError a where
  fromStageError :: StageError -> a
