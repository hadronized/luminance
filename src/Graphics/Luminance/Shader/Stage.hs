-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

module Graphics.Luminance.Shader.Stage where

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

newtype Stage = Stage { stageID :: GLuint }

newtype StageError = CompilationFailed String deriving (Eq,Show)

class HasStageError a where
  fromStageError :: StageError -> a

createTcsShader :: (HasStageError e,MonadError e m,MonadIO m,MonadResource m) => String -> m Stage
createTcsShader = mkShader GL_TESS_CONTROL_SHADER

createTesShader :: (HasStageError e,MonadError e m,MonadIO m,MonadResource m) => String -> m Stage
createTesShader = mkShader GL_TESS_EVALUATION_SHADER

createVertexShader :: (HasStageError e,MonadError e m,MonadIO m,MonadResource m) => String -> m Stage
createVertexShader = mkShader GL_VERTEX_SHADER

createGeometryShader :: (HasStageError e,MonadError e m,MonadIO m,MonadResource m) => String -> m Stage
createGeometryShader = mkShader GL_GEOMETRY_SHADER

createFragmentShader :: (HasStageError e,MonadError e m,MonadIO m,MonadResource m) => String -> m Stage
createFragmentShader = mkShader GL_FRAGMENT_SHADER

createComputeShader :: (HasStageError e,MonadError e m,MonadIO m,MonadResource m) => String -> m Stage
createComputeShader = mkShader GL_COMPUTE_SHADER

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

isCompiled :: GLuint -> IO Bool
isCompiled sid = do
  ok <- alloca $ liftA2 (*>) (glGetShaderiv sid GL_COMPILE_STATUS) peek
  pure $ ok == GL_TRUE

clogLength :: GLuint -> IO Int
clogLength sid =
  fmap fromIntegral . alloca $
    liftA2 (*>) (glGetShaderiv sid GL_INFO_LOG_LENGTH) peek

clog :: Int -> GLuint -> IO String
clog l sid =
  allocaArray l $
    liftA2 (*>) (glGetShaderInfoLog sid (fromIntegral l) nullPtr)
      (peekCString . castPtr)
