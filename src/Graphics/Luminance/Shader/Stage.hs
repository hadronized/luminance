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

tcsShader :: (MonadIO m,MonadResource m) => String -> m (Either String Stage)
tcsShader = mkShader GL_TESS_CONTROL_SHADER

tesShader :: (MonadIO m,MonadResource m) => String -> m (Either String Stage)
tesShader = mkShader GL_TESS_EVALUATION_SHADER

vertexShader :: (MonadIO m,MonadResource m) => String -> m (Either String Stage)
vertexShader = mkShader GL_VERTEX_SHADER

geometryShader :: (MonadIO m,MonadResource m) => String -> m (Either String Stage)
geometryShader = mkShader GL_GEOMETRY_SHADER

fragmentShader :: (MonadIO m,MonadResource m) => String -> m (Either String Stage)
fragmentShader = mkShader GL_FRAGMENT_SHADER

computeShader :: (MonadIO m,MonadResource m) => String -> m (Either String Stage)
computeShader = mkShader GL_COMPUTE_SHADER

mkShader :: (MonadIO m,MonadResource m) => GLenum -> String -> m (Either String Stage)
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
        pure . Right $ Stage sid
    | otherwise -> pure $ Left cl

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
