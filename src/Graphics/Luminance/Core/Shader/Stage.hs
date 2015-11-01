{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}

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
import Control.Monad ( unless )
import Control.Monad.Except ( MonadError(throwError) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Resource ( MonadResource, register )
import Graphics.GL
import Graphics.Luminance.Core.Query ( getGLExtensions )
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

-- |A shader 'Stage' type.
data StageType
  = TessControlShader
  | TessEvaluationShader
  | VertexShader
  | GeometryShader
  | FragmentShader
    deriving (Eq,Show)

-- |Create a new tessellation control shader from a 'String' representation of its source code.
createTessCtrlShader :: (HasStageError e,MonadError e m,MonadIO m,MonadResource m) => String -> m Stage
createTessCtrlShader = mkShader GL_TESS_CONTROL_SHADER

-- |Create a shader stage from a 'String' representation of its source code and its type.
--
-- Note: on some hardware and backends, /tessellation shaders/ aren’t available. That function
-- throws 'UnsupportedStage' error in such cases.
createStage :: (HasStageError e,MonadError e m,MonadIO m,MonadResource m)
            => StageType
            -> String
            -> m Stage
createStage t src = case t of
    TessControlShader -> checkTessSupport "tessellation control" >> mkShader GL_TESS_CONTROL_SHADER src
    TessEvaluationShader -> checkTessSupport "tessellation evaluation" >> mkShader GL_TESS_EVALUATION_SHADER src
    VertexShader -> mkShader GL_VERTEX_SHADER src
    GeometryShader -> mkShader GL_GEOMETRY_SHADER src
    FragmentShader -> mkShader GL_FRAGMENT_SHADER src
  where
    checkTessSupport stage = do
      exts <- getGLExtensions
      unless ("GL_ARB_tessellation_shader" `elem` exts) $
        throwError $ fromStageError (UnsupportedStage stage)

-- Create a shader from the kind of shader and its source code 'String' representation.
mkShader :: (HasStageError e,MonadError e m,MonadIO m,MonadResource m)
         => GLenum
         -> String
         -> m Stage
mkShader target src = do
  (sid,compiled,cl) <- liftIO $ do
    sid <- glCreateShader target
    withCString (prependGLSLPragma src) $ \cstr -> do
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

prependGLSLPragma :: String -> String
prependGLSLPragma src =
#if GL45_BACKEND
     "#version 450 core\n"
#elif GL32_BACKEND
     "#version 150 core\n"
#endif
#if __GL_BINDLESS_TEXTURES
  ++ "#extension GL_ARB_bindless_texture : require\n"
  ++ "layout (bindless_sampler) uniform;"
#endif
  ++ src

--------------------------------------------------------------------------------
-- Shader stage errors ---------------------------------------------------------

-- |Error type of shaders.
--
-- 'CompilationFailed reason' occurs when a shader fails to compile, and the 'String' 'reason'
-- contains a description of the failure.
--
-- 'UnsupportedStage stage' occurs when you try to create a shader which type is not supported on
-- the current hardware.
data StageError
  = CompilationFailed String 
  | UnsupportedStage String
    deriving (Eq,Show)

-- |Types that can handle 'StageError'.
class HasStageError a where
  fromStageError :: StageError -> a
