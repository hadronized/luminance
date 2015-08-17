-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

module Graphics.Luminance.Shader.Program where

import Control.Applicative ( liftA2 )
import Control.Monad.Except ( MonadError(throwError) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Resource ( MonadResource, register )
import Data.Foldable ( traverse_ )
import Foreign.C ( peekCString, withCString )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( allocaArray )
import Foreign.Ptr ( castPtr, nullPtr )
import Foreign.Storable ( peek )
import Graphics.Luminance.Shader.Stage ( Stage(..) )
import Graphics.Luminance.Shader.Uniform ( Uniform(..) )
import Graphics.GL
import Numeric.Natural ( Natural )

newtype Program = Program { programID :: GLuint }

newtype ProgramError = LinkFailed String deriving (Eq,Show)

class HasProgramError a where
  fromProgramError :: ProgramError -> a

createProgram :: (HasProgramError e,MonadError e m,MonadIO m,MonadResource m)
              => [Stage]
              -> ((forall a. (Uniform a) => Either String Natural -> m (Maybe (a -> m ()))) -> m i)
              -> m (Program,i)
createProgram stages buildIface = do
  (pid,linked,cl) <- liftIO $ do
    pid <- glCreateProgram
    traverse_ (glAttachShader pid . stageID) stages
    glLinkProgram pid
    linked <- isLinked pid
    ll <- clogLength pid
    cl <- clog ll pid
    pure (pid,linked,cl)
  if
    | linked    -> do
        _ <- register $ glDeleteProgram pid
        let prog = Program pid
        iface <- buildIface $ ifaceWith prog
        pure (prog,iface)
    | otherwise -> throwError . fromProgramError $ LinkFailed cl

isLinked :: GLuint -> IO Bool
isLinked pid = do
  ok <- alloca $ liftA2 (*>) (glGetProgramiv pid GL_LINK_STATUS) peek
  pure $ ok == GL_TRUE

clogLength :: GLuint -> IO Int
clogLength pid =
  fmap fromIntegral .
    alloca $ liftA2 (*>) (glGetProgramiv pid GL_INFO_LOG_LENGTH) peek

clog :: Int -> GLuint -> IO String
clog l pid =
  allocaArray l $
    liftA2 (*>) (glGetProgramInfoLog pid (fromIntegral l) nullPtr)
      (peekCString . castPtr)

ifaceWith :: (MonadIO m,Uniform a)
          => Program
          -> Either String Natural
          -> m (Maybe (a -> m ()))
ifaceWith prog access = case access of
    Left name -> do
      location <- liftIO . withCString name $ glGetUniformLocation pid
      if
        | isActive location -> pure . Just $ uploadUniform pid location
        | otherwise         -> pure Nothing
    Right sem
      | isActive sem -> pure . Just $ uploadUniform pid (fromIntegral sem)
      | otherwise    -> pure Nothing
  where
    pid = programID prog
    isActive :: (Ord a,Num a) => a -> Bool
    isActive = (> -1)
