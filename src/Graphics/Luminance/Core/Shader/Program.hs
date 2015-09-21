-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

module Graphics.Luminance.Core.Shader.Program where

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
import Graphics.Luminance.Core.Shader.Stage ( Stage(..) )
import Graphics.Luminance.Core.Shader.Uniform ( U, Uniform(..) )
import Graphics.GL
import Numeric.Natural ( Natural )

--------------------------------------------------------------------------------
-- Shader program --------------------------------------------------------------

-- |Shader program.
newtype Program = Program { programID :: GLuint }

-- |Create a new shader 'Program'.
--
-- That function takes a list of 'Stage's and a uniform interface builder function and yields a
-- 'Program' and the interface.
--
-- The builder function takes a function you can use to retrieve uniforms. You can pass
-- 'Left name' to map a 'String' to a uniform or you can pass 'Right sem' to map a semantic
-- 'Natural' to a uniform. If the uniform can’t be retrieved, throws 'InactiveUniform'.
--
-- In the end, you get the new 'Program' and a polymorphic value you can choose the type of in
-- the function you pass as argument. You can use that value to gather uniforms for instance.
createProgram :: (HasProgramError e,MonadError e m,MonadIO m,MonadResource m)
              => [Stage]
              -> ((forall a. (Uniform a) => Either String Natural -> m (U a)) -> m i)
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

-- |A simpler version of 'createProgram'. That function assumes you don’t need a uniform interface
-- and then just returns the 'Program'.
createProgram_ :: (HasProgramError e,MonadError e m,MonadIO m,MonadResource m)
                => [Stage]
                -> m Program
createProgram_ stages = fmap fst $ createProgram stages (\_ -> pure ())

-- |Is a shader program linked?
isLinked :: GLuint -> IO Bool
isLinked pid = do
  ok <- alloca $ liftA2 (*>) (glGetProgramiv pid GL_LINK_STATUS) peek
  pure $ ok == GL_TRUE

-- |Shader program link log’s length.
clogLength :: GLuint -> IO Int
clogLength pid =
  fmap fromIntegral .
    alloca $ liftA2 (*>) (glGetProgramiv pid GL_INFO_LOG_LENGTH) peek

-- |Shader program link log.
clog :: Int -> GLuint -> IO String
clog l pid =
  allocaArray l $
    liftA2 (*>) (glGetProgramInfoLog pid (fromIntegral l) nullPtr)
      (peekCString . castPtr)

-- |Either map a 'String' or 'Natural' to a uniform.
ifaceWith :: (HasProgramError e,MonadError e m,MonadIO m,Uniform a)
          => Program
          -> Either String Natural
          -> m (U a)
ifaceWith prog access = case access of
    Left name -> do
      location <- liftIO . withCString name $ glGetUniformLocation pid
      if
        | isActive location -> pure $ toU pid location
        | otherwise         -> throwError . fromProgramError $ InactiveUniform access
    Right sem
      | isActive sem -> pure $ toU pid (fromIntegral sem)
      | otherwise    -> throwError . fromProgramError $ InactiveUniform access
  where
    pid = programID prog
    isActive :: (Ord a,Num a) => a -> Bool
    isActive = (> -1)

--------------------------------------------------------------------------------
-- Shader program errors -------------------------------------------------------

-- |Shader program error.
--
-- 'LinkFailed reason' happens when a program fails to link. 'reason' contains the error message.
--
-- 'InactiveUniform uni' happens at linking when a uniform is inactive in the program; that
-- is, unused or semantically set to a negative value.
data ProgramError
  = LinkFailed String
  | InactiveUniform (Either String Natural)
    deriving (Eq,Show)

-- |Types that can handle 'ProgramError' – read as, “have”.
class HasProgramError a where
  fromProgramError :: ProgramError -> a
