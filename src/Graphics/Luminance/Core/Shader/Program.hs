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
import Control.Monad.Trans.State ( StateT, evalStateT, gets, modify )
import Data.Foldable ( traverse_ )
import Foreign.C ( peekCString, withCString )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( allocaArray )
import Foreign.Ptr ( castPtr, nullPtr )
import Foreign.Storable ( Storable(peek, sizeOf) )
import Graphics.Luminance.Core.Buffer ( Region(..), bufferID )
import Graphics.Luminance.Core.Shader.Stage ( Stage(..) )
import Graphics.Luminance.Core.Shader.Uniform ( U(..), Uniform(..) )
import Graphics.Luminance.Core.Shader.UniformBlock ( UB, UniformBlock )
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
              -> ((forall a. (Uniform a) => Either String Natural -> UniformInterface m (U a)) -> (forall a. (Storable a,UniformBlock a) => String -> UniformInterface m (U (Region rw (UB a)))) -> UniformInterface m i)
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
    | linked -> do
        _ <- register $ glDeleteProgram pid
        let prog = Program pid
        a <- runUniformInterface $ buildIface (uniformize prog) (uniformizeBlock prog)
        pure (prog,a)
    | otherwise -> throwError . fromProgramError $ LinkFailed cl

-- |A simpler version of 'createProgram'. That function assumes you don’t need a uniform interface
-- and then just returns the 'Program'.
createProgram_ :: (HasProgramError e,MonadError e m,MonadIO m,MonadResource m)
                => [Stage]
                -> m Program
createProgram_ stages = fmap fst $ createProgram stages (\_ _ -> pure ())

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

--------------------------------------------------------------------------------
-- Uniform interface -----------------------------------------------------------

newtype UniformInterface m a = UniformInterface {
    runUniformInterface' :: StateT UniformInterfaceCtxt m a
  } deriving (Applicative,Functor,Monad)

runUniformInterface :: (Monad m) => UniformInterface m a -> m a
runUniformInterface ui = evalStateT (runUniformInterface' ui) emptyUniformInterfaceCtxt

newtype UniformInterfaceCtxt = UniformInterfaceCtxt {
    uniformInterfaceBufferBinding :: GLuint
  } deriving (Eq,Show)

emptyUniformInterfaceCtxt :: UniformInterfaceCtxt
emptyUniformInterfaceCtxt = UniformInterfaceCtxt {
    uniformInterfaceBufferBinding = 0
  }

-- |Either map a 'String' or 'Natural' to a uniform.
uniformize :: (HasProgramError e,MonadError e m,MonadIO m,Uniform a)
           => Program
           -> Either String Natural
           -> UniformInterface m (U a)
uniformize Program{programID = pid} access = UniformInterface $ case access of
  Left name -> do
    location <- liftIO . withCString name $ glGetUniformLocation pid
    if
      | location /= -1 -> pure $ toU pid location
      | otherwise         -> throwError . fromProgramError $ InactiveUniform access
  Right sem
    | sem /= -1 -> pure $ toU pid (fromIntegral sem)
    | otherwise    -> throwError . fromProgramError $ InactiveUniform access

-- |Map a 'String' to a uniform block.
uniformizeBlock :: forall a e m rw. (HasProgramError e,MonadError e m,MonadIO m,Storable a,UniformBlock a)
                => Program
                -> String
                -> UniformInterface m (U (Region rw (UB a)))
uniformizeBlock Program{programID = pid} name = UniformInterface $ do
    index <- liftIO . withCString name $ glGetUniformBlockIndex pid
    if
      | index /= GL_INVALID_INDEX -> do
          -- retrieve a new binding value and use it
          binding <- gets uniformInterfaceBufferBinding
          modify $ \ctxt -> ctxt { uniformInterfaceBufferBinding = succ $ uniformInterfaceBufferBinding ctxt }
          liftIO (glUniformBlockBinding pid index binding)
          pure . U $ \r -> do
            glBindBufferRange
              GL_UNIFORM_BUFFER
              binding
              (bufferID $ regionBuffer r)
              (fromIntegral $ regionOffset r)
              (fromIntegral $ regionSize r * sizeOf (undefined :: a))
      | otherwise -> throwError . fromProgramError $ InactiveUniformBlock name

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
  | InactiveUniformBlock String
    deriving (Eq,Show)

-- |Types that can handle 'ProgramError' – read as, “have”.
class HasProgramError a where
  fromProgramError :: ProgramError -> a
