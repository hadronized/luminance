{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Control.Monad ( unless, when )
import Control.Monad.Except ( MonadError(throwError) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Resource ( MonadResource, register )
import Control.Monad.Trans.State ( StateT, evalStateT, gets, modify )
import Data.Foldable ( toList, traverse_ )
import Data.Functor.Contravariant ( Contravariant(..) )
import Data.Functor.Contravariant.Divisible ( Decidable(..), Divisible(..) )
import Data.Int ( Int32 )
import Data.Proxy ( Proxy(..) )
import Data.Semigroup ( Semigroup(..) )
import Data.Void ( absurd )
import Data.Word ( Word32 )
import Foreign.C ( peekCString, withCString )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( allocaArray, withArrayLen )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( castPtr, nullPtr )
import Foreign.Storable ( Storable(peek) )
import Graphics.Luminance.Core.Buffer ( Region(..), bufferID )
import Graphics.Luminance.Core.Debug
import Graphics.Luminance.Core.Cubemap ( Cubemap(..) )
import Graphics.Luminance.Core.Pixel ( Pixel )
import Graphics.Luminance.Core.Shader.Stage ( Stage(..) )
import Graphics.Luminance.Core.Shader.UniformBlock ( UB, UniformBlock(sizeOfSTD140) )
import Graphics.Luminance.Core.Texture
import Graphics.Luminance.Core.Texture1D
import Graphics.Luminance.Core.Texture2D
import Graphics.Luminance.Core.Texture3D
import Graphics.GL
#ifdef __GL_BINDLESS_TEXTURES
import Graphics.GL.Ext.ARB.BindlessTexture ( glProgramUniformHandleui64ARB )
#endif
import Linear
import Linear.V ( V(..) )
import Numeric.Natural ( Natural )

--------------------------------------------------------------------------------
-- Shader program --------------------------------------------------------------

-- |Shader program.
newtype Program = Program { programID :: GLuint } deriving (Eq,Show)

-- |Create a new shader 'Program'.
--
-- That function takes a list of 'Stage's and a uniform interface builder function and yields a
-- 'Program' and the interface.
--
-- The builder function takes a function you can use to retrieve uniforms. You can pass
-- values of type 'UniformName' to identify the uniform you want to retrieve. If the uniform can’t
-- be retrieved, throws 'InactiveUniform'.
--
-- In the end, you get the new 'Program' and a polymorphic value you can choose the type of in
-- the function you pass as argument. You can use that value to gather uniforms for instance.
createProgram :: (HasProgramError e,MonadError e m,MonadIO m,MonadResource m)
              => [Stage]
              -> ((forall a. UniformName a -> UniformInterface m (U a)) -> UniformInterface m i)
              -> m (Program,i)
createProgram stages buildIface = do
  (pid,linked,cl) <- liftIO $ do
    pid <- debugGL glCreateProgram
    traverse_ (debugGL . glAttachShader pid . stageID) stages
    debugGL $ glLinkProgram pid
    linked <- debugGL $ isLinked pid
    ll <- clogLength pid
    cl <- clog ll pid
    pure (pid,linked,cl)
  unless linked $ do
    liftIO (glDeleteProgram pid)
    throwError . fromProgramError $ LinkFailed cl
  _ <- register $ glDeleteProgram pid
  let prog = Program pid
  a <- runUniformInterface $ buildIface (uniformize prog)
  pure (prog,a)

-- |A simpler version of 'createProgram'. That function assumes you don’t need a uniform interface
-- and then just returns the 'Program'.
createProgram_ :: (HasProgramError e,MonadError e m,MonadIO m,MonadResource m)
                => [Stage]
                -> m Program
createProgram_ stages = fmap fst $ createProgram stages (\_ -> pure ())

-- |Is a shader program linked?
isLinked :: GLuint -> IO Bool
isLinked pid = do
  ok <- debugGL . alloca $ liftA2 (*>) (glGetProgramiv pid GL_LINK_STATUS) peek
  pure $ ok == GL_TRUE

-- |Shader program link log’s length.
clogLength :: GLuint -> IO Int
clogLength pid =
  fmap fromIntegral . debugGL . alloca $ liftA2 (*>) (glGetProgramiv pid GL_INFO_LOG_LENGTH) peek

-- |Shader program link log.
clog :: Int -> GLuint -> IO String
clog l pid =
  debugGL . allocaArray l $
    liftA2 (*>) (glGetProgramInfoLog pid (fromIntegral l) nullPtr) (peekCString . castPtr)

--------------------------------------------------------------------------------
-- Uniform interface -----------------------------------------------------------

newtype UniformInterface m a = UniformInterface {
    runUniformInterface' :: StateT UniformInterfaceCtxt m a
  } deriving (Applicative,Functor,Monad)

runUniformInterface :: (Monad m) => UniformInterface m a -> m a
runUniformInterface ui = evalStateT (runUniformInterface' ui) emptyUniformInterfaceCtxt

data UniformInterfaceCtxt = UniformInterfaceCtxt {
    -- Used to generate bindings for uniform blocks
    uniformInterfaceBufferBinding :: GLuint
#if !defined(__GL_BINDLESS_TEXTURES)
    -- Used to generate texture units when necessary
  , uniformInterfaceTextureUnit :: GLenum
#endif
  } deriving (Eq,Show)

emptyUniformInterfaceCtxt :: UniformInterfaceCtxt
emptyUniformInterfaceCtxt = UniformInterfaceCtxt {
    uniformInterfaceBufferBinding = 0
#if !defined(__GL_BINDLESS_TEXTURES)
  , uniformInterfaceTextureUnit = 0
#endif
  }

-- |Possible way to name uniform values.
data UniformName :: * -> * where
  UniformName :: (Uniform a) => String -> UniformName a
  UniformSemantic :: (Uniform a) => Natural -> UniformName a
  UniformBlockName :: (UniformBlock a) => String -> UniformName (Region rw (UB a))

-- |A uniform name with type-erasure. You can only access the constructors and the carried name but
-- you can’t reconstruct the phantom type.
data SomeUniformName = forall a. SomeUniformName (UniformName a)

instance Eq SomeUniformName where
  SomeUniformName (UniformName a) == SomeUniformName (UniformName b) = a == b
  SomeUniformName (UniformSemantic a) == SomeUniformName (UniformSemantic b) = a == b
  SomeUniformName (UniformBlockName a) == SomeUniformName (UniformBlockName b) = a == b
  _ == _ = False

instance Show SomeUniformName where
  show (SomeUniformName name) = case name of
    UniformName n -> "UniformName " ++ n
    UniformSemantic s -> "UniformSemantic " ++ show s
    UniformBlockName n -> "UniformBlockName " ++ n

-- |A way to get several kind of uniforms through a single interface.
uniformize :: (HasProgramError e,MonadError e m,MonadIO m)
           => Program
           -> UniformName a
           -> UniformInterface m (U a)
uniformize program@Program{programID = pid} getter = UniformInterface $ case getter of
  UniformName name -> do
    location <- liftIO . debugGL . withCString name $ glGetUniformLocation pid
    when (location == -1) . throwError . fromProgramError $ InactiveUniform (SomeUniformName getter)
    runUniformInterface' (toU pid location)
  UniformSemantic sem -> do
    when (sem == -1) . throwError . fromProgramError $ InactiveUniform (SomeUniformName getter)
    runUniformInterface' (toU pid $ fromIntegral sem)
  UniformBlockName name -> runUniformInterface (uniformizeBlock program name $ InactiveUniform (SomeUniformName getter))

-- |Map a 'String' to a uniform block.
uniformizeBlock :: forall a e m rw. (HasProgramError e,MonadError e m,MonadIO m,UniformBlock a)
                => Program
                -> String
                -> ProgramError
                -> UniformInterface m (U (Region rw (UB a)))
uniformizeBlock Program{programID = pid} name onError = UniformInterface $ do
  index <- liftIO . debugGL . withCString name $ glGetUniformBlockIndex pid
  when (index == GL_INVALID_INDEX) (throwError $ fromProgramError onError)
  -- retrieve a new binding value and use it
  binding <- gets uniformInterfaceBufferBinding
  modify $ \ctxt -> ctxt { uniformInterfaceBufferBinding = succ $ uniformInterfaceBufferBinding ctxt }
  liftIO . debugGL $ glUniformBlockBinding pid index binding
  pure . U $ \r -> do
    debugGL $ glBindBufferRange
      GL_UNIFORM_BUFFER
      binding
      (bufferID $ regionBuffer r)
      (fromIntegral $ regionOffset r)
      (fromIntegral $ regionSize r * sizeOfSTD140 (Proxy :: Proxy a))

#if !defined(__GL_BINDLESS_TEXTURES)
nextTextureUnit :: (Monad m) => UniformInterface m GLuint
nextTextureUnit = UniformInterface $ do
  texUnit <- gets uniformInterfaceTextureUnit
  modify $ \ctxt -> ctxt { uniformInterfaceTextureUnit = succ texUnit }
  pure texUnit
#endif

--------------------------------------------------------------------------------
-- Uniform ---------------------------------------------------------------------

-- |A shader uniform. @'U' a@ doesn’t hold any value. It’s more like a mapping between the host
-- code and the shader the uniform was retrieved from.
--
-- 'U' is contravariant in its argument. That means that you can use 'contramap' to build more
-- interesting uniform types.
--
-- Another interesting part is the fact that 'U' is also monoidal. You can accumulate several of
-- them with '(<>)' if they have the same type. That means that you can join them so that when you
-- pass an actual value, it gets shared inside the resulting value.
--
-- The '()' instance doesn’t do anything and doesn’t even use its argument.
newtype U a = U { runU :: a -> IO () }

instance Contravariant U where
  contramap f u = U $ runU u . f

instance Decidable U where
  lose f = U $ absurd . f
  choose f p q = U $ either (runU p) (runU q) . f

instance Divisible U where
  divide f p q = U $ \a -> do
    let (b,c) = f a
    runU p b
    runU q c
  conquer = mempty

instance Monoid (U a) where
  mempty = U . const $ pure ()
  mappend = (<>)

instance Semigroup (U a) where
  u <> v = U $ \a -> runU u a >> runU v a

-- |Class of types that can be sent down to shaders. That class is closed because shaders cannot
-- handle a lot of uniform types. However, you should have a look at the 'U' documentation for
-- further information about how to augment the scope of the types you can send down to shaders.
class Uniform a where
  -- |@'toU' prog l@ creates a new 'U a' by mapping it to the 'Program' @prog@ and using the
  -- location 'l'.
  toU :: (Monad m) => GLuint -> GLint -> UniformInterface m (U a)

--------------------------------------------------------------------------------
-- Unit instance ---------------------------------------------------------------

instance Uniform () where
  toU _ _ = pure mempty

--------------------------------------------------------------------------------
-- Int32 instances -------------------------------------------------------------

-- scalar
instance Uniform Int32 where
#ifdef __GL45
  toU prog l = pure $ U (glProgramUniform1i prog l)
#elif defined(__GL33)
  toU _ l = pure $ U (glUniform1i l)
#endif

-- D2
instance Uniform (Int32,Int32) where
#ifdef __GL45
  toU prog l = pure . U $ \(x,y) -> glProgramUniform2i prog l x y
#elif defined(__GL33)
  toU _ l = pure . U $ \(x,y) -> glUniform2i l x y
#endif

instance Uniform (V2 Int32) where
#ifdef __GL45
  toU prog l = pure . U $ \(V2 x y) -> glProgramUniform2i prog l x y
#elif defined(__GL33)
  toU _ l = pure . U $ \(V2 x y) -> glUniform2i l x y
#endif

instance Uniform (V 2 Int32) where
#ifdef __GL45
  toU prog l = pure . U $ \(V v) -> case toList v of
    [x,y] -> glProgramUniform2i prog l x y
    _ -> pure ()
#elif defined(__GL33)
  toU _ l = pure . U $ \(V v) -> case toList v of
    [x,y] -> glUniform2i l x y
    _ -> pure ()
#endif
    
-- D3
instance Uniform (Int32,Int32,Int32) where
#ifdef __GL45
  toU prog l = pure . U $ \(x,y,z) -> glProgramUniform3i prog l x y z
#elif defined(__GL33)
  toU _ l = pure . U $ \(x,y,z) -> glUniform3i l x y z
#endif

instance Uniform (V3 Int32) where
#ifdef __GL45
  toU prog l = pure . U $ \(V3 x y z) -> glProgramUniform3i prog l x y z
#elif defined(__GL33)
  toU _ l = pure . U $ \(V3 x y z) -> glUniform3i l x y z
#endif

instance Uniform (V 3 Int32) where
#ifdef __GL45
  toU prog l = pure . U $ \(V v) -> case toList v of
    [x,y,z] -> glProgramUniform3i prog l x y z
    _ -> pure ()
#elif defined(__GL33)
  toU _ l = pure . U $ \(V v) -> case toList v of
    [x,y,z] -> glUniform3i l x y z
    _ -> pure ()
#endif

-- D4
instance Uniform (Int32,Int32,Int32,Int32) where
#ifdef __GL45
  toU prog l = pure . U $ \(x,y,z,w) -> glProgramUniform4i prog l x y z w
#elif defined(__GL33)
  toU _ l = pure . U $ \(x,y,z,w) -> glUniform4i l x y z w
#endif

instance Uniform (V4 Int32) where
#ifdef __GL45
  toU prog l = pure . U $ \(V4 x y z w) -> glProgramUniform4i prog l x y z w
#elif defined(__GL33)
  toU _ l = pure . U $ \(V4 x y z w) -> glUniform4i l x y z w
#endif

instance Uniform (V 4 Int32) where
#ifdef __GL45
  toU prog l = pure . U $ \(V v) -> case toList v of
    [x,y,z,w] -> glProgramUniform4i prog l x y z w
    _ -> pure ()
#elif defined(__GL33)
  toU _ l = pure . U $ \(V v) -> case toList v of
    [x,y,z,w] -> glUniform4i l x y z w
    _ -> pure ()
#endif

-- scalar array
instance Uniform [Int32] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen v $ glProgramUniform1iv prog l . fromIntegral
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen v $ glUniform1iv l . fromIntegral
#endif

-- D2 array
instance Uniform [(Int32,Int32)] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen (concatMap unPair v) $
    glProgramUniform2iv prog l . fromIntegral
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen (concatMap unPair v) $
    glUniform2iv l . fromIntegral
#endif

instance Uniform [V2 Int32] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen v $ \size p ->
    glProgramUniform2iv prog l (fromIntegral size) (castPtr p)
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen v $ \size p ->
    glUniform2iv l (fromIntegral size) (castPtr p)
#endif

instance Uniform [V 2 Int32] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen v $ \size p ->
    glProgramUniform2iv prog l (fromIntegral size) (castPtr p)
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen v $ \size p ->
    glUniform2iv l (fromIntegral size) (castPtr p)
#endif

-- D3 array
instance Uniform [(Int32,Int32,Int32)] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen (concatMap unTriple v) $
    glProgramUniform3iv prog l . fromIntegral
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen (concatMap unTriple v) $
    glUniform3iv l . fromIntegral
#endif

instance Uniform [V3 Int32] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen v $ \size p ->
    glProgramUniform3iv prog l (fromIntegral size) (castPtr p)
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen v $ \size p ->
    glUniform3iv l (fromIntegral size) (castPtr p)
#endif

instance Uniform [V 3 Int32] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen v $ \size p ->
    glProgramUniform3iv prog l (fromIntegral size) (castPtr p)
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen v $ \size p ->
    glUniform3iv l (fromIntegral size) (castPtr p)
#endif

-- D4 array
instance Uniform [(Int32,Int32,Int32,Int32)] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen (concatMap unQuad v) $
    glProgramUniform4iv prog l . fromIntegral
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen (concatMap unQuad v) $
    glUniform4iv l . fromIntegral
#endif

instance Uniform [V4 Int32] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen v $ \size p ->
    glProgramUniform4iv prog l (fromIntegral size) (castPtr p)
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen v $ \size p ->
    glUniform4iv l (fromIntegral size) (castPtr p)
#endif

instance Uniform [V 4 Int32] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen v $ \size p ->
    glProgramUniform4iv prog l (fromIntegral size) (castPtr p)
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen v $ \size p ->
    glUniform4iv l (fromIntegral size) (castPtr p)
#endif

--------------------------------------------------------------------------------
-- Word32 instances ------------------------------------------------------------

-- scalar
instance Uniform Word32 where
#ifdef __GL45
  toU prog l = pure . U $ glProgramUniform1ui prog l
#elif defined(__GL33)
  toU _ l = pure . U $ glUniform1ui l
#endif

-- D2
instance Uniform (Word32,Word32) where
#ifdef __GL45
  toU prog l = pure . U $ \(x,y) -> glProgramUniform2ui prog l x y
#elif defined(__GL33)
  toU _ l = pure . U $ \(x,y) -> glUniform2ui l x y
#endif

instance Uniform (V2 Word32) where
#ifdef __GL45
  toU prog l = pure . U $ \(V2 x y) -> glProgramUniform2ui prog l x y
#elif defined(__GL33)
  toU _ l = pure . U $ \(V2 x y) -> glUniform2ui l x y
#endif

instance Uniform (V 2 Word32) where
#ifdef __GL45
  toU prog l = pure . U $ \(V v) -> case toList v of
    [x,y] -> glProgramUniform2ui prog l x y
    _ -> pure ()
#elif defined(__GL33)
  toU _ l = pure . U $ \(V v) -> case toList v of
    [x,y] -> glUniform2ui l x y
    _ -> pure ()
#endif

-- D3
instance Uniform (Word32,Word32,Word32) where
#ifdef __GL45
  toU prog l = pure . U $ \(x,y,z) -> glProgramUniform3ui prog l x y z
#elif defined(__GL33)
  toU _ l = pure . U $ \(x,y,z) -> glUniform3ui l x y z
#endif

instance Uniform (V3 Word32) where
#ifdef __GL45
  toU prog l = pure . U $ \(V3 x y z) -> glProgramUniform3ui prog l x y z
#elif defined(__GL33)
  toU _ l = pure . U $ \(V3 x y z) -> glUniform3ui l x y z
#endif

instance Uniform (V 3 Word32) where
#ifdef __GL45
  toU prog l = pure . U $ \(V v) -> case toList v of
    [x,y,z] -> glProgramUniform3ui prog l x y z
    _ -> pure ()
#elif defined(__GL33)
  toU _ l = pure . U $ \(V v) -> case toList v of
    [x,y,z] -> glUniform3ui l x y z
    _ -> pure ()
#endif

-- D4
instance Uniform (Word32,Word32,Word32,Word32) where
#ifdef __GL45
  toU prog l = pure . U $ \(x,y,z,w) -> glProgramUniform4ui prog l x y z w
#elif defined(__GL33)
  toU _ l = pure . U $ \(x,y,z,w) -> glUniform4ui l x y z w
#endif

instance Uniform (V4 Word32) where
#ifdef __GL45
  toU prog l = pure . U $ \(V4 x y z w) -> glProgramUniform4ui prog l x y z w
#elif defined(__GL33)
  toU _ l = pure . U $ \(V4 x y z w) -> glUniform4ui l x y z w
#endif

instance Uniform (V 4 Word32) where
#ifdef __GL45
  toU prog l = pure . U $ \(V v) -> case toList v of
    [x,y,z,w] -> glProgramUniform4ui prog l x y z w
    _ -> pure ()
#elif defined(__GL33)
  toU _ l = pure . U $ \(V v) -> case toList v of
    [x,y,z,w] -> glUniform4ui l x y z w
    _ -> pure ()
#endif

-- scalar array
instance Uniform [Word32] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen v $
    glProgramUniform1uiv prog l . fromIntegral
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen v $
    glUniform1uiv l . fromIntegral
#endif

-- D2 array
instance Uniform [(Word32,Word32)] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen (concatMap unPair v) $
    glProgramUniform2uiv prog l . fromIntegral
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen (concatMap unPair v) $
    glUniform2uiv l . fromIntegral
#endif

instance Uniform [V2 Word32] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen v $ \size p ->
    glProgramUniform2uiv prog l (fromIntegral size) (castPtr p)
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen v $ \size p ->
    glUniform2uiv l (fromIntegral size) (castPtr p)
#endif

instance Uniform [V 2 Word32] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen v $ \size p ->
    glProgramUniform2uiv prog l (fromIntegral size) (castPtr p)
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen v $ \size p ->
    glUniform2uiv l (fromIntegral size) (castPtr p)
#endif

-- D3 array
instance Uniform [(Word32,Word32,Word32)] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen (concatMap unTriple v) $
    glProgramUniform3uiv prog l . fromIntegral
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen (concatMap unTriple v) $
    glUniform3uiv l . fromIntegral
#endif

instance Uniform [V3 Word32] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen v $ \size p ->
    glProgramUniform3uiv prog l (fromIntegral size) (castPtr p)
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen v $ \size p ->
    glUniform3uiv l (fromIntegral size) (castPtr p)
#endif

instance Uniform [V 3 Word32] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen v $ \size p ->
    glProgramUniform3uiv prog l (fromIntegral size) (castPtr p)
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen v $ \size p ->
    glUniform3uiv l (fromIntegral size) (castPtr p)
#endif

-- D4 array
instance Uniform [(Word32,Word32,Word32,Word32)] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen (concatMap unQuad v) $
    glProgramUniform4uiv prog l . fromIntegral
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen (concatMap unQuad v) $
    glUniform4uiv l . fromIntegral
#endif

instance Uniform [V4 Word32] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen v $ \size p ->
    glProgramUniform4uiv prog l (fromIntegral size) (castPtr p)
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen v $ \size p ->
    glUniform4uiv l (fromIntegral size) (castPtr p)
#endif

instance Uniform [V 4 Word32] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen v $ \size p ->
    glProgramUniform4uiv prog l (fromIntegral size) (castPtr p)
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen v $ \size p ->
    glUniform4uiv l (fromIntegral size) (castPtr p)
#endif

--------------------------------------------------------------------------------
-- Float instances -------------------------------------------------------------

-- scalar
instance Uniform Float where
#ifdef __GL45
  toU prog l = pure . U $ glProgramUniform1f prog l
#elif defined(__GL33)
  toU _ l = pure . U $ glUniform1f l
#endif

-- D2
instance Uniform (Float,Float) where
#ifdef __GL45
  toU prog l = pure . U $ \(x,y) -> glProgramUniform2f prog l x y
#elif defined(__GL33)
  toU _ l = pure . U $ \(x,y) -> glUniform2f l x y
#endif

instance Uniform (V2 Float) where
#ifdef __GL45
  toU prog l = pure . U $ \(V2 x y) -> glProgramUniform2f prog l x y
#elif defined(__GL33)
  toU _ l = pure . U $ \(V2 x y) -> glUniform2f l x y
#endif

instance Uniform (V 2 Float) where
#ifdef __GL45
  toU prog l = pure . U $ \(V v) -> case toList v of
    [x,y] -> glProgramUniform2f prog l x y
    _ -> pure ()
#elif defined(__GL33)
  toU _ l = pure . U $ \(V v) -> case toList v of
    [x,y] -> glUniform2f l x y
    _ -> pure ()
#endif

-- D3
instance Uniform (Float,Float,Float) where
#ifdef __GL45
  toU prog l = pure . U $ \(x,y,z) -> glProgramUniform3f prog l x y z
#elif defined(__GL33)
  toU _ l = pure . U $ \(x,y,z) -> glUniform3f l x y z
#endif

instance Uniform (V3 Float) where
#ifdef __GL45
  toU prog l = pure . U $ \(V3 x y z) -> glProgramUniform3f prog l x y z
#elif defined(__GL33)
  toU _ l = pure . U $ \(V3 x y z) -> glUniform3f l x y z
#endif

instance Uniform (V 3 Float) where
#ifdef __GL45
  toU prog l = pure . U $ \(V v) -> case toList v of
    [x,y,z] -> glProgramUniform3f prog l x y z
    _ -> pure ()
#elif defined(__GL33)
  toU _ l = pure . U $ \(V v) -> case toList v of
    [x,y,z] -> glUniform3f l x y z
    _ -> pure ()
#endif

-- D4
instance Uniform (Float,Float,Float,Float) where
#ifdef __GL45
  toU prog l = pure . U $ \(x,y,z,w) -> glProgramUniform4f prog l x y z w
#elif defined(__GL33)
  toU _ l = pure . U $ \(x,y,z,w) -> glUniform4f l x y z w
#endif

instance Uniform (V4 Float) where
#ifdef __GL45
  toU prog l = pure . U $ \(V4 x y z w) -> glProgramUniform4f prog l x y z w
#elif defined(__GL33)
  toU _ l = pure . U $ \(V4 x y z w) -> glUniform4f l x y z w
#endif

instance Uniform (V 4 Float) where
#ifdef __GL45
  toU prog l = pure . U $ \(V v) -> case toList v of
    [x,y,z,w] -> glProgramUniform4f prog l x y z w
    _ -> pure ()
#elif defined(__GL33)
  toU _ l = pure . U $ \(V v) -> case toList v of
    [x,y,z,w] -> glUniform4f l x y z w
    _ -> pure ()
#endif

-- scalar array
instance Uniform [Float] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen v $
    glProgramUniform1fv prog l . fromIntegral
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen v $
    glUniform1fv l . fromIntegral
#endif

-- D2 array
instance Uniform [(Float,Float)] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen (concatMap unPair v) $
    glProgramUniform2fv prog l . fromIntegral
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen (concatMap unPair v) $
    glUniform2fv l . fromIntegral
#endif

instance Uniform [V2 Float] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen v $ \size p ->
    glProgramUniform2fv prog l (fromIntegral size) (castPtr p)
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen v $ \size p ->
    glUniform2fv l (fromIntegral size) (castPtr p)
#endif

instance Uniform [V 2 Float] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen v $ \size p ->
    glProgramUniform2fv prog l (fromIntegral size) (castPtr p)
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen v $ \size p ->
    glUniform2fv l (fromIntegral size) (castPtr p)
#endif

-- D3 array
instance Uniform [(Float,Float,Float)] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen (concatMap unTriple v) $
    glProgramUniform3fv prog l . fromIntegral
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen (concatMap unTriple v) $
    glUniform3fv l . fromIntegral
#endif

instance Uniform [V3 Float] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen v $ \size p ->
    glProgramUniform3fv prog l (fromIntegral size) (castPtr p)
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen v $ \size p ->
    glUniform3fv l (fromIntegral size) (castPtr p)
#endif

instance Uniform [V 3 Float] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen v $ \size p ->
    glProgramUniform3fv prog l (fromIntegral size) (castPtr p)
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen v $ \size p ->
    glUniform3fv l (fromIntegral size) (castPtr p)
#endif

-- D4 array
instance Uniform [(Float,Float,Float,Float)] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen (concatMap unQuad v) $
    glProgramUniform4fv prog l . fromIntegral
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen (concatMap unQuad v) $
    glUniform4fv l . fromIntegral
#endif

instance Uniform [V4 Float] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen v $ \size p ->
    glProgramUniform4fv prog l (fromIntegral size) (castPtr p)
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen v $ \size p ->
    glUniform4fv l (fromIntegral size) (castPtr p)
#endif

instance Uniform [V 4 Float] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen v $ \size p ->
    glProgramUniform4fv prog l (fromIntegral size) (castPtr p)
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen v $ \size p ->
    glUniform4fv l (fromIntegral size) (castPtr p)
#endif

--------------------------------------------------------------------------------
-- Matrices --------------------------------------------------------------------
instance Uniform (M44 Float) where
#ifdef __GL45
  toU prog l = pure . U $ \v -> with v $ glProgramUniformMatrix4fv prog l 1 GL_FALSE . castPtr
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> with v $ glUniformMatrix4fv l 1 GL_FALSE . castPtr
#endif

instance Uniform [M44 Float] where
#ifdef __GL45
  toU prog l = pure . U $ \v -> withArrayLen v $ \size p ->
    glProgramUniformMatrix4fv prog l (fromIntegral size) GL_FALSE (castPtr p)
#elif defined(__GL33)
  toU _ l = pure . U $ \v -> withArrayLen v $ \size p ->
    glUniformMatrix4fv l (fromIntegral size) GL_FALSE (castPtr p)
#endif

--------------------------------------------------------------------------------
-- Textures --------------------------------------------------------------------

#if defined(__GL45) && defined(__GL_BINDLESS_TEXTURES)
instance Uniform (Texture1D f) where
  toU prog l = pure . U $ glProgramUniformHandleui64ARB prog l . baseTextureHnd . texture1DBase

instance Uniform (Texture2D f) where
  toU prog l = pure . U $ glProgramUniformHandleui64ARB prog l . baseTextureHnd . texture2DBase

instance Uniform (Texture3D f) where
  toU prog l = pure . U $ glProgramUniformHandleui64ARB prog l . baseTextureHnd . texture3DBase

instance Uniform (Cubemap f) where
  toU prog l = pure . U $ glProgramUniformHandleui64ARB prog l . baseTextureHnd . cubemapBase
#elif defined(__GL33)
instance (Pixel f) => Uniform (Texture1D f) where
  toU = toUTex
  
instance (Pixel f) => Uniform (Texture2D f) where
  toU = toUTex

instance (Pixel f) => Uniform (Texture3D f) where
  toU = toUTex

instance (Pixel f) => Uniform (Cubemap f) where
  toU = toUTex

toUTex :: forall m tex. (Monad m,Texture tex) => GLuint -> GLint -> UniformInterface m (U tex)
toUTex _ l = do
  texUnit <- nextTextureUnit
  pure . U $ \tex -> do
    debugGL $ glUniform1i l (fromIntegral texUnit) -- FIXME: a bit redundant?
    debugGL $ glActiveTexture (GL_TEXTURE0 + texUnit)
    debugGL $ glBindTexture (textureTypeEnum (Proxy :: Proxy tex)) (baseTextureID $ toBaseTexture tex)
#endif

--------------------------------------------------------------------------------
-- Untuple functions -----------------------------------------------------------

unPair :: (a,a) -> [a]
unPair (x,y) = [x,y]

unTriple :: (a,a,a) -> [a]
unTriple (x,y,z) = [x,y,z]

unQuad :: (a,a,a,a) -> [a]
unQuad (x,y,z,w) = [x,y,z,w]

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
  | InactiveUniform SomeUniformName
    deriving (Eq,Show)

-- |Types that can handle 'ProgramError' – read as, “have”.
class HasProgramError a where
  fromProgramError :: ProgramError -> a
