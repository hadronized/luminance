-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

module Graphics.Luminance.Shader.Uniform where

import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Int ( Int32 )
import Data.Word ( Word32 )
import Foreign.Marshal.Array ( withArrayLen )
import Graphics.GL

unPair :: (a,a) -> [a]
unPair (x,y) = [x,y]

unTriple :: (a,a,a) -> [a]
unTriple (x,y,z) = [x,y,z]

unQuad :: (a,a,a,a) -> [a]
unQuad (x,y,z,w) = [x,y,z,w]

class Uniform a where
  uploadUniform :: (MonadIO m) => GLuint -> GLint -> a -> m ()

--------------------------------------------------------------------------------
-- Int32 instances

-- scalar
instance Uniform Int32 where
  uploadUniform = glProgramUniform1i

-- D2
instance Uniform (Int32,Int32) where
  uploadUniform prog l (x,y) = glProgramUniform2i prog l x y

-- D3
instance Uniform (Int32,Int32,Int32) where
  uploadUniform prog l (x,y,z) = glProgramUniform3i prog l x y z

-- D4
instance Uniform (Int32,Int32,Int32,Int32) where
  uploadUniform prog l (x,y,z,w) = glProgramUniform4i prog l x y z w

-- scalar array
instance Uniform [Int32] where
  uploadUniform prog l v = liftIO . withArrayLen v $
    glProgramUniform1iv prog l . fromIntegral

-- D2 array
instance Uniform [(Int32,Int32)] where
  uploadUniform prog l v = liftIO . withArrayLen (concatMap unPair v) $
    glProgramUniform2iv prog l . fromIntegral

-- D3 array
instance Uniform [(Int32,Int32,Int32)] where
  uploadUniform prog l v = liftIO . withArrayLen (concatMap unTriple v) $
    glProgramUniform3iv prog l . fromIntegral

-- D4 array
instance Uniform [(Int32,Int32,Int32,Int32)] where
  uploadUniform prog l v = liftIO . withArrayLen (concatMap unQuad v) $
    glProgramUniform4iv prog l . fromIntegral

--------------------------------------------------------------------------------
-- Word32 instances

-- scalar
instance Uniform Word32 where
  uploadUniform = glProgramUniform1ui

-- D2
instance Uniform (Word32,Word32) where
  uploadUniform prog l (x,y) = glProgramUniform2ui prog l x y

-- D3
instance Uniform (Word32,Word32,Word32) where
  uploadUniform prog l (x,y,z) = glProgramUniform3ui prog l x y z

-- D4
instance Uniform (Word32,Word32,Word32,Word32) where
  uploadUniform prog l (x,y,z,w) = glProgramUniform4ui prog l x y z w

-- scalar array
instance Uniform [Word32] where
  uploadUniform prog l v = liftIO . withArrayLen v $
    glProgramUniform1uiv prog l . fromIntegral

-- D2 array
instance Uniform [(Word32,Word32)] where
  uploadUniform prog l v = liftIO . withArrayLen (concatMap unPair v) $
    glProgramUniform2uiv prog l . fromIntegral

-- D3 array
instance Uniform [(Word32,Word32,Word32)] where
  uploadUniform prog l v = liftIO . withArrayLen (concatMap unTriple v) $
    glProgramUniform3uiv prog l . fromIntegral

-- D4 array
instance Uniform [(Word32,Word32,Word32,Word32)] where
  uploadUniform prog l v = liftIO . withArrayLen (concatMap unQuad v) $
    glProgramUniform4uiv prog l . fromIntegral

--------------------------------------------------------------------------------
-- Float instances

-- scalar
instance Uniform Float where
  uploadUniform = glProgramUniform1f

-- D2
instance Uniform (Float,Float) where
  uploadUniform prog l (x,y) = glProgramUniform2f prog l x y

-- D3
instance Uniform (Float,Float,Float) where
  uploadUniform prog l (x,y,z) = glProgramUniform3f prog l x y z

-- D4
instance Uniform (Float,Float,Float,Float) where
  uploadUniform prog l (x,y,z,w) = glProgramUniform4f prog l x y z w

-- scalar array
instance Uniform [Float] where
  uploadUniform prog l v = liftIO . withArrayLen v $
    glProgramUniform1fv prog l . fromIntegral

-- D2 array
instance Uniform [(Float,Float)] where
  uploadUniform prog l v = liftIO . withArrayLen (concatMap unPair v) $
    glProgramUniform2fv prog l . fromIntegral

-- D3 array
instance Uniform [(Float,Float,Float)] where
  uploadUniform prog l v = liftIO . withArrayLen (concatMap unTriple v) $
    glProgramUniform3fv prog l . fromIntegral

-- D4 array
instance Uniform [(Float,Float,Float,Float)] where
  uploadUniform prog l v = liftIO . withArrayLen (concatMap unQuad v) $
    glProgramUniform4fv prog l . fromIntegral
