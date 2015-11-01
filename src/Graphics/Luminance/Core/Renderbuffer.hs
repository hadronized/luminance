{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Core.Renderbuffer where

import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Resource ( MonadResource, register )
import Data.Proxy ( Proxy )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Utils ( with )
import Foreign.Storable ( peek )
import Graphics.GL
import Graphics.Luminance.Core.Pixel ( Pixel(pixelIFormat) )
import Numeric.Natural ( Natural )

newtype Renderbuffer = Renderbuffer { renderbufferID :: GLuint } deriving (Eq,Show)

createRenderbuffer :: (MonadIO m,MonadResource m,Pixel f) => Natural -> Natural -> Proxy f -> m Renderbuffer
#if GL45_BACKEND
createRenderbuffer w h depthProxy = do
  rid <- liftIO . alloca $ \p -> do
    glCreateRenderbuffers 1 p
    rid <- peek p
    glNamedRenderbufferStorage rid (pixelIFormat depthProxy) (fromIntegral w) (fromIntegral h)
    pure rid
  _ <- register $ with rid (glDeleteRenderbuffers 1)
  pure (Renderbuffer rid)
#elif GL32_BACKEND
createRenderbuffer w h depthProxy = do
  rid <- liftIO . alloca $ \p -> do
    glGenRenderbuffers 1 p
    rid <- peek p
    glRenderbufferStorage GL_RENDERBUFFER (pixelIFormat depthProxy) (fromIntegral w) (fromIntegral h)
    pure rid
  _ <- register $ with rid (glDeleteRenderbuffers 1)
  pure (Renderbuffer rid)
#endif
