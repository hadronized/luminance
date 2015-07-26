-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Framebuffer where

import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Resource ( MonadResource, register )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Utils ( with )
import Foreign.Storable ( peek )
import Graphics.GL
import Graphics.Luminance.RW

newtype Framebuffer rw c d = Framebuffer { framebufferID :: GLint } deriving (Eq,Show)

type ColorFramebuffer rw c = Framebuffer rw c ()
type DepthFramebuffer rw d = Framebuffer rw () d

-- |A chain of types, right-associated.
data a :. b = a :. b deriving (Eq,Functor,Ord,Show)

infixr 6 :.

createFramebuffer :: (MonadIO m,MonadResource m)
                  => m (Framebuffer rw c d)
createFramebuffer = do
  fid <- liftIO . alloca $ \p -> do
    glGenFramebuffers 1 p
    peek p
  _ <- register . with fid $ glDeleteFramebuffers 1
  pure $ Framebuffer (fromIntegral fid)

{-
class FramebufferAttachment c d where
  createFramebufferTextures :: (MonadIO m) => Framebuffer rw c d -> m ()

instance FramebufferAttachment () d where
  createFramebufferTextures = createFramebufferDepthTextures
-}

{-
createFramebufferDepthTextures :: (ChannelSize s,ChannelType t,MonadIO m)
                               => Framebuffer rw () (Format t (CDepth s))
                               -> m ()
createFramebufferDepthTextures framebuffer = do
-}
