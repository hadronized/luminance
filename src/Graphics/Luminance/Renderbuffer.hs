-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Renderbuffer where

import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Resource ( MonadResource, register )
import Foreign.Marshal.Alloca ( alloca )
import Foreign.Storable ( peek )
import Graphics.GL

newtype Renderbuffer = Renderbuffer { renderbufferID :: GLuint } deriving (Eq,Show)

createRenderbuffer :: (MonadIO m,MonadResource m,Pixel f) => Natural -> Natural -> Proxy f -> m Renderbuffer
createRenderbuffer w h _ = do
  rid <- liftIO . alloca $ \p -> do
    glCreateRenderbuffers 1 p
    peek p
  _ <- register . with rid $ glDeleteRenderbuffers 1
  pure $ Renderbuffer rid
