-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.GLResource (
    -- * Managing OpenGL resources
    Managed
  , runManaged
  , register
  ) where

import Control.Monad.Trans ( MonadTrans )
import Control.Monad.Codensity ( Codensity(..), lowerCodensity )
import Control.Monad.IO.Class ( MonadIO )

newtype Managed m a = Managed {
    unManaged :: Codensity m a
  } deriving (Applicative,Functor,Monad,MonadIO,MonadTrans)

runManaged :: (Monad m) => Managed m a -> m a
runManaged = lowerCodensity . unManaged

register :: (MonadIO m) => m () -> m a -> Managed m a
register fin gen = Managed $ Codensity $ \k -> do
  a <- gen
  r <- k a
  fin
  pure r
