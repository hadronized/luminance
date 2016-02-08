{-# LANGUAGE DeriveFunctor #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015, 2016 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Core.GPU where

import Control.Arrow ( (&&&) )
import Data.Function ( fix )

data GPU f a = GPU { runGPU :: f (a,GPU f a) } deriving (Functor)

instance (Applicative f) => Applicative (GPU f) where
  pure x = fix $ \g -> GPU $ pure (x,g)
  f <*> a = GPU $ (\(f',fn) (a',an) -> (f' a',fn <*> an)) <$> runGPU f <*> runGPU a

instance (Monad m) => Monad (GPU m) where
  return = pure
  x >>= f = GPU $ runGPU x >>= runGPU . f . fst

once :: (Applicative f) => f a -> GPU f a
once = GPU . fmap (id &&& pure)
