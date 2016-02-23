{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015, 2016 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-----------------------------------------------------------------------------

module Graphics.Luminance.Core.Driver where

import Control.Monad.Except ( MonadError )
import Data.Word ( Word32 )
import GHC.Exts ( Constraint )
import Graphics.Luminance.Core.Framebuffer ( HasFramebufferError )
import Graphics.Luminance.Core.Geometry ( GeometryMode )
import Graphics.Luminance.Core.RW ( Readable, RW, Writable )
import Graphics.Luminance.Core.Shader.Program ( HasProgramError )
import Graphics.Luminance.Core.Shader.Stage ( HasStageError, StageType )
import Numeric.Natural ( Natural )

class (Monad m) => Driver m where
  -- buffers
  type BuildBuffer m :: * -> * -> *
  type Buffer m :: * -> * -> *
  createRegion :: Natural -> BuildBuffer m rw (Buffer m rw a)
  createBuffer :: BuildBuffer m rw a -> m a
  readWhole    :: Buffer m r a -> m [a]
  writeWhole   :: Buffer m w a -> f a -> m ()
  fill         :: Buffer m w a -> a -> m ()
  (@?)         :: Buffer m r a -> Natural -> m (Maybe a)
  (@!)         :: Buffer m r a -> Natural -> m a
  writeAt      :: Buffer m w a -> Natural -> a -> m ()
  writeAt'     :: Buffer m w a -> Natural -> a -> m ()
  -- textures
  type Filter m :: *
  -- framebuffers
  type Framebuffer m :: * -> * -> * -> *
  type FramebufferColorAttachment m :: * -> Constraint
  type FramebufferDepthAttachment m :: * -> Constraint
  type FramebufferBlitMask m :: *
  createFramebuffer  :: (FramebufferColorAttachment m c,FramebufferDepthAttachment m d,HasFramebufferError e,MonadError e m)
                     => Natural
                     -> Natural
                     -> Natural
                     -> m (Framebuffer m rw c d)
  defaultFramebuffer :: m (Framebuffer m RW () ())
  framebufferBlit    :: (Readable r,Writable w)
                     => Framebuffer m r c d
                     -> Framebuffer m w c' d'
                     -> Int
                     -> Int
                     -> Natural
                     -> Natural
                     -> Int
                     -> Int
                     -> Natural
                     -> Natural
                     -> FramebufferBlitMask m
                     -> Filter m
                     -> m ()
  -- geometries
  type Geometry m :: *
  type Vertex m :: * -> Constraint
  createGeometry :: (Foldable f,Vertex m v)
                 => f v
                 -> Maybe (f Word32)
                 -> GeometryMode
                 -> m (Geometry m)
  -- shader stages
  type Stage m :: *
  createStage :: (HasStageError e,MonadError e m)
              => StageType
              -> String
              -> m (Stage m)
  type Program m :: * -> *
  type UniformName m :: * -> *
  type UniformInterface m :: * -> *
  type U m :: * -> *
  type U' m :: *
  createProgram :: (HasProgramError e,MonadError e m)
                => [Stage m]
                -> ((forall a. UniformName m a -> UniformInterface m (U m a)) -> UniformInterface m i)
                -> m (Program m i)
  updateUniform :: Program m a -> (a -> U' m) -> m ()
  -- draw
  type Output m :: * -> * -> *
  draw :: (Writable w) => Framebuffer m w c d -> [(Program m a,a -> U' m,[a -> (U' m,Geometry m)])] -> m (Output m c d)
