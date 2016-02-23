{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}

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
import Graphics.Luminance.Core.Buffer ( BuildBuffer, Buffer )
import Graphics.Luminance.Core.Framebuffer (Framebuffer, FramebufferBlitMask
                                           , FramebufferColorAttachment, FramebufferDepthAttachment
                                           , HasFramebufferError )
import Graphics.Luminance.Core.Geometry ( Geometry, GeometryMode )
import Graphics.Luminance.Core.RW ( Readable, RW, Writable )
import Graphics.Luminance.Core.Shader.Program ( HasProgramError, Program, U, UniformInterface
                                              , UniformName )
import Graphics.Luminance.Core.Shader.Stage ( HasStageError, Stage, StageType )
import Graphics.Luminance.Core.Texture ( Filter )
import Graphics.Luminance.Core.Vertex ( Vertex )
import Numeric.Natural ( Natural )


class (Monad m) => Driver m where
  -- buffers
  createBuffer :: BuildBuffer rw a -> m a
  readWhole    :: Buffer r a -> m [a]
  writeWhole   :: Buffer w a -> f a -> m ()
  fill         :: Buffer w a -> a -> m ()
  (@?)         :: Buffer r a -> Natural -> m (Maybe a)
  (@!)         :: Buffer r a -> Natural -> m a
  writeAt      :: Buffer w a -> Natural -> a -> m ()
  writeAt'     :: Buffer w a -> Natural -> a -> m ()
  -- framebuffers
  createFramebuffer  :: (FramebufferColorAttachment c,FramebufferDepthAttachment d,HasFramebufferError e,MonadError e m)
                     => Natural
                     -> Natural
                     -> Natural
                     -> m (Framebuffer rw c d)
  defaultFramebuffer :: m (Framebuffer RW () ())
  framebufferBlit    :: (Readable r,Writable w)
                     => Framebuffer r c d
                     -> Framebuffer w c' d'
                     -> Int
                     -> Int
                     -> Natural
                     -> Natural
                     -> Int
                     -> Int
                     -> Natural
                     -> Natural
                     -> FramebufferBlitMask
                     -> Filter
                     -> m ()
  -- geometries
  createGeometry :: (Foldable f,Vertex v)
                 => f v
                 -> Maybe (f Word32)
                 -> GeometryMode
                 -> m Geometry
  -- shader stages
  createStage :: (HasStageError e,MonadError e m)
              => StageType
              -> String
              -> m Stage
  createProgram :: (HasProgramError e,MonadError e m)
                => [Stage]
                -> ((forall a. UniformName a -> UniformInterface m (U a)) -> UniformInterface m i)
                -> m (Program i)

-- |A simpler version of 'createProgram'. That function assumes you don’t need a uniform interface
-- and then just returns the 'Program'.
createProgram_ :: (Driver m,HasProgramError e,MonadError e m)
                => [Stage]
                -> m (Program ())
createProgram_ stages = createProgram stages (\_ -> pure ())
