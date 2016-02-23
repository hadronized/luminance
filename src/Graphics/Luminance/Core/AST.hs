{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Graphics.Luminance.Core.AST where

import Control.Monad.Free ( Free, liftF )
import Data.Word ( Word32 )
import Graphics.Luminance.Core.Buffer ( BuildBuffer, Buffer )
import Graphics.Luminance.Core.Framebuffer ( Framebuffer, FramebufferBlitMask
                                           , FramebufferColorAttachment, FramebufferDepthAttachment
                                           , FramebufferError )
import Graphics.Luminance.Core.Geometry ( Geometry, GeometryMode )
import Graphics.Luminance.Core.RW ( Readable, RW, Writable )
import Graphics.Luminance.Core.Shader.Program ( Program, ProgramError, U, U', UniformInterface
                                              , UniformName )
import Graphics.Luminance.Core.Shader.Stage ( Stage, StageError, StageType )
import Graphics.Luminance.Core.Texture ( Filter )
import Graphics.Luminance.Core.Vertex ( Vertex )
import Numeric.Natural ( Natural )

newtype AST a = AST { runAST :: Free ASTF a }

instance Functor AST where
  fmap f = AST . fmap f . runAST

data ASTF :: * -> * where
  -- buffers
  BufferCreate     :: BuildBuffer rw a -> (a -> n) -> ASTF n
  BufferReadWhole  :: Buffer r a -> ([a] -> n) -> ASTF n
  BufferWriteWhole :: Buffer w a -> f a -> n -> ASTF n
  BufferFill       :: Buffer w a -> a -> n -> ASTF n
  BufferGetAt      :: Buffer r a -> Natural -> (Maybe a -> n) -> ASTF n
  BufferGetAt'     :: Buffer r a -> Natural -> (a -> n) -> ASTF n
  BufferWriteAt    :: Buffer w a -> Natural -> a -> n -> ASTF n
  BufferWriteAt'   :: Buffer w a -> Natural -> a -> n -> ASTF n
  -- framebuffers
  FramebufferCreate  :: (FramebufferColorAttachment c,FramebufferDepthAttachment d)
                     => Natural
                     -> Natural
                     -> Natural
                     -> (Either FramebufferError (Framebuffer rw c d) -> n)
                     -> ASTF n
  FramebufferDefault :: (Framebuffer RW () () -> n) -> ASTF n
  FramebufferBlit    :: (Readable r,Writable w)
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
                     -> n
                     -> ASTF n
  -- geometries
  GeometryCreate :: (Foldable f,Vertex v)
                 => f v
                 -> Maybe (f Word32)
                 -> GeometryMode
                 -> (Geometry -> n)
                 -> ASTF n
  -- shaders
  ShaderStageCreate :: StageType -> String -> (Either StageError Stage -> n) -> ASTF n
  ShaderProgramCreate :: [Stage]
                      -> ((forall a. UniformName a -> UniformInterface m (U a)) -> UniformInterface m i)
                      -> (Either ProgramError (Program i) -> n)
                      -> ASTF n
  ShaderUniformUpdate :: Program a -> (a -> U') -> n -> ASTF n

instance Functor ASTF where
  fmap f ast = case ast of
    BufferCreate bf g -> BufferCreate bf (f . g)
    BufferReadWhole b g -> BufferReadWhole b (f . g)
    BufferWriteWhole b x n -> BufferWriteWhole b x (f n)
    BufferFill b x n -> BufferFill b x (f n)
    BufferGetAt b i g -> BufferGetAt b i (f . g)
    BufferGetAt' b i g -> BufferGetAt' b i (f . g)
    BufferWriteAt b i x n -> BufferWriteAt b i x (f n)
    BufferWriteAt' b i x n -> BufferWriteAt' b i x (f n)
    FramebufferCreate w h mm g -> FramebufferCreate w h mm (f . g)
    FramebufferDefault g -> FramebufferDefault (f . g)
    FramebufferBlit src dst srcX srcY srcW srcH dstX dstY dstW dstH mask flt n ->
      FramebufferBlit src dst srcX srcY srcW srcH dstX dstY dstW dstH mask flt (f n)
    GeometryCreate v i m g -> GeometryCreate v i m (f . g)
    ShaderStageCreate st src g -> ShaderStageCreate st src (f . g)
    ShaderProgramCreate st ui g -> ShaderProgramCreate st ui (f . g)
    ShaderUniformUpdate p u n -> ShaderUniformUpdate p u (f n)

