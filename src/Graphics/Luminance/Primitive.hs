-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

module Graphics.Luminance.Primitive where

import Graphics.Luminance.Index ( Index )

newtype Point = Point { pointIndex :: Index } deriving (Eq,Ord,Show)

data Line = Line {
    lineIndexA :: Index
  , lineIndexB :: Index
  } deriving (Eq,Ord,Show)

data Triangle = Triangle {
    triangleIndexA :: Index
  , triangleIndexB :: Index
  , triangleIndexC :: Index
  } deriving (Eq,Ord,Show)
