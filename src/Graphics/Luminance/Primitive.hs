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

newtype Point a = Point { pointIndex :: Index a } deriving (Eq,Ord,Show)

data Line a = Line {
    lineIndexA :: Index a
  , lineIndexB :: Index a
  } deriving (Eq,Ord,Show)

data Triangle a = Triangle {
    triangleIndexA :: Index a
  , triangleIndexB :: Index a
  , triangleIndexC :: Index a
  } deriving (Eq,Ord,Show)
