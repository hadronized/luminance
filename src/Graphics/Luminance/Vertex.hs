-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

module Graphics.Luminance.Vertex where

import Data.Foldable ( toList )
import Data.Int ( Int32 )
import Data.Word ( Word32 )

data Component f
  = Ints (f Int32)
  | UInts (f Word32)
  | Floats (f Float)

instance (Foldable f) => Show (Component f) where
  show c = case c of
    Ints   v -> show $ toList v
    UInts  v -> show $ toList v
    Floats v -> show $ toList v

data D a
  = D1 !a
  | D2 !a !a
  | D3 !a !a !a
  | D4 !a !a !a !a
    deriving (Eq,Functor,Foldable,Ord,Show)

newtype Vertex = Vertex { unVertex :: [Component D] } deriving (Show)
