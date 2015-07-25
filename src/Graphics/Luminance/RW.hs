-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.RW where

class Readable r where
class Writable w where

data R  = R  deriving (Eq,Ord,Show)
data W  = W  deriving (Eq,Ord,Show)
data RW = RW deriving (Eq,Ord,Show)

instance Readable R
instance Writable W
instance Readable RW
instance Writable RW
