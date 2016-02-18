-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015, 2016 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.Luminance.Core.RW where

-- |Readable typeclass, for types that admit reads.
class Readable r where

-- |Writable typeclass, for types that admit writes.
class Writable w where

-- |Read-only type.
data R = R deriving (Eq,Ord,Show)

instance Readable R

-- |Write-only type.
data W = W deriving (Eq,Ord,Show)

instance Writable W

-- |Read-write type.
data RW = RW deriving (Eq,Ord,Show)

instance Readable RW
instance Writable RW
