{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015, 2016 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

module Control.Some (
    -- * Free existential quantification
    Some(..)
  ) where

data Some :: (k -> *) -> * where
  Some :: f a -> Some f
