{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveTraversable #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-----------------------------------------------------------------------

module Graphics.Luminance.Core.Shader.UniformBlock where

import Control.Monad.IO.Class ( MonadIO(..) ) 
import Data.Int ( Int32 )
import Data.Proxy ( Proxy(..) )
import Data.Word ( Word32 )
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable(..), peekByteOff, pokeByteOff )
import GHC.Generics
import Graphics.Luminance.Core.Tuple ( (:.) )
import Linear.V2 ( V2 )
import Linear.V3 ( V3 )
import Linear.V4 ( V4 )

--------------------------------------------------------------------------------
-- UB wrapper type -------------------------------------------------------------

newtype UB a = UB { unUB :: a } deriving (Eq,Foldable,Functor,Ord,Show,Traversable)

instance (UniformBlock a) => Storable (UB a) where
  alignment _ = alignmentSTD140 (Proxy :: Proxy a)
  sizeOf _ = sizeOfSTD140 (Proxy :: Proxy a)
  peekByteOff p o = fmap UB (peekSTD140 p o)
  pokeByteOff p o = pokeSTD140 p o . unUB

--------------------------------------------------------------------------------
-- Uniform block ---------------------------------------------------------------

class UniformBlock a where
  isStruct :: proxy a -> Bool
  isStruct _ = True

  alignmentSTD140 :: proxy a -> Int
  default alignmentSTD140 :: (Generic a,GUniformBlock (Rep a)) => proxy a -> Int
  alignmentSTD140 _ = galignmentSTD140 (Proxy :: Proxy (Rep a))

  sizeOfSTD140 :: proxy a -> Int
  default sizeOfSTD140 :: (Generic a,GUniformBlock (Rep a)) => proxy a -> Int
  sizeOfSTD140 _ = gsizeOfSTD140 (Proxy :: Proxy (Rep a))

  paddingSTD140 :: proxy a -> Int
  default paddingSTD140 :: (Generic a,GUniformBlock (Rep a)) => proxy a -> Int
  paddingSTD140 _ = gpaddingSTD140 (Proxy :: Proxy (Rep a))

  peekSTD140 :: (MonadIO m) => Ptr b -> Int -> m a
  default peekSTD140 :: (Generic a,GUniformBlock (Rep a),MonadIO m) => Ptr b -> Int -> m a
  peekSTD140 p o = liftIO $ fmap to (gpeekSTD140 p o)

  pokeSTD140 :: (MonadIO m) => Ptr b -> Int -> a -> m ()
  default pokeSTD140 :: (Generic a,GUniformBlock (Rep a),MonadIO m) => Ptr b -> Int -> a -> m ()
  pokeSTD140 p o a = liftIO $ gpokeSTD140 p o (from a)

roundUp :: Int -> Int -> Int
roundUp u a = a + mod (u - a) u

--------------------------------------------------------------------------------
-- Generic UniformBlock --------------------------------------------------------

class GUniformBlock f where
  galignmentSTD140 :: proxy f -> Int
  gsizeOfSTD140 :: proxy f -> Int
  gpaddingSTD140 :: proxy f -> Int
  gpeekSTD140 :: (MonadIO m) => Ptr b -> Int -> m (f a)
  gpokeSTD140 :: (MonadIO m) => Ptr b -> Int -> f a -> m ()

instance GUniformBlock U1 where
  galignmentSTD140 _ = 1
  gsizeOfSTD140 _ = 0
  gpaddingSTD140 _ = 0
  gpeekSTD140 _ _ = pure U1
  gpokeSTD140 _ _ _ = pure ()

instance (GUniformBlock f,GUniformBlock g) => GUniformBlock (f :*: g) where
  galignmentSTD140 _ = galignmentSTD140 (Proxy :: Proxy f) `max` galignmentSTD140 (Proxy :: Proxy g)
  gsizeOfSTD140 _ = gsizeOfSTD140 (Proxy :: Proxy f) - gpaddingSTD140 (Proxy :: Proxy f) + gsizeOfSTD140 (Proxy :: Proxy g)
  gpaddingSTD140 _ = gsizeOfSTD140 (Proxy :: Proxy (f :*: g)) `rem` 16
  gpeekSTD140 p o = liftIO $
        (:*:)
    <$> gpeekSTD140 p o
    <*> gpeekSTD140 p (o + gsizeOfSTD140 (Proxy :: Proxy f) - gpaddingSTD140 (Proxy :: Proxy f))
  gpokeSTD140 p o (f :*: g) = liftIO $ do
    gpokeSTD140 p o f
    gpokeSTD140 p (o + gsizeOfSTD140 (Proxy :: Proxy f) - gpaddingSTD140 (Proxy :: Proxy f)) g

instance (GUniformBlock f) => GUniformBlock (D1 c f) where
  galignmentSTD140 _ = galignmentSTD140 (Proxy :: Proxy f)
  gsizeOfSTD140 _ = gsizeOfSTD140 (Proxy :: Proxy f)
  gpaddingSTD140 _ = gpaddingSTD140 (Proxy :: Proxy f)
  gpeekSTD140 p o = fmap M1 (gpeekSTD140 p o)
  gpokeSTD140 p o (M1 a) = gpokeSTD140 p o a

instance (GUniformBlock f) => GUniformBlock (C1 c f) where
  galignmentSTD140 _ = galignmentSTD140 (Proxy :: Proxy f)
  gsizeOfSTD140 _ = gsizeOfSTD140 (Proxy :: Proxy f)
  gpaddingSTD140 _ = gpaddingSTD140 (Proxy :: Proxy f)
  gpeekSTD140 p o = fmap M1 (gpeekSTD140 p o)
  gpokeSTD140 p o (M1 a) = gpokeSTD140 p o a

instance (GUniformBlock f) => GUniformBlock (S1 c f) where
  galignmentSTD140 _ = galignmentSTD140 (Proxy :: Proxy f)
  gsizeOfSTD140 _ = gsizeOfSTD140 (Proxy :: Proxy f)
  gpaddingSTD140 _ = gpaddingSTD140 (Proxy :: Proxy f)
  gpeekSTD140 p o = fmap M1 (gpeekSTD140 p o)
  gpokeSTD140 p o (M1 a) = gpokeSTD140 p o a

instance (UniformBlock c) => GUniformBlock (K1 i c) where
  galignmentSTD140 _ = alignmentSTD140 (Proxy :: Proxy c)
  gsizeOfSTD140 _ = sizeOfSTD140 (Proxy :: Proxy c)
  gpaddingSTD140 _ = paddingSTD140 (Proxy :: Proxy c)
  gpeekSTD140 p o = fmap K1 (peekSTD140 p o)
  gpokeSTD140 p o (K1 a) = pokeSTD140 p o a

--------------------------------------------------------------------------------
-- Basic instances -------------------------------------------------------------

instance UniformBlock Int32 where
  isStruct _ = False
  alignmentSTD140 _ = 4
  sizeOfSTD140 _ = 4
  paddingSTD140 _ = 0
  peekSTD140 p o = liftIO (peekByteOff p o)
  pokeSTD140 p o a = liftIO (pokeByteOff p o a)

instance UniformBlock Word32 where
  isStruct _ = False
  alignmentSTD140 _ = 4
  sizeOfSTD140 _ = 4
  paddingSTD140 _ = 0
  peekSTD140 p o = liftIO (peekByteOff p o)
  pokeSTD140 p o a = liftIO (pokeByteOff p o a)

instance UniformBlock Float where
  isStruct _ = False
  alignmentSTD140 _ = 4
  sizeOfSTD140 _ = 4
  paddingSTD140 _ = 0
  peekSTD140 p o = liftIO (peekByteOff p o)
  pokeSTD140 p o a = liftIO (pokeByteOff p o a)

instance UniformBlock Bool where
  isStruct _ = False
  alignmentSTD140 _ = 4
  sizeOfSTD140 _ = 4
  paddingSTD140 _ = 0
  peekSTD140 p o = liftIO (fmap toBool $ peekByteOff p o)
  pokeSTD140 p o a = liftIO (pokeByteOff p o $ fromBool a)

instance (Storable a,UniformBlock a) => UniformBlock (V2 a) where
  isStruct _ = False
  alignmentSTD140 _ = alignmentSTD140 (Proxy :: Proxy a) * 2
  sizeOfSTD140 _ = sizeOfSTD140 (Proxy :: Proxy a) * 2
  paddingSTD140 _ = 0
  peekSTD140 p o = liftIO (peekByteOff p o)
  pokeSTD140 p o a = liftIO (pokeByteOff p o a)

instance (Storable a,UniformBlock a) => UniformBlock (V3 a) where
  isStruct _ = False
  alignmentSTD140 _ = alignmentSTD140 (Proxy :: Proxy a) * 4
  sizeOfSTD140 _ = sizeOfSTD140 (Proxy :: Proxy a) * 4
  paddingSTD140 _ = sizeOfSTD140 (Proxy :: Proxy a)
  peekSTD140 p o = liftIO (peekByteOff p o)
  pokeSTD140 p o a = liftIO (pokeByteOff p o a)

instance (Storable a,UniformBlock a) => UniformBlock (V4 a) where
  isStruct _ = False
  alignmentSTD140 _ = alignmentSTD140 (Proxy :: Proxy a) * 4
  sizeOfSTD140 _ = sizeOfSTD140 (Proxy :: Proxy a) * 4
  paddingSTD140 _ = 0
  peekSTD140 p o = liftIO (peekByteOff p o)
  pokeSTD140 p o a = liftIO (pokeByteOff p o a)

instance (UniformBlock a,UniformBlock b) => UniformBlock (a :. b) where

instance (UniformBlock a,UniformBlock b) => UniformBlock (a,b)
instance (UniformBlock a,UniformBlock b,UniformBlock c) => UniformBlock (a,b,c)
instance (UniformBlock a,UniformBlock b,UniformBlock c,UniformBlock d) => UniformBlock (a,b,c,d)

fromBool :: Bool -> Int32
fromBool False = 0
fromBool True = 1

toBool :: Int32 -> Bool
toBool 0 = False
toBool _ = True
