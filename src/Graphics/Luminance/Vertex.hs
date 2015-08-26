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

import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Proxy ( Proxy(..) )
import Foreign.Marshal.Array ( peekArray, pokeArray )
import Foreign.Ptr ( Ptr, castPtr )
import Foreign.Storable ( Storable(..) )
import GHC.TypeLits ( KnownNat, Nat, natVal )
import Graphics.GL
import Graphics.Luminance.GPU
import Graphics.Luminance.Tuple

data V :: Nat -> * -> * where
  V1 :: !a -> V 1 a
  V2 :: !a -> !a -> V 2 a
  V3 :: !a -> !a -> !a -> V 3 a
  V4 :: !a -> !a -> !a -> !a -> V 4 a

instance (Storable a) => Storable (V 1 a) where
  sizeOf _ = sizeOf (undefined :: a)
  alignment _ = alignment (undefined :: a)
  peek p = fmap V1 $ peek (castPtr p :: Ptr a)
  poke p (V1 x) = poke (castPtr p) x

instance (Storable a) => Storable (V 2 a) where
  sizeOf _ = 2 * sizeOf (undefined :: a)
  alignment _ = alignment (undefined :: a)
  peek p = do
    [x,y] <- peekArray 2 (castPtr p :: Ptr a)
    pure $ V2 x y
  poke p (V2 x y) = pokeArray (castPtr p) [x,y]

instance (Storable a) => Storable (V 3 a) where
  sizeOf _ = 3 * sizeOf (undefined :: a)
  alignment _ = alignment (undefined :: a)
  peek p = do
    [x,y,z] <- peekArray 3 (castPtr p :: Ptr a)
    pure $ V3 x y z
  poke p (V3 x y z) = pokeArray (castPtr p) [x,y,z]

instance (Storable a) => Storable (V 4 a) where
  sizeOf _ = 4 * sizeOf (undefined :: a)
  alignment _ = alignment (undefined :: a)
  peek p = do
    [x,y,z,w] <- peekArray 4 (castPtr p :: Ptr a)
    pure $ V4 x y z w
  poke p (V4 x y z w) = pokeArray (castPtr p) [x,y,z,w]

class Vertex v where
  setFormatV :: (MonadIO m) => GLuint -> GLuint -> proxy v -> m ()

instance (GPU a,KnownNat n,Storable a) => Vertex (V n a) where
  setFormatV vid index _ = do
    glVertexArrayAttribFormat vid index (fromIntegral $ natVal (Proxy :: Proxy n)) (glType (Proxy :: Proxy a)) GL_FALSE 0
    glVertexArrayAttribBinding vid index vertexBindingIndex
    glEnableVertexArrayAttrib vid index

instance (Vertex a,Vertex b) => Vertex (a :. b) where
  setFormatV vid index _ = do
    setFormatV vid index (Proxy :: Proxy a)
    setFormatV vid index (Proxy :: Proxy b)

vertexBindingIndex :: GLuint
vertexBindingIndex = 0
