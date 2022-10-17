{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Numeric.Regression.Internal where

import           Data.Kind
import           Data.Monoid
import qualified Data.Vector         as VB
import qualified Data.Vector.Unboxed as VU


data Acc a = Acc {-# UNPACK #-} !Int !a

instance Semigroup a => Semigroup (Acc a) where
  Acc m a <> Acc n b = Acc (m + n) (a <> b)

instance Monoid a => Monoid (Acc a) where
  mempty = Acc 0 mempty

acc :: a -> Acc (Sum a)
acc = Acc 1 . Sum

dot :: (ModelVector v, Foldable v, Num a)
    => v a
    -> v a
    -> a
dot x y = (+y0) . getSum . foldMap Sum $ fZipWith (*) x y
  where y0 | length x == length y + 1 = fLast x
           | length y == length x + 1 = fLast y
           | otherwise = error $ "dot: Unexpected length in multiplication: " ++ show (fLength x, fLength y)


class ModelVector f where
  fZipWith :: (a -> b -> c) -> f a -> f b -> f c
  fLength :: f a -> Int
  fLast :: f a -> a
  fAppend :: f a -> f a -> f a
  fMap :: (a -> a) -> f a -> f a

instance ModelVector [] where
  fZipWith = zipWith
  fLength = length
  fLast = last
  fAppend = (++)
  fMap = fmap

instance ModelVector VB.Vector where
  fZipWith = VB.zipWith
  fLength = VB.length
  fLast = VB.last
  fAppend = (VB.++)
  fMap = VB.map

-- instance ModelVector VU.Vector where
--   -- type Constr VU.Vector = forall a . (a :: Type) -> VU.Unbox a
--   fZipWith :: (Double -> Double -> Double) -> VU.Vector Double -> VU.Vector Double -> VU.Vector Double
--   fZipWith = VU.zipWith
--   fLength = VU.length
--   fLast = VU.last

-- instance ModelVector VS.Vector where
--   fZipWith :: forall a b c . (VS.Storable a, VS.Storable b, VS.Storable c) => (a -> b -> c) -> VS.Vector a -> VS.Vector b -> VS.Vector c
--   fZipWith = VS.zipWith
