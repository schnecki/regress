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
dot theta x = (+y0) . getSum . foldMap Sum $ fZipWith (*) theta x
  where y0 | length theta == length x + 1 = fLast theta -- intercept
           | otherwise = error $ "dot: Unexpected length in multiplication: " ++ show (fLength theta, fLength x) ++ ". Expected (n, n-1) for (theta, x)"


class ModelVector f where
  fZipWith :: (a -> b -> c) -> f a -> f b -> f c
  fLength :: f a -> Int
  fLast :: f a -> a
  fAppend :: f a -> f a -> f a
  fMap :: (a -> a) -> f a -> f a
  fTake :: Int -> f a -> f a
  fInit :: f a -> f a
  fSnoc :: f a -> a -> f a

instance ModelVector [] where
  fZipWith = zipWith
  fLength = length
  fLast = last
  fAppend = (++)
  fMap = fmap
  fTake = take
  fInit = init
  fSnoc xs = (xs ++) . pure

instance ModelVector VB.Vector where
  fZipWith = VB.zipWith
  fLength = VB.length
  fLast = VB.last
  fAppend = (VB.++)
  fMap = VB.map
  fTake = VB.take
  fInit = VB.init
  fSnoc = VB.snoc

-- instance ModelVector VU.Vector where
--   -- type Constr VU.Vector = forall a . (a :: Type) -> VU.Unbox a
--   fZipWith :: (Double -> Double -> Double) -> VU.Vector Double -> VU.Vector Double -> VU.Vector Double
--   fZipWith = VU.zipWith
--   fLength = VU.length
--   fLast = VU.last

-- instance ModelVector VS.Vector where
--   fZipWith :: forall a b c . (VS.Storable a, VS.Storable b, VS.Storable c) => (a -> b -> c) -> VS.Vector a -> VS.Vector b -> VS.Vector c
--   fZipWith = VS.zipWith
