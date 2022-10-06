module Numeric.Regression.Internal where

import           Control.Applicative
import           Data.Foldable
import           Data.Monoid
import qualified Data.Vector         as VB


data Acc a = Acc {-# UNPACK #-} !Int !a

instance Semigroup a => Semigroup (Acc a) where
  Acc m a <> Acc n b = Acc (m + n) (a <> b)

instance Monoid a => Monoid (Acc a) where
  mempty = Acc 0 mempty

acc :: a -> Acc (Sum a)
acc = Acc 1 . Sum

dot :: (ModelVector v, Applicative v, Foldable v, Num a)
    => v a
    -> v a
    -> a
dot x y = (+y0) . getSum . foldMap Sum $ fZipWith (*) x y
  where y0 | length x == length y + 1 = last xs
           | length y == length x + 1 = last ys
           | otherwise = 0
        xs = toList x
        ys = toList y


class ModelVector f where
  fZipWith :: (a -> b -> c) -> f a -> f b -> f c
  fLength :: f a -> Int
  fLast :: f a -> a

instance ModelVector [] where
  fZipWith = zipWith
  fLength = length
  fLast = last
instance ModelVector VB.Vector where
  fZipWith = VB.zipWith
  fLength = VB.length
  fLast = VB.last
-- instance ModelVector VS.Vector where
--   fZipWith :: forall a b c . (VS.Storable a, VS.Storable b, VS.Storable c) => (a -> b -> c) -> VS.Vector a -> VS.Vector b -> VS.Vector c
--   fZipWith = VS.zipWith
