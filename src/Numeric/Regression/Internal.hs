{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Numeric.Regression.Internal where

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
           | otherwise = error $ "dot: Unexpected length in multiplication: " ++ show (fLength theta, fLength x) ++ ". Expected (n, n-1) for (theta, x). See also dot' for a version without intercept."

dot' :: (ModelVector v, Foldable v, Num a)
    => v a
    -> v a
    -> a
dot' theta x
  | length theta == length x = getSum . foldMap Sum $ fZipWith (*) theta x
  | otherwise = error $ "dot': Unexpected length in multiplication: " ++ show (fLength theta, fLength x) ++ ". Expected (n, n) for (theta, x). See also dot for a version with intercept."


class ModelVector f where
  fNull :: f a -> Bool
  fReplicate :: Int -> a -> f a
  fIdx :: f a -> Int -> a
  fFromList :: [a] -> f a
  fToList :: f a -> [a]
  fZipWith :: (a -> b -> c) -> f a -> f b -> f c
  fZipWith3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
  fLength :: f a -> Int
  fLast :: f a -> a
  fAppend :: f a -> f a -> f a
  fMap :: (a -> a) -> f a -> f a
  fTake :: Int -> f a -> f a
  fDrop :: Int -> f a -> f a
  fInit :: f a -> f a
  fSnoc :: f a -> a -> f a
  fConcatList :: [f a] -> f a

instance ModelVector [] where
  fNull = null
  fReplicate = replicate
  fIdx = (!!)
  fFromList = id
  fToList = id
  fZipWith = zipWith
  fZipWith3 = zipWith3
  fLength = length
  fLast = last
  fAppend = (++)
  fMap = fmap
  fTake = take
  fDrop = drop
  fInit = init
  fSnoc xs = (xs ++) . pure
  fConcatList = concat

instance ModelVector VB.Vector where
  fNull = VB.null
  fReplicate = VB.replicate
  fIdx = (VB.!)
  fFromList = VB.fromList
  fToList = VB.toList
  fZipWith = VB.zipWith
  fZipWith3 = VB.zipWith3
  fLength = VB.length
  fLast = VB.last
  fAppend = (VB.++)
  fMap = VB.map
  fTake = VB.take
  fDrop = VB.drop
  fInit = VB.init
  fSnoc = VB.snoc
  fConcatList = VB.concat

-- instance ModelVector VU.Vector where
--   -- type Constr VU.Vector = forall a . (a :: Type) -> VU.Unbox a
--   fZipWith :: (Double -> Double -> Double) -> VU.Vector Double -> VU.Vector Double -> VU.Vector Double
--   fZipWith = VU.zipWith
--   fLength = VU.length
--   fLast = VU.last

-- instance ModelVector VS.Vector where
--   fZipWith :: forall a b c . (VS.Storable a, VS.Storable b, VS.Storable c) => (a -> b -> c) -> VS.Vector a -> VS.Vector b -> VS.Vector c
--   fZipWith = VS.zipWith
