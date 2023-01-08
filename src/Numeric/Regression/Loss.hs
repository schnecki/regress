{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Regression.Loss
  ( LossFunction
  , Loss (..)
  , getLossFun
  , l1Loss
  , l2Loss
  , pseudoHuberLoss
  ) where

import           Control.DeepSeq
import           Data.Serialize
import           GHC.Generics
import           Numeric.AD      hiding (grad, grad')

data Loss
  = LossL1
  | LossL2
  | LossPseudoHuber (Maybe Rational)
            deriving (Show, Eq, Ord, NFData, Generic, Serialize)

getLossFun :: Loss -> LossFunction a
getLossFun LossL1                   = l1Loss
getLossFun LossL2                   = l2Loss
getLossFun (LossPseudoHuber mDelta) = pseudoHuberLoss mDelta
{-# INLINE getLossFun #-}

type LossFunction a
  = (Mode a, Fractional (Scalar a), Floating a) =>
     a -- ^ Expected y
  -> a -- ^ Model output
  -> a -- ^ Error

-- | Mean absolute error (MAE).
l1Loss :: LossFunction a
l1Loss y fx = abs (y - fx)
{-# INLINE l1Loss #-}

-- | Mean squared error (MSE).
l2Loss :: LossFunction a
l2Loss y fx = 0.5 * (y - fx) ^ (2 :: Int)
{-# INLINE l2Loss #-}

-- | Differentiable pseudo Huber Loss. Default delta: 1.35
pseudoHuberLoss :: (Mode a, Fractional (Scalar a)) => Maybe Rational -> LossFunction a
pseudoHuberLoss mDelta y fx = delta ^ (2 :: Int) * (sqrt (1 + (a / delta) ^ (2 :: Int)) - 1)
  where
    delta = maybe 1.35 (auto . fromRational) mDelta
    a = y - fx
{-# INLINE pseudoHuberLoss #-}

