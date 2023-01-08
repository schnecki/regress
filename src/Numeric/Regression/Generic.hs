{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Regression.Generic
  ( ModelParameters
  , InputVector
  , ModelFunction
  , Loss (..)
  , LearningRate
  , AdamConfig (..)
  , AdamState (..)
  , totalCost
  , regressOn
  , regressStochasticOn
  , regressStochasticAdamOn
  , stochasticGradientDescentAvg
  , stochasticGradientDescentAdam
  , dot
  , dot'
  ) where

import           Control.Arrow               ((&&&))
import           Control.DeepSeq
import           Data.Default
import           Data.Foldable
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid
import           Data.Reflection
import           Data.Serialize
import           GHC.Generics
import           Numeric.AD                  hiding (grad, grad')
import           Numeric.AD.Internal.Reverse
import           Numeric.AD.Mode.Reverse     as Reverse (grad', gradWith, gradWith')

import           Numeric.Regression.Internal
import           Numeric.Regression.Loss

type LearningRate = Double

-- | A model using the given @f@ to store parameters of type @a@. Can be thought of as some kind of vector throughough this package.
type ModelParameters f a = f a

-- | Input vector.
type InputVector f a = f a

-- | Model function taking the parameters `theta` and the input vector `x` and returns @f(x | \theta)@.
type ModelFunction
   = forall f a. (ModelVector f, Foldable f, Floating a) =>
                   ModelParameters f a -> InputVector f a -> a


-- | Cost function for a linear regression on a set of observations
totalCost :: (ModelVector v, Foldable v, ModelVector f, Foldable f, Floating a, Mode a, Fractional (Scalar a))
          => ModelFunction       -- ^ Model function computing @f(x | theta)@.
          -> Loss                -- ^ Loss function.
          -> ModelParameters v a -- ^ Theta @t0@
          -> f a                 -- ^ expected @y@ value for each observation
          -> f (InputVector v a) -- ^ input data for each observation
          -> a                   -- ^ total cost over all observations
totalCost modelFun loss theta ys xs =
  -- let Acc n (Sum s) = foldMap acc $ fZipWith (errorFunction modelFun theta) xs ys
  let Acc n (Sum s) = foldMap acc $ fZipWith (\y x -> getLossFun loss y (modelFun theta x)) ys xs
  in s / fromIntegral n
{-# INLINE totalCost #-}


-- | Given some observed \"predictions\" @ys@, the corresponding
--   input values @xs@ and initial values for the model's parameters @theta0@,
--
-- > regress ys xs theta0
--
-- returns a stream of values for the parameters that'll fit the data better
-- and better.
--
-- Example:
--
-- @
-- -- the theta we're approximating
-- realtheta :: Model V.Vector Double
-- realtheta = V.fromList [1.0, 2.0, 3.0]
--
-- -- let's start there and make 'regress'
-- -- get values that better fit the input data
-- theta0 :: Model V.Vector Double
-- theta0 = V.fromList [0.2, 3.0, 2.23]
--
-- -- input data. (output value, vector of values for each input)
-- ys_ex :: V.Vector Double
-- xs_ex :: V.Vector (V.Vector Double)
-- (ys_ex, xs_ex) = V.unzip . V.fromList $
--   [ (3, V.fromList [0, 0, 1])
--   , (1, V.fromList [1, 0, 0])
--   , (2, V.fromList [0, 1, 0])
--   , (6, V.fromList [1, 1, 1])
--   ]
--
-- compute :: (ModelVector v, Foldable v, Num a)
--    -> Model v a          -- ^ theta vector, the model's parameters
--    -> v a                -- ^ @x@ vector, with the observed numbers
--    -> a                  -- ^ predicted @y@ for this observation
-- compute theta x = theta `dot` x
--
-- -- stream of increasingly accurate parameters
-- thetaApproxs :: [Model V.Vector Double]
-- thetaApproxs = regressOn compute ys_ex xs_ex theta0
-- @
regressOn:: (ModelVector f, ModelVector v, Traversable v, Applicative f, Foldable f, Ord a, Floating a)
        => ModelFunction         -- ^ Model function to compute @f(x | t)@, where `t` are the model parameters @theta@.
        -> Loss                  -- ^ Loss function.
        -> f a                   -- ^ expected @y@ value for each observation
        -> f (InputVector v a)   -- ^ input data for each observation
        -> ModelParameters v a   -- ^ initial parameters for the model, from which we'll improve
        -> [ModelParameters v a] -- ^ a stream of increasingly accurate values for the model's parameter to better fit the observations.
regressOn modelFun loss ys xs t0 =
  gradientDescent (\theta -> totalCost modelFun loss theta (fmap auto ys) (fmap (fmap auto) xs)) t0
{-# INLINE regressOn #-}

regressStochasticOn ::
     (ModelVector v, Traversable v, Foldable f, Ord a, Floating a)
  => ModelFunction         -- ^ Model function to compute @f(x | theta)@ with model parameters @theta@.
  -> Loss                  -- ^ Loss function.
  -> LearningRate          -- ^ Learning rate, e.g. 1e-3.
  -> f a                   -- ^ expected @y@ value for each observation
  -> f (InputVector v a)   -- ^ input data for each observation
  -> ModelParameters v a   -- ^ initial parameters for the model, from which we'll improve
  -> [ModelParameters v a] -- ^ a stream of increasingly accurate values for the model's parameter to better fit the observations.
regressStochasticOn modelFun loss learnRate ys xs t0 =
  stochasticGradientDescentAvg learnRate (\(y, x) theta -> getLossFun loss (auto y) (modelFun theta (fmap auto x))) (zip (toList ys) (toList xs)) t0
{-# INLINE regressStochasticOn #-}


regressStochasticAdamOn ::
     (ModelVector v, Traversable v, Foldable f, Ord a, Floating a)
  => ModelFunction         -- ^ Model function to compute @f(x | theta)@ with model parameters @theta@.
  -> Loss                  -- ^ Loss function.
  -> AdamConfig            -- ^ Adam config.
  -> Maybe (AdamState v a) -- ^ Adam Current state.
  -> f a                   -- ^ expected @y@ value for each observation
  -> f (InputVector v a)   -- ^ input data for each observation
  -> ModelParameters v a   -- ^ initial parameters for the model, from which we'll improve
  -> [(AdamState v a, ModelParameters v a)] -- ^ a stream of increasingly accurate values for the model's parameter to better fit the observations.
regressStochasticAdamOn modelFun loss cfg st ys xs t0 =
  stochasticGradientDescentAdam cfg st (\(y, x) theta -> getLossFun loss (auto y) (modelFun theta (fmap auto x))) (zip (toList ys) (toList xs)) t0
{-# INLINE regressStochasticAdamOn #-}


-- | The 'stochasticGradientDescentAvg' function uses simple stochastic gradient descent approximates of the true gradient by a gradient at a single example. In contrast to the stochastic gradient
-- descent algorithm of the original AD package, this one calculates an average gradient over all examples and applies it to the model.
--
-- It uses reverse mode automatic differentiation to compute the gradient The learning rate is constant through out, and is set to 0.001
stochasticGradientDescentAvg ::
     forall f a e. (ModelVector f, Traversable f, Fractional a, Ord a)
  => Double
  -> (forall s. Reifies s Tape => e -> f (Reverse s a) -> Reverse s a)
  -> [e]
  -> f a
  -> [f a]
stochasticGradientDescentAvg learnRate errorSingle dataset t0 = go gradAvg0 (fromRational $ toRational learnRate)
  where
    xgxs0 :: [f (a, a)] -- for each example, for each parameter: parameter and gradient
    xgxs0 = fmap (\di -> Reverse.gradWith (,) (errorSingle di) t0) dataset
    gradAvg0 :: f (a, a)
    gradAvg0 = fMap (\(xi, grad) -> (xi / fromIntegral len, grad / fromIntegral len)) $ foldl1 (fZipWith (\(a, b) (c, d) -> (a + c, b + d))) xgxs0
    len = length dataset
    go xgx !eta
      | eta == 0 = []
      | otherwise = t1 : go gradAvg1 eta
      where
        t1 = fmap (\(xi, gxi) -> xi - eta * gxi) xgx
        xgxs1 :: [f (a, a)] -- for each example, for each parameter: parameter and gradient
        xgxs1 = fmap (\di -> Reverse.gradWith (,) (errorSingle di) t1) dataset
        gradAvg1 :: f (a, a)
        gradAvg1 = fMap (\(xi, grad) -> (xi / fromIntegral len, grad / fromIntegral len)) $ foldl1 (fZipWith (\(a, b) (c, d) -> (a + c, b + d))) xgxs1
{-# INLINE stochasticGradientDescentAvg #-}


data AdamConfig =
  AdamConfig
    { adamCfgAlpha             :: !Double -- ^ Alpha [Default: 0.001]
    , adamCfgBeta1             :: !Double -- ^ Beta 1 [Default: 0.9]
    , adamCfgBeta2             :: !Double -- ^ Beta 2 [Default: 0.999]
    , adamCfgEpsilon           :: !Double -- ^ Epsilon [Default: 1e-7]
    , adamCfgWeightDecayLambda :: !Double -- ^ Weight decay to use [Default: 0.001]
    }
  deriving (Eq, Show, Generic, NFData, Serialize)


-- | Default settings for Adam.
instance Default AdamConfig where
  def = AdamConfig  0.001 0.9 0.999 1e-7 0.001

data AdamState f a =
  AdamState
    { adamStStep :: Int
    , adamStM    :: f a
    , adamStV    :: f a
    }
  deriving (Eq, Show, Generic, NFData, Serialize)


-- | The 'stochasticGradientDescentAdam' function approximates the true gradient of the constFunction by a gradient at a single example. In contrast to the stochastic gradient descent algorithm of the
-- original AD package, this one cacluclates an average gradient over all examples and applies it to the model. Only one update is performed to prevent divergence, i.e. there will be at maximum one
-- element in the list.
--
-- It uses reverse mode automatic differentiation to compute the gradient The learning rate is constant through out, and is set to 0.001
stochasticGradientDescentAdam ::
     forall f a e. (ModelVector f, Traversable f, Fractional a, Floating a)
  => AdamConfig
  -> Maybe (AdamState f a)
  -> (forall s. Reifies s Tape => e -> f (Reverse s a) -> Reverse s a)
  -> [e]
  -> f a
  -> [(AdamState f a, f a)]
stochasticGradientDescentAdam (AdamConfig alphaD beta1D beta2D epsilonD lambdaD) mSt errorSingle dataset t0 = go gradAvg0 (fromMaybe mkState mSt)
  where
    mkState = AdamState 0 (fReplicate (fLength t0) 0) (fReplicate (fLength t0) 0)
    alpha = fromRational (toRational alphaD)
    beta1 = fromRational (toRational beta1D)
    beta2 = fromRational (toRational beta2D)
    epsilon = fromRational (toRational epsilonD)
    lambda = fromRational (toRational lambdaD)
    xgxs0 :: [f (a, a)] -- for each example, for each parameter: result and gradient
    xgxs0 = fmap (\di -> Reverse.gradWith (,) (errorSingle di) t0) dataset
    gradAvg0 :: f (a, a)
    gradAvg0 = fMap (\(xi, grad) -> (xi / fromIntegral len, grad / fromIntegral len)) $ foldl1 (fZipWith (\(a, b) (c, d) -> (a + c, b + d))) xgxs0
    len = length dataset
    go xgx (AdamState step0 ms vs) = (st', t1) : go gradAvg1 st'
      where
        adam' (xi, gxi) m' v' =
          let alphaT = alpha * sqrt (1 - beta2 ^ step) / (1 - beta1 ^ step)
              xi' = xi - alphaT * (m' / (sqrt v' + epsilon) + lambda * xi)
           in xi'
        st' = AdamState step ms' vs'
        step = step0 + 1
        ms' = fZipWith (\m (_, gxi) -> beta1 * m + (1 - beta1) * gxi) ms xgx
        vs' = fZipWith (\v (_, gxi) -> beta2 * v + (1 - beta2) * gxi ^ (2 :: Int)) vs xgx
        t1 = fZipWith3 adam' xgx ms' vs'
        xgxs1 :: [f (a, a)] -- for each example, for each parameter: result and gradient
        xgxs1 = fmap (\di -> Reverse.gradWith (,) (errorSingle di) t1) dataset
        gradAvg1 :: f (a, a)
        gradAvg1 = fMap (\(xi, grad) -> (xi / fromIntegral len, grad / fromIntegral len)) $ foldl1 (fZipWith (\(a, b) (c, d) -> (a + c, b + d))) xgxs1
{-# INLINE stochasticGradientDescentAdam #-}
