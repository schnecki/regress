{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Regression.Generic
  ( Model
  , RegressionFunction (..)
  , regressionNrCoefficients
  , cost
  , totalCost
  , compute
  , regressOn
  , regressOns
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Data.Dynamic
import           Data.Foldable
import           Data.Monoid
import           Data.Reflection
import           Data.Serialize
import           Data.Traversable
import           GHC.Generics
import           Numeric.AD                  hiding (grad, grad')
import           Numeric.AD.Internal.Reverse
import           Numeric.AD.Mode.Reverse
import           Numeric.Regression.Internal
import           Unsafe.Coerce               (unsafeCoerce)


-- | A model using the given @f@ to store parameters of type @a@.
--   Can be thought of as some kind of vector throughough this
--   package.
type Model f a = f a


-- | Function to apply to the input.
data RegressionFunction
  = RegLinear -- ^ Linear regression.
  | RegQuadratic -- ^ Quadratic regression.
  deriving (Show, Eq, Ord, NFData, Generic, Serialize, Enum, Bounded)


regressionNrCoefficients :: RegressionFunction -> Int -> Int
regressionNrCoefficients RegLinear n    = n + 1
regressionNrCoefficients RegQuadratic n = 2 * n + 1


-- | Compute the predicted value for
--   the given model on the given observation
compute ::
     (ModelVector v, Foldable v, Num a)
  => RegressionFunction -- ^ Regression function
  -> Model v a          -- ^ theta vector, the model's parameters
  -> v a                -- ^ @x@ vector, with the observed numbers
  -> a                  -- ^ predicted @y@ for this observation
compute RegLinear theta x    = theta `dot` x
compute RegQuadratic theta x = theta `dot` (x `fAppend` fMap (^2) x) -- TODO: all x1*x2 combinations + theta * x
{-# INLINE compute #-}


-- | Cost function for a linear regression on a single observation
cost :: (ModelVector v, Foldable v, Floating a)
     => RegressionFunction -- ^ Regression function: f(theta, xs).
     -> Model v a          -- ^ Theta @t0@
     -> v a                -- ^ @x@ vector
     -> a                  -- ^ expected @y@ for the observation
     -> a                  -- ^ cost
cost regFun theta x y = 0.5 * (y - compute regFun theta x) ^ (2 :: Int)
{-# INLINE cost #-}


-- | Cost function for a linear regression on a set of observations
totalCost :: (ModelVector v, Foldable v, ModelVector f, Foldable f, Floating a)
          => RegressionFunction -- ^ Regression function: f(theta, xs).
          -> Model v a          -- ^ Theta @t0@
          -> f a                -- ^ expected @y@ value for each observation
          -> f (v a)            -- ^ input data for each observation
          -> a                  -- ^ total cost over all observations
totalCost regFun theta ys xs =
  let Acc n (Sum s) = foldMap acc $ fZipWith (cost regFun theta) xs ys
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
-- -- stream of increasingly accurate parameters
-- thetaApproxs :: [Model V.Vector Double]
-- thetaApproxs = learnAll ys_ex xs_ex theta0
-- @
regressOn:: (ModelVector f, ModelVector v, Traversable v, Applicative f, Foldable f, Ord a, Floating a)
        => RegressionFunction -- ^ Regression function: f(theta, xs).
        -> f a                -- ^ expected @y@ value for each observation
        -> f (v a)            -- ^ input data for each observation
        -> Model v a          -- ^ initial parameters for the model, from which we'll improve
        -> [Model v a]        -- ^ a stream of increasingly accurate values for the model's parameter to better fit the observations.
regressOn regFun ys xs t0 =
  gradientDescent (\theta -> totalCost regFun theta (fmap auto ys) (fmap (fmap auto) xs)) t0


regressOns:: (ModelVector f, ModelVector v, Traversable v, Applicative f, Foldable f, Ord a, Floating a)
        => [RegressionFunction] -- ^ Regression function: f(theta, xs).
        -> f a                -- ^ expected @y@ value for each observation
        -> f (v a)            -- ^ input data for each observation
        -> Model v a          -- ^ initial parameters for the model, from which we'll improve
        -> [Model v a]        -- ^ a stream of increasingly accurate values for the model's parameter to better fit the observations.
regressOns [] _ _ _ = error "regressOns: Empty regression function list"
regressOns regFuns ys xs t0 =
  gradientDescent (\theta -> (/ fromIntegral (length regFuns)) . sum $ map (\regFun -> totalCost regFun theta (fmap auto ys) (fmap (fmap auto) xs)) regFuns) t0