{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Compute some probabilities that rely on being in specific order in a way
-- that's friendly to being put through automatic differentiation.
--
-- The function `probabilityFirstIsFirst` is the main feature of this module
-- and approximates a probability using Simpson's rule to approximate some
-- integrals.
--
-- It answers the following problem:
--
-- Let X1, X2, ... XN be independent distributions. Sample x1, x2, ... xN from
-- each distribution.
--
-- What is the probability that a particular xi sampled is larger than all
-- others?
--
-- This is computed with integral: / pdf_i(t) cdf_j(t) cdf_(j+1)(t) ... (t)dt
-- Where j /= i and integral goes from negative infinity to positive infinity.
--

module Numeric.Integrotor
  (
  -- * Computing probabilities
    probabilityFirstIsFirst
  , probabilityFirstIsFirstAccurate
  -- * Create distributions
  , nd
  , cd
  , ld
  -- * Compute Pdfs and Cdfs of distributions
  , Dist(..)
  , normalPdf
  , normalCdf
  , stdev
  , cauchyPdf
  , cauchyCdf
  , levyPdf
  , levyCdf
  , NormalDistribution(..)
  , CauchyDistribution(..)
  , LevyDistribution(..) )
  where

import Data.Data
import Data.Foldable
import GHC.Generics

import Numeric.Integrotor.Simpson

-- | Typeclass for distributions used in this module.
class Dist d where
  -- | Point density function
  pdf          :: Floating a => a -> d a -> a
  -- | Cumulative density function
  cdf          :: Floating a => a -> d a -> a
  -- | "Middle" point of a distribution, it's mean if it exists, otherwise it's
  --    some concept of a middle point for the distribution. It's used as a
  --    component to decide how to apply Simpson's rule.
  middle       :: Floating a => d a -> a
  -- | "Deviation" of a distribution. Should be standard deviation if it
  --   exists; otherwise some other concept of deviation. It's used as a
  --   component to decide how to apply Simpson's rule.
  deviation    :: Floating a => d a -> a

data NormalDistribution a = NormalDistribution !a !a
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Functor, Foldable, Traversable )

data CauchyDistribution a = CauchyDistribution !a !a
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Functor, Foldable, Traversable )

data LevyDistribution a = LevyDistribution !a !a
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Functor, Foldable, Traversable )

instance Dist NormalDistribution where
  pdf = normalPdf
  {-# INLINE pdf #-}

  cdf = normalCdf
  {-# INLINE cdf #-}

  middle = mean
  deviation = stdev

instance Dist CauchyDistribution where
  pdf = cauchyPdf
  {-# INLINE pdf #-}

  cdf = cauchyCdf
  {-# INLINE cdf #-}

  middle (CauchyDistribution x0 _) = x0
  deviation (CauchyDistribution _ scale) = scale*5

instance Dist LevyDistribution where
  pdf = levyPdf
  {-# INLINE pdf #-}

  cdf = levyCdf
  {-# INLINE cdf #-}

  middle (LevyDistribution x0 _) = x0
  deviation (LevyDistribution _ scale) = scale*5

ld :: a -> a -> LevyDistribution a
ld = LevyDistribution
{-# INLINE ld #-}

cd :: a -> a -> CauchyDistribution a
cd = CauchyDistribution
{-# INLINE cd #-}

nd :: a -> a -> NormalDistribution a
nd = NormalDistribution
{-# INLINE nd #-}

mean :: NormalDistribution a -> a
mean (NormalDistribution m _) = m
{-# INLINE mean #-}

-- | Returns the standard deviation of a normal distribution.
stdev :: NormalDistribution a -> a
stdev (NormalDistribution _ v) = v
{-# INLINE stdev #-}

levyPdf :: Floating a => a -> LevyDistribution a -> a
levyPdf x (LevyDistribution mean scale)
  = sqrt (scale/(2*pi)) * (exp (negate $ scale/(2*(x-mean)))/((x-mean)**(3/2)))
{-# INLINE levyPdf #-}

levyCdf :: Floating a => a -> LevyDistribution a -> a
levyCdf x (LevyDistribution mean scale)
  = 1 - erf (sqrt $ scale / (2*(x - mean)))
{-# INLINE levyCdf #-}

cauchyPdf :: Floating a => a -> CauchyDistribution a -> a
cauchyPdf x (CauchyDistribution x0 scale)
  = 1/((pi*scale) * (1 + ((x - x0)/scale)**2))
{-# INLINE cauchyPdf #-}

cauchyCdf :: Floating a => a -> CauchyDistribution a -> a
cauchyCdf x (CauchyDistribution x0 scale)
  = (1/pi) * atan ((x - x0)/scale) + 0.5
{-# INLINE cauchyCdf #-}

normalPdf :: Floating a => a -> NormalDistribution a -> a
normalPdf x (NormalDistribution mean stdev)
  = (1/sqrt (2*pi*(stdev**2))) * exp (negate $ ((x - mean)**2)/(2*(stdev**2)))
{-# INLINE normalPdf #-}

normalCdf :: Floating a => a -> NormalDistribution a -> a
normalCdf point (NormalDistribution mean stdev) =
  0.5 * (1 + erf ( (point-mean) / (stdev * sqrt 2) ) )
{-# INLINE normalCdf #-}

erf :: Floating a => a -> a
erf x = signum x * erf' (signum x * x)
 where
  erf' x = 1 - (a1*t + a2*(t**2) + a3*(t**3) + a4*(t**4) + a5*(t**5)) * exp (negate $ x**2)
   where
    t = 1/(1 + p*x)
  {-# INLINE erf' #-}

  p  = 0.3275911
  a1 = 0.254829592
  a2 = -0.284496736
  a3 = 1.421413741
  a4 = -1.453152027
  a5 = 1.061405429
{-# INLINE erf #-}

mul :: (Foldable f, Num a) => f a -> a
mul = foldl' (*) 1
{-# INLINE mul #-}

{-# ANN module "HLint: ignore Reduce duplication" #-}
-- | Returns probability that the first distribution given, when sampled, ends
-- up being larger than values drawn from all the other distributions.
--
-- This is an approximation which is fairly close for normal distributions but
-- can be a bit off for long-tailed distributions like cauchy and levy.
probabilityFirstIsFirst :: (Functor f, Dist d, Floating a, Ord a, Foldable f) => d a -> f (d a) -> a
probabilityFirstIsFirst nd_target rest_of_nds =
  simpsonIntegral 20 (minpoint-5*maxdeviation) (maxpoint+5*maxdeviation)
    (\point -> pdf point nd_target *
               mul (cdf point <$> rest_of_nds))
 where
  minpoint = min (middle nd_target) $ minimum $ fmap middle rest_of_nds
  maxpoint = max (middle nd_target) $ maximum $ fmap middle rest_of_nds

  maxdeviation = max (deviation nd_target) $ maximum $ fmap deviation rest_of_nds
{-# INLINE probabilityFirstIsFirst #-}

-- | Same as `probabilitiyFirstIsFirst` but uses a million times more splits
-- and extends integral approximation range.
--
-- Yields much more accurate results but is also a million times slower.
probabilityFirstIsFirstAccurate :: (Functor f, Dist d, Floating a, Ord a, Foldable f) => d a -> f (d a) -> a
probabilityFirstIsFirstAccurate nd_target rest_of_nds =
  simpsonIntegral 5000000 (minpoint-500*maxdeviation) (maxpoint+500*maxdeviation)
    (\point -> pdf point nd_target *
               mul (cdf point <$> rest_of_nds))
 where
  minpoint = min (middle nd_target) $ minimum $ fmap middle rest_of_nds
  maxpoint = max (middle nd_target) $ maximum $ fmap middle rest_of_nds

  maxdeviation = max (deviation nd_target) $ maximum $ fmap deviation rest_of_nds
{-# INLINE probabilityFirstIsFirstAccurate #-}

