{-# LANGUAGE BangPatterns #-}

module Numeric.Integrotor.Simpson
  ( simpsonRule
  , simpsonIntegral )
  where

simpsonRule :: Fractional a => a -> a -> (a -> a) -> a
simpsonRule a b point =
  ((b - a)/6) * (point a + 4 * point ((a+b)/2) + point b)
{-# INLINE simpsonRule #-}

simpsonIntegral :: Fractional a => Int -> a -> a -> (a -> a) -> a
simpsonIntegral x _ _ _ | x <= 0 = error "simpsonIntegral: must use at least one split."
simpsonIntegral 1 a b point = simpsonRule a b point
simpsonIntegral n a b point =
  go 0 0
 where
  split_size = (b-a)/fromIntegral n

  go !accum partition | partition >= n = accum
  go !accum partition =
    go (accum +
        simpsonRule (fromIntegral partition * split_size + a)
                    (fromIntegral (partition+1) * split_size + a)
                    point)
       (partition+1)
{-# INLINE simpsonIntegral #-}

