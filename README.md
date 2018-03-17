The description of this repo is described in from src/Numeric/Integrotor.hs
which I copied here to this readme:

```
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
```

