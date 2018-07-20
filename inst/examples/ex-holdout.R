# Test/train splits using the number of observations
holdout_n(10, times = 5, size = 2)

# Test/train splits without shuffling
holdout_n(10, times = 1, size = 2, shuffle = FALSE)

# Test/train splits using the fraction of observation
holdout_frac(10, frac = 0.3, times = 3)

# Monte-Carlo cross-validation
crossv_mc(10, frac = 0.3, times = 3)

# Manual test/train splits
holdout_idx(10, test = list(1:2, 2:3, 4:5))
