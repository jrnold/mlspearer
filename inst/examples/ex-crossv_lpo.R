# Example originally from modelr::crossv_mc
library("purrr")
# leave-on-out cross-validation
crossv_loo(10)

# leave-two-out cross-validation
crossv_lpo(10, size = 2)
