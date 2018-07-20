#' Generate cross-validated time-series K-fold test-training pairs
#'
#' Splits data into test/train sets for cross-validating time series
#' data. In each split, the indices are increasing, so only previous data
#' are used to test future data.
#'
#' The data are split into \eqn{K \geq 2} folds, where for each
#' \eqn{i \in 2:K}{i in 2:K}, the \eqn{i}-th fold is the test set
#' and folds \eqn{1, ..., (i - 1)} folds are the training set.
#'
#' @template param-n
#' @param v A scalar integer representing the number of folds to generate.
#'
#' @references
#'
#' -   Scikit-learn v. 0.18.1. Cross-validation: evaluating estimator performance.
#'     [URL](http://scikit-learn.org/stable/modules/cross_validation.html#cross-validation).
#'
#' @export
#' @importFrom purrr flatten_int
cross_vfold_ts <- function(n, v = 5L) {
  folds <- partition(seq_len(n), as.integer(v), shuffle = FALSE)
  map(2:v, function(i) {
    sample_idx(in_id = flatten_int(folds[seq_len(i - 1L)]),
               out_id = folds[[i]], n = n)
  })
}
