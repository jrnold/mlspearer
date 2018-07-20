#' Generate test/train splits
#'
#' Functions to generate test/train splits. The function `holdout_n()`
#' generates splits such that `size` observations are in the test
#' set and `n - size` is in the training set. The function `holdout_frac()`
#' splits the data such that `frac` proportion of elements are in the test set
#' and `1 - frac` proportion are in the training set.
#'
#' Either `holdout_frac()` and `holdout_n()`, when combined with
#' `shuffle = TRUE` and `times > 1` can be used to generate test/train splits
#' using Monte Carlo cross-validation.
#' The function `crossv_mc()` is a convenience function for Monte Carlo
#' cross-validation.
#'
#' The function `holdout_idx()` generates test/train splits from manually
#' specified indexes.
#'
#' @template param-n
#' @template param-times
#' @param size A scalar integer representing the number of elements in the
#'   test set.
#' @param prob A numeric vector with observation-specific probabilities that
#'   an observation is the test set. If `NULL`, all observations have equal
#'   probabilities.
#' @param shuffle A logical scalar indicating whether to shuffle the
#'   items prior to splitting into test/train sets. This should be used
#'   whenever `times > 1`.
#'
#' @export
#' @example inst/examples/ex-holdout.R
#' @importFrom rlang is_scalar_logical is_null is_double
holdout_n <- function(n, times = 1L, size = 1L, shuffle = TRUE, prob = NULL) {
  assert_that(is_scalar_integerish(n) & n >= 0L)
  assert_that(is_scalar_integerish(size) & size >= 1L)
  assert_that(is_scalar_logical(shuffle))
  assert_that(is_null(prob) || is_double(prob))
  rerun(times, holdout_n_one(size, shuffle = shuffle, prob = prob))
}

holdout_n_one <- function(n, size = 1L, shuffle = TRUE, prob = prob) {
  if (shuffle) {
    idx <- sample.int(n, size = size, replace = FALSE, prob = prob)
    ret <- sample_idx(out_id = idx, n = n)
  } else {
    ret <- if (size == n) {
      sample_idx(out_id = seq_len(n), in_id = integer(0), n = n)
    } else if (size == 0) {
      sample_idx(in_id = seq_len(n), out_id = integer(0))
    } else {
      sample_idx(in_id = seq(1, n - size), out_id = seq(n - size + 1, n))
    }
  }
  ret
}

#' @rdname holdout_n
#' @param frac A numeric scalar between 0 and 1 representing the proportion of
#'   items in the test set.
#' @export
holdout_frac <- function(n, times = 1L, frac = 0.3, shuffle = TRUE,
                         prob = NULL) {
  assert_that(is_scalar_double(frac))
  frac <- max(min(0, frac), 1)
  holdout_n(n, size = floor(frac * n), times = times, shuffle = shuffle,
            prob = prob)
}

#' @rdname holdout_n
#' @export
crossv_mc <- function(n, times = 25, frac = 0.3, prob = NULL) {
  holdout_frac(n = n, frac = frac, times = times, prob = prob, shuffle = TRUE)
}

#' @rdname holdout_n
#' @param test,train A list of integer vectors, each containing the indexes
#'   in the test (train) splits. If test (train) `NULL`, then the index values
#'   will be set to the complement of the train (test) indexes.
#'
#' @importFrom purrr transpose
#' @export
holdout_idx <- function(n, train = NULL, test = NULL) {
  if (is.integer(train)) {
    train <- list(train)
  }
  if (is.integer(test)) {
    test <- list(test)
  }
  if (is.null(test)) {
    if (is.null(train)) {
      stop("Either `test` or `train` must be non-`NULL`.", call. = FALSE)
    } else {
      ret <- map(test, function(i) {
        list(in_id = complement(i, n), out_id = i)
      })
    }
  } else {
    if (is.null(train)) {
      ret <- map(test, function(i) {
        list(in_id = i, out_id = complement(i, n))
      })
    } else {
      ret <- transpose(list(in_id = train, out_id = test))
    }
  }
  ret
}
