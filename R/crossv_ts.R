#' Generate test/train sets for time-series
#'
#' Generate test/train sets for time series. Each training set consists of
#' observations before the test set. This method also called "evaluation on a
#' rolling forecasting origin".
#'
#' @details
#' In time-series cross-validation the training set only uses
#' observations that are prior to the test set. Suppose the time series has
#' \eqn{n} observations, the training set has a maximum size of
#' \eqn{r \leq n}{r <= n} and minimum size of \eqn{s \geq r}{s >= r}.
#' and the test set has a maximum size of \eqn{p \leq n}{p <= n} and
#' minimum size of \eqn{q \geq p}{q >= p}.
#'
#' For indices \eqn{i \in \{1, \dots, N\}}{i in \{1, \dots, N\}}:
#'
#' -   Select observations \eqn{i, \dots, \max(p, n)}{i, ..., max(p, n)}
#'     for the test set.
#' -   Select observations
#'     \eqn{\max(i - h - p), \dots, i - h}{max(i - h - p), ..., i - h}
#'     for the training set.
#' -   If the test set has a size of at least `q` and the training set
#'     has a size of at least `r`.
#'
#' @template param-n
#' @param horizon Difference between the first test set observation and the last training set observation
#' @param test_size A scalar integer indicating the size of the test set.
#' @param train_size The maximum size of the training set. This allows for a
#'   rolling window training set, instead of using all observations from the
#'   start of the time series.
#' @param test_partial,train_partial A logical scalar or positive scalar integer which
#'   handles how partial test (train) sets are handled. If `TRUE`, then partial
#'   test (train) sets are allowed. If `FALSE`, then partial test (train) sets
#'   are not allowed. If it is an integer, then it is the minimum allowable
#'   size of a test (train) set.
#' @param test_start An integer vector of the starting index values of the
#'   test set.
#' @param from,to,by Integer scalar values used to generate values of
#'   `test_start` if `test_start = NULL`. In that case,
#'   `test_start = seq(from, to, by)`.
#'
#' @references
#'
#' -   Hyndman RJ (2017).
#'     *forecast: Forecasting functions for time series and linear models*.
#'     R package version 8.0,
#'     [URL](http://github.com/robjhyndman/forecast).
#' -   Hyndman RJ and Khandakar Y (2008). "Automatic time series forecasting:
#'     the forecast package for R." *Journal of Statistical Software*.
#'     [URL](http://www.jstatsoft.org/article/view/v027i03).
#' -   Rob J. Hyndman.
#'     "[Cross-validation for time series](http://robjhyndman.com/hyndsight/tscv/)".
#'     December 5, 2016.
#' -   Rob J. Hyndman.
#'     "[Time series cross-validation: an R example](http://robjhyndman.com/hyndsight/tscvexample/)".
#'     August 26, 2011.
#' -   Rob J. Hyndman and George Athanasopoulos.
#'     "Evaluating Forecast Accuracy." [URL](https://www.otexts.org/fpp/2/5).
#' -   Max Kuhn. "Data splitting for Time Series." *The caret Package*.
#'     2016-11-29. [URL](https://topepo.github.io/caret/data-splitting.html).
#'
#' @export
#' @importFrom purrr pmap
crossv_ts <- function(n,
                      horizon = 1L,
                      test_size = 1L, test_partial = FALSE,
                      train_partial = TRUE, train_size = n,
                      test_start = NULL,
                      from = 1L, to = n, by = 1L) {
  # I don't think I need this function
  test_start <- test_start %||% seq(from, to, by)
  test_end <- pmin(test_start + test_size - 1L, n)
  test_len <- test_end - test_start + 1L
  train_end <- test_start - horizon
  train_start <- pmax(train_end - train_size + 1L, 1L)
  train_len <- train_end - train_start + 1L
  # check the validity of these segments
  keep_test <- (test_len >= test_size) |
      (test_partial & test_len >= test_partial)
  keep_train <- (train_len >= train_size) |
      (train_partial & train_len >= train_partial)
  idx <- keep_test & keep_train

  f <- function(test_start, test_end, train_start, train_end) {
    sample_idx(in_id = train_start:train_end,
               out_id = test_start:test_end, n = n)
  }
  pmap(list(test_start = test_start[idx], test_end = test_end[idx],
            train_start = train_start[idx], train_end = train_end[idx]),
       f)
}
