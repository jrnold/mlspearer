#' Generate time-series bootstrap replicates
#'
#' Generate bootstrap replicates for time series items.
#' The replicate time series can be generated using either fixed or random
#' block lengths.
#'
#' @details If `type = "fixed"` then each replicate time series is found
#' by taking blocks of length `block_size`, from the original time series and
#' putting them end-to-end until a new series of length `size` is created.
#' When `type = "geom"`, a similar approach is taken except
#' that the block lengths are sampled from a geometric distribution with mean
#' `block_size`.
#'
#' @param n The number of observations.
#' @param times The number of bootstrap samples.
#' @param block_size If `type = "fixed"`, then `size` is the fixed block length
#'     used in generating the bootstrap samples.  If `type = "geom"`
#'    then `size` is the mean of the geometric distribution from which the
#'    block lengths were sample. The value of `size` must be a positive
#'    integer less than `n`.
#' @param type The type of simulation used to generate the replicate time
#'    series. The possible input values are `"fixed"`, for block resampling
#'    with fixed block lengths of `size`, and `"geom"`, for block resampling
#'    with block lengths having a geometric distribution with mean `size`).
#' @param size A scalar integer representing the number of items in the
#'    output samples.
#' @param endcorr A scalar logical indicating whether to adjust for end
#'    correlations in blocks.
#'
#' @seealso
#' The \pkg{boot} function [boot::tsboot()], from which this function is
#' derived.
#'
#' @references
#'
#' -   Kunsch, H.R. (1989) The jackknife and the bootstrap for general stationary observations. *Annals of Statistics*
#' -   Davison, A.C. and Hinkley, D.V. (1997) *Bootstrap Methods and Their Application*. Cambridge University Press.
#' -   Politis, D.N. and Romano, J.P. (1994) "The stationary bootstrap. Journal of the American Statistical Association."
#' -   Angelo Canty and Brian Ripley (2016). boot: Bootstrap R (S-Plus) Functions. R package version 1.3-18.
#'
#' @export
#' @importFrom assertthat assert_that
#' @importFrom rlang is_integerish is_scalar_logical is_scalar_double
#' @importFrom rlang is_scalar_integerish
bootstrap_ts <- function(n, times = 1L, block_size = 1L, size = n,
                         type = c("fixed", "geom"), endcorr = TRUE) {
  assert_that(is_integerish(n) & block_size >= 1L)
  assert_that(is_integerish(times) & times >= 1L)
  assert_that(is_integerish(block_size) & block_size >= 1L)
  block_size <- max(block_size, n)
  assert_that(is_integerish(size) & size >= 1L)
  assert_that(is_scalar_logical(endcorr))
  assertthat::is.flag(endcorr)
  type <- match.arg(type)
  f <- switch(type,
              geom = bootstrap_ts_geom,
              fixed = bootstrap_ts_mbb,
              stop("type = ", type, " is not recognized.", call. = FALSE))
  rerun(times, f(n, block_size = block_size, size = size, endcorr = endcorr))
}

# Adapted from boot:::make.ends
mod <- function(i, n, endcorr) {
  if (endcorr) 1 + (i - 1) %% n
  else i
}

# Fixed Moving Block Bootstrap from boot::tsboot and boot:::ts.array
# Fixed Moving Block Bootstrap from boot::tsboot
#' @importFrom purrr map2
bootstrap_ts_mbb <- function(n, block_size = 1L, size = n, endcorr = TRUE) {
  endpt <- if (endcorr) n else n - block_size + 1L
  nn <- ceiling(size / block_size)
  lens <- c(rep(block_size, nn - 1L), 1L + (size - 1L) %% block_size)
  starts <- sample.int(endpt, nn, replace = TRUE)
  in_id <- flatten_int(map2(starts, lens, function(start, len) {
    if (len >= 1) {
      as.integer(mod(seq(start, start + len - 1L), n, endcorr))
    } else {
      integer()
    }
  }))
  sample_idx(in_id = in_id, n = n)
}

# Fixed Moving Block Bootstrap from boot::tsboot and boot:::ts.array

#' @importFrom utils head
#' @importFrom stats rgeom
#' @importFrom purrr detect_index
bootstrap_ts_geom <- function(n, block_size = 1L, size = n, ...) {
  endpt <- n - size + 1L
  # worst case scenario is to draw m. So take m draws from
  # geom and truncate
  lens <- 1L + rgeom(size, 1L / block_size)
  len_total <- cumsum(lens)
  # truncate to minimum length >= m
  lens <- lens[seq_len(detect_index(len_total, function(.x) {.x  >= size}))]
  starts <- sample.int(endpt, length(lens), replace = TRUE)
  f <- function(start, len) {
    if (len >= 1) {
      as.integer(mod(seq.int(start, start + len - 1L), n, TRUE))
    } else {
      integer()
    }
  }
  sample_idx(in_id = head(flatten_int(map2(starts, lens, f)), size), n = n)
}
