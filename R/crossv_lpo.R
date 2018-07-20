#' Generate cross-validated leave-p-out test/training pairs
#'
#' Generate cross-validated leave-one-out or leave-p-out test/training pairs.
#' The function `leave-p-out` generates leave-p-out test/training pairs.
#' The function `leave-one-out` is convenience function for the
#' common special case of leave-one-out cross-validation, `p = 1`.
#'
#' @template param-n
#' @param size A scalar integer representing the number of items to include
#'   in the training set.
#'
#' @export
#' @example inst/examples/ex-crossv_lpo.R
crossv_lpo <- function(n, size = 1L) {
  f <- function(x) {
    sample_idx(out_id = x, n = n)
  }
  idx <- if (size == 1L) {
    idx <- seq_len(n)
  } else {
    utils::combn(seq_len(n), size, simplify = FALSE)
  }
  map(idx, f)
}

#' @rdname crossv_lpo
#' @export
crossv_loo <- function(n) {
  crossv_lpo(n, size = 1L)
}
