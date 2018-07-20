#' Generate rolling windows
#'
#' Generate resample objects of rolling windows over elements of a data frame.
#'
#' @template param-n
#' @param partial A logical flag or numeric vector. If `FALSE` all indexes of a rolling
#'   window must be within the rows (or groups) of `data`. If `TRUE`,
#'   then the subset of indexes that are in range are used.
#'   A numeric argument to `partial` can be used to determine the minimal
#'   window size for partial windows.
#' @param width The window width.
#' @param align Is the window left-aligned, right-aligned, or centered
#'   relative to the reference index of the window.
#' @param offsets An integer vector or `NULL`. If non-`NULL`,
#'   then it is the offsets of elements in the window relative to the
#'   reference index of the window; `0` is the reference index, negative
#'   numbers are indices before the index, while positive numbers are after the
#'   index. If `NULL`, the offsets are generated from the `width`
#'   and `align` arguments.
#' @param indices,from,to,by The indices at which to generate windows. If
#'   `indices` is non-`NULL`, then it is used. Otherwise, the indices
#'   are generated from `seq(from, to, by)`.
#'
#' @importFrom purrr %||% map
#' @export
roll <- function(n,
                  width = 1L,
                  align = c("left", "right", "center"),
                  partial = TRUE,
                  indices = NULL,
                  from = 1L,
                  to = n,
                  by = 1L,
                  offsets = NULL) {
  offsets <- offsets %||% switch(
    align,
    right = seq(to = 0L, length.out = width),
    center = seq(to = floor(width / 2), length.out = width),
    left = seq(from = 0L, length.out = width)
  )
  offsets <- as.integer(offsets)
  f <- function(i) {
    window <- i + offsets
    inrange <- window >= 1 & window <= n
    if (all(inrange) || (partial && (sum(inrange) >= partial))) {
      window[inrange]
    } else {
      NULL
    }
  }
  indices <- indices %||% seq(from, to, by)
  map(indices, f)
}
