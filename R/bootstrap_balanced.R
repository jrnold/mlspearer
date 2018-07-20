#' Generate balanced bootstrap replicates
#'
#' A balanced bootstrap generates `times` replicates in which each element
#' appears `times` times.
#'
#' @template param-n
#' @template param-times
#'
#' @seealso The \pkg{boot} function [boot::boot()] which is the
#'   canonical R bootstrap implementation.
#'
#' @references
#'
#' -   Gleason, John 1988. "Algorithms for Balanced Bootstrap Simulations".
#'     *The American Statistician*.
#'     [doi:10.2307/2685134](https://dx.doi.org/10.2307/2685134).
#' -   Davison, A.C., Hinkley, D. V., and Schechtman, E. 1986.
#'     "Efficient Bootstrap Simulation." *Biometrika*.
#' -   Angelo Canty and Brian Ripley (2016).
#'     *boot: Bootstrap R (S-Plus) Functions*. R package version 1.3-18.
#'
#' @export
#' @example inst/examples/ex-bootstrap_balanced.R
bootstrap_balanced <- function(n, times = 1L) {
  map(partition(rep(seq_len(n), times), times), sample_idx, out_id = NULL,
      n = n)
}
