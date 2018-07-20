#' Generate bootstrap replicates
#'
#' Generate nonparametric bootstrap samples. In addition to the the ordinary
#' bootstrap, it supports weights, the Bayesian bootstrap, and sub- or
#' super-sampling.
#'
#' @template param-n
#' @template param-times
#' @param size The number of observations in each output sample.
#' @param weights A numeric vector with observation level weights.
#' @param bayes A scalar logical, indicating whether to use the Bayesian
#'   bootstrap.
#'
#' @seealso
#' The \pkg{boot} function [boot::boot()] which is the
#' canonical R bootstrap implementation. The \pkg{modelr} function
#' [modelr::bootstrap()], \pkg{rsample} function [rsample::bootstraps]
#' provide similar implementations specialized to data frames.
#'
#' @references
#'
#' -   Angelo Canty and Brian Ripley (2016). "boot: Bootstrap R (S-Plus) Functions." R package version 1.3-18.
#' -   Davison, A. C. & Hinkley, D. V. (1997) Bootstrap Methods and Their Applications. Cambridge University Press, Cambridge. ISBN 0-521-57391-2
#'
#' @export
#' @importFrom stats rgamma
#' @importFrom purrr rerun
#' @example inst/examples/ex-bootstrap.R
bootstrap <- function(n, times = 1L, weights = NULL, bayes = FALSE,
                      size = n) {
  if (!bayes) {
    rerun(times, sample_idx(in_id = sample.int(n, size = size,
                                               replace = TRUE,
                                               prob = weights), n = n))
  } else {
    weights <- weights %||% 1
    # sample dirichlet with independent gammas
    # don't need to normalize these probabilities since sample.int()
    # will normalize `prob`.
    probs <- rerun(times, rgamma(n, shape = weights))
    f <- function(prob) {
      sample_idx(in_id = sample.int(n, size = size, replace = TRUE,
                                    prob = prob),
                 n = n)
    }
    map(probs, f)
  }
}
