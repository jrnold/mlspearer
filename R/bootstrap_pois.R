#' Generate Poisson bootstrap replicates
#'
#' @template param-n
#' @template param-times
#'
#' @importFrom stats rpois
#' @export
bootstrap_pois <- function(n, times) {
  rerun(times, bootstrap_pois_one)
}

bootstrap_pois_one <- function(n, lambda = 1) {
  sample_idx(in_id = rep.int(seq_len(n), rpois(n, lambda)), n = n)
}
