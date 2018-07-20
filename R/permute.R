#' Generate permutation subsets
#'
#' Generate `R` replicates with permutations of the rows or groups of a data frame.
#'
#' @details If `R` is greater than the total number of permutations,
#'   only the total number of permutations is returned. Since the total number
#'   of permutations is `factorial(n)`, this will only happen if `n`
#'   is small.
#'
#' @template param-n
#' @template param-times
#' @param force_random If `TRUE`, then generate random permutations, even
#'    if `n` is small enough that `R` is more than the number of
#'    permutations.
#'
#' @export
permute <- function(n, times = 1L, force_random = FALSE) {
  # allow for full set of permutations if someone really wants it
  # return full set of permutations if R >= n!
  if (is.infinite(times) || (log(times) >= lfactorial(n) && !force_random)) {
    combinat::permn(times, fun = function(x) sample_idx(in_id = x, n = n))
  } else {
    rerun(times, sample_idx(in_id = sample.int(n, n, replace = FALSE),
                            n = n))
  }
}
