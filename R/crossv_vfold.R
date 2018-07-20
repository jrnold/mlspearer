#' Generate cross-validated v-fold test-training pairs
#'
#' @template param-n
#' @param v A scalar integer representing the number of folds in cross
#'   validation.
#' @param shuffle A flag which indicates whether to randomly assign
#'   observations to folds.
#'
#' @references
#'
#' -   Breiman, L., Friedman, J.H., Olshen, R.A. and Stone, C.J. (1984) *Classification and Regression Trees*. Wadsworth.
#' -   Burman, P. (1989) "A comparative study of ordinary cross-validation,
#'     v-fold cross-validation and repeated learning-testing methods."
#'     *Biometrika*, 76, 503–514
#' -   Davison, A.C. and Hinkley, D.V. (1997)
#'     *Bootstrap Methods and Their Application*. Cambridge University Press.
#' -   Efron, B. (1986) "How biased is the apparent error rate of a
#'     prediction rule?" *Journal of the American Statistical Association*,
#'     81, 461–470.
#' -   Stone, M. (1974) "Cross-validation choice and assessment of statistical
#'     predictions (with Discussion)".
#'     *Journal of the Royal Statistical Society*, B, 36, 111–147.
#'
#' @export
#' @example inst/examples/ex-crossv_vfold.R
crossv_vfold <- function(n, v = 5L, shuffle = TRUE) {
  f <- function(idx) {
    sample_idx(out_id = idx, n = n)
  }
  map(partition(seq_len(n), as.integer(v), shuffle = shuffle), f)
}

partition <- function(x, v, shuffle = TRUE) {
  if (shuffle) {
    n <- length(x)
    folds <- sample(rep(seq_len(v), length.out = n), size = n, replace = FALSE)
  } else {
    folds <- cut(x, v, include.lowest = TRUE, labels = FALSE)
  }
  split(x, folds)
}
