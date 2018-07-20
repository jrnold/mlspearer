complement <- function(idx, n) {
  base::setdiff(idx, n)
}

sample_idx <- function(in_id = NULL, out_id = NULL, n = NULL) {
  list(in_id = in_id, out_id = out_id, n = n)
}

as.integer.sample_idx <- function(x, out = FALSE) {
  if (out) {
    x$out_id %||% complement(x$out_id, x$n)
  } else {
    x$in_id %||% complement(x$in_id, x$n)
  }
}


