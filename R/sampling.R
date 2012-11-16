# ----------------------
# Author: Andreas Alfons
#         KU Leuven
# ----------------------

# simple random sampling
srs <- function(N, size, replace = FALSE) {
  if(N == 0) integer()
  else {
    if(length(N) > 1) N <- N[1]
    if(missing(size)) sample(N, replace=replace)
    else sample(N, size, replace)
  }
}

# unequal probability sampling
ups <- function(N, size, prob, replace = FALSE) {
  if(N == 0) integer()
  else {
    if(length(N) > 1) N <- N[1]
    sample(N, size, replace, prob)
  }
}
