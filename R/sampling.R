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

# for internal use (in 'setNA')
samplex <- function(x, size, prob = NULL) {
  if(length(x) == 0) x
  else if(length(x) == 1) {
    if(!missing(size)) {
      if(isTRUE(size == 0)) x[FALSE]
      else if(isTRUE(size == 1)) x
      else stop("cannot take a sample larger than the population")
    } else x
  } else sample(x, size, prob = prob)
}
