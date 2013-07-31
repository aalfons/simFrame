# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

## simple random sampling
#' @export
srs <- function(N, size, replace = FALSE) {
  if(N == 0) integer()
  else {
    if(length(N) > 1) N <- N[1]
    if(missing(size)) sample(N, replace=replace)
    else sample(N, size, replace)
  }
}

## unequal probability sampling
#' @export
ups <- function(N, size, prob, replace = FALSE) {
  if(N == 0) integer()
  else {
    if(length(N) > 1) N <- N[1]
    sample(N, size, replace, prob)
  }
}

## for internal use (in 'setNA')
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

## Midzuno sampling
#' @useDynLib simFrame
#' @export
midzuno <- function(prob, eps = 1e-06) {
  prob <- 1 - tilleCpp(1 - prob, eps)  # call internal function for tille sampling
  which(prob >= 1 - eps)  # indices of sampled observations
}

## Tille sampling
#' @useDynLib simFrame
#' @export
tille <- function(prob, eps = 1e-06) {
  prob <- tilleCpp(prob, eps)  # call internal function
  which(prob >= 1 - eps)  # indices of sampled observations
}

## internal function for Tille sampling that calls the C++ function
tilleCpp <- function(prob, eps = 1e-06) {
  if(any(is.na(prob))) stop("there are missing values in 'prob'")
  list <- (prob > eps) & (prob < 1 - eps)  # indices of probabilities to be used
  probList <- prob[list]  # probabilities to be used
  if(length(probList) > 0) {
    prob[list] <- .Call("R_tille", R_prob=probList)  # call C++ function
  } else warning("all values in 'prob' outside the interval (eps, 1-eps)")
  prob
}

## Brewer sampling
#' @useDynLib simFrame
#' @export
brewer <- function(prob, eps = 1e-06) {
  if(any(is.na(prob))) stop("there are missing values in 'prob'")
  list <- (prob > eps) & (prob < 1 - eps)  # indices of probabilities to be used
  probList <- prob[list]  # probabilities to be used
  N <- length(probList)
  if(N < 1) stop("all values in 'prob' outside the interval (eps, 1-eps)")
  prob[list] <- .Call("R_brewer", R_prob=probList)  # call C++ function
  which(prob >= 1 - eps)  # indices of sampled observations
}

## compute inclusion probabilities
#' @useDynLib simFrame
#' @export
inclusionProb <- function(prob, size) {
  prob <- as.numeric(prob)
  size <- as.integer(size[1])
  if(length(prob) == 0) return(numeric())
  if(length(size) == 0 || size == 0) return(rep.int(0, length(prob)))
  if(size < 0) stop("'size' must be a non-negative integer")
  .Call("R_inclusionProb", R_prob=prob, R_size=size, PACKAGE="simFrame")
}
