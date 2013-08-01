# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

#' Random sampling
#' 
#' Functions for random sampling.
#' 
#' \code{srs} and \code{ups} are wrappers for simple random sampling and
#' unequal probability sampling, respectively.  Both functions make use of
#' \code{\link{sample}}.
#' 
#' \code{brewer}, \code{midzuno} and \code{tille} perform Brewer's, Midzuno's
#' and \enc{Tillé}{Tille}'s method, respectively, for unequal probability
#' sampling without replacement and fixed sample size.
#' 
#' @encoding utf8
#' @name sampling
#' 
#' @param N  a non-negative integer giving the number of observations from 
#' which to sample.
#' @param size  a non-negative integer giving the number of observations to
#' sample.
#' @param prob  for \code{ups}, a numeric vector giving the probability weights
#' (see \code{\link{sample}}).  For \code{tille} and \code{midzuno}, a vector
#' of inclusion probabilities (see \code{\link{inclusionProb}}).
#' @param replace  a logical indicating whether sampling should be performed
#' with or without replacement.
#' @param eps  a numeric control value giving the desired accuracy.
#' 
#' @return An integer vector giving the indices of the sampled observations.
#' 
#' @note \code{brewer}, \code{midzuno} and \code{tille} are faster C++
#' implementations of \code{\link[sampling]{UPbrewer}},
#' \code{\link[sampling]{UPmidzuno}} and \code{\link[sampling]{UPtille}},
#' respectively, from package \code{sampling}.
#' 
#' @author Andreas Alfons
#' 
#' @seealso \code{"\linkS4class{BasicSampleControl}"},
#' \code{"\linkS4class{TwoStageSampleControl}"}, \code{\link{setup}},
#' \code{\link{inclusionProb}}
#' 
#' @references Brewer, K. (1975), A simple procedure for sampling \eqn{\pi}{pi}
#' pswor, Australian Journal of Statistics, \bold{17}(3), 166-172.
#' 
#' Midzuno, H. (1952) On the sampling system with probability proportional to
#' sum of size. \emph{Annals of the Institute of Statistical Mathematics},
#' \bold{3}(2), 99--107.
#' 
#' \enc{Tillé}{Tille}, Y. (1996) An elimination procedure of unequal 
#' probability sampling without replacement. \emph{Biometrika}, \bold{83}(1), 
#' 238--241.
#' 
#' Deville, J.-C. and \enc{Tillé}{Tille}, Y. (1998) Unequal probability 
#' sampling without replacement through a splitting method. \emph{Biometrika},
#' \bold{85}(1), 89--101.
#' 
#' @examples
#' ## simple random sampling
#' # without replacement
#' srs(10, 5)
#' # with replacement
#' srs(5, 10, replace = TRUE)
#' 
#' ## unequal probability sampling
#' # without replacement
#' ups(10, 5, prob = 1:10)
#' # with replacement
#' ups(5, 10, prob = 1:5, replace = TRUE)
#' 
#' ## Brewer, Midzuno and Tille sampling
#' # define inclusion probabilities
#' prob <- c(0.2,0.7,0.8,0.5,0.4,0.4)
#' # Brewer sampling
#' brewer(prob)
#' # Midzuno sampling
#' midzuno(prob)
#' # Tille sampling
#' tille(prob)
#' 
#' @keywords distribution

NULL


#' @rdname sampling
#' @export

srs <- function(N, size, replace = FALSE) {
  if(N == 0) integer()
  else {
    if(length(N) > 1) N <- N[1]
    if(missing(size)) sample(N, replace=replace)
    else sample(N, size, replace)
  }
}


#' @rdname sampling
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


#' @rdname sampling
#' @useDynLib simFrame
#' @export

midzuno <- function(prob, eps = 1e-06) {
  prob <- 1 - tilleCpp(1 - prob, eps)  # call internal function for tille sampling
  which(prob >= 1 - eps)  # indices of sampled observations
}


#' @rdname sampling
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


#' @rdname sampling
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


#' Inclusion probabilities
#' 
#' Get the first-order inclusion probabilities from a vector of probability
#' weights.
#' 
#' @param prob a numeric vector of non-negative probability weights.
#' @param size a non-negative integer giving the sample size.
#' 
#' @return A numeric vector of the first-order inclusion probabilities.
#' 
#' @note This is a faster C++ implementation of
#' \code{\link[sampling]{inclusionprobabilities}} from package \code{sampling}.
#' 
#' @author Andreas Alfons
#' 
#' @seealso \code{\link{sampling}}, \code{\link{setup}}, 
#' \code{"\linkS4class{SampleSetup}"}
#' 
#' @examples
#' pweights <- sample(1:5, 25, replace = TRUE)
#' inclusionProb(pweights, 10)
#' 
#' @keywords distribution survey
#' 
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
