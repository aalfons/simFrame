\encoding{utf8}
\name{sampling}
\alias{brewer}
\alias{midzuno}
\alias{sampling}
\alias{srs}
\alias{tille}
\alias{ups}
\title{Random sampling}
\usage{
  srs(N, size, replace = FALSE)

  ups(N, size, prob, replace = FALSE)

  midzuno(prob, eps = 1e-06)

  tille(prob, eps = 1e-06)

  brewer(prob, eps = 1e-06)
}
\arguments{
  \item{N}{a non-negative integer giving the number of
  observations from which to sample.}

  \item{size}{a non-negative integer giving the number of
  observations to sample.}

  \item{prob}{for \code{ups}, a numeric vector giving the
  probability weights (see \code{\link{sample}}).  For
  \code{tille} and \code{midzuno}, a vector of inclusion
  probabilities (see \code{\link{inclusionProb}}).}

  \item{replace}{a logical indicating whether sampling
  should be performed with or without replacement.}

  \item{eps}{a numeric control value giving the desired
  accuracy.}
}
\value{
  An integer vector giving the indices of the sampled
  observations.
}
\description{
  Functions for random sampling.
}
\details{
  \code{srs} and \code{ups} are wrappers for simple random
  sampling and unequal probability sampling, respectively.
  Both functions make use of \code{\link{sample}}.

  \code{brewer}, \code{midzuno} and \code{tille} perform
  Brewer's, Midzuno's and \enc{Tillé}{Tille}'s method,
  respectively, for unequal probability sampling without
  replacement and fixed sample size.
}
\note{
  \code{brewer}, \code{midzuno} and \code{tille} are faster
  C++ implementations of \code{\link[sampling]{UPbrewer}},
  \code{\link[sampling]{UPmidzuno}} and
  \code{\link[sampling]{UPtille}}, respectively, from
  package \code{sampling}.
}
\examples{
## simple random sampling
# without replacement
srs(10, 5)
# with replacement
srs(5, 10, replace = TRUE)

## unequal probability sampling
# without replacement
ups(10, 5, prob = 1:10)
# with replacement
ups(5, 10, prob = 1:5, replace = TRUE)

## Brewer, Midzuno and Tille sampling
# define inclusion probabilities
prob <- c(0.2,0.7,0.8,0.5,0.4,0.4)
# Brewer sampling
brewer(prob)
# Midzuno sampling
midzuno(prob)
# Tille sampling
tille(prob)
}
\author{
  Andreas Alfons
}
\references{
  Brewer, K. (1975), A simple procedure for sampling
  \eqn{\pi}{pi} pswor, Australian Journal of Statistics,
  \bold{17}(3), 166-172.

  Midzuno, H. (1952) On the sampling system with
  probability proportional to sum of size. \emph{Annals of
  the Institute of Statistical Mathematics}, \bold{3}(2),
  99--107.

  \enc{Tillé}{Tille}, Y. (1996) An elimination procedure of
  unequal probability sampling without replacement.
  \emph{Biometrika}, \bold{83}(1), 238--241.

  Deville, J.-C. and \enc{Tillé}{Tille}, Y. (1998) Unequal
  probability sampling without replacement through a
  splitting method. \emph{Biometrika}, \bold{85}(1),
  89--101.
}
\seealso{
  \code{"\linkS4class{BasicSampleControl}"},
  \code{"\linkS4class{TwoStageSampleControl}"},
  \code{\link{setup}}, \code{\link{inclusionProb}}
}
\keyword{distribution}

