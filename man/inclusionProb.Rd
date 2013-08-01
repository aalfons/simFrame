\name{inclusionProb}
\alias{inclusionProb}
\title{Inclusion probabilities}
\usage{
  inclusionProb(prob, size)
}
\arguments{
  \item{prob}{a numeric vector of non-negative probability
  weights.}

  \item{size}{a non-negative integer giving the sample
  size.}
}
\value{
  A numeric vector of the first-order inclusion
  probabilities.
}
\description{
  Get the first-order inclusion probabilities from a vector
  of probability weights.
}
\note{
  This is a faster C++ implementation of
  \code{\link[sampling]{inclusionprobabilities}} from
  package \code{sampling}.
}
\examples{
pweights <- sample(1:5, 25, replace = TRUE)
inclusionProb(pweights, 10)
}
\author{
  Andreas Alfons
}
\seealso{
  \code{\link{sampling}}, \code{\link{setup}},
  \code{"\linkS4class{SampleSetup}"}
}
\keyword{distribution}
\keyword{survey}

