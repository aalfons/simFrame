\name{inclusionProb}
\Rdversion{1.1}
\alias{inclusionProb}
%% aliases to avoid confusion due to capitalization
\alias{inclusionprob}
\alias{InclusionProb}
\alias{inclusionprob}

\title{Inclusion probabilities}
\description{
Get the first-order inclusion probabilities from a vector of probability
weights.
}
\usage{
inclusionProb(prob, size)
}
\arguments{
  \item{prob}{a numeric vector of non-negative probability weights.}
  \item{size}{a non-negative integer giving the sample size.}
}
\value{
  A numeric vector of the first-order inclusion probabilities.
}
\note{
  This is a faster C++ implementation of
  \code{inclusionprobabilities} from package \code{sampling}.
}
\author{Andreas Alfons}
\seealso{
  \code{\link{setup}}, \code{"\linkS4class{SampleSetup}"}
}
\examples{
pweights <- sample(1:5, 25, replace = TRUE)
inclusionProb(pweights, 10)
}
\keyword{distribution}
\keyword{survey}
