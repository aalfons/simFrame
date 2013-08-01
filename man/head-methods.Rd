\name{head-methods}
\docType{methods}
\alias{head-methods}
\alias{head,SampleSetup-method}
\alias{head,SimControl-method}
\alias{head,SimResults-method}
\alias{head,VirtualContControl-method}
\alias{head,VirtualDataControl-method}
\alias{head,VirtualNAControl-method}
\alias{head,VirtualSampleControl-method}

\title{Methods for returning the first parts of an object}
\description{
  Return the first parts of an object.
}
\usage{
\S4method{head}{SampleSetup}(x, k = 6, n = 6, \dots)

\S4method{head}{SimControl}(x)

\S4method{head}{SimResults}(x, \dots)

\S4method{head}{VirtualContControl}(x)

\S4method{head}{VirtualDataControl}(x)

\S4method{head}{VirtualNAControl}(x)

\S4method{head}{VirtualSampleControl}(x)
}
\arguments{
  \item{x}{an object.}
  \item{k}{the number of set up samples to be kept in the resulting object.}
  \item{n}{the number of indices to be kept in each of the set up samples in 
    the resulting object.}
  \item{\dots}{additional arguments to be passed down to methods.}
}
\value{
  An object of the same class as \code{x}, but in general smaller.  See the 
  \dQuote{Methods} section below for details.
}
\section{Methods}{
  \describe{
  \item{\code{signature(x = "SampleSetup")}}{returns the first parts of set up 
    samples. The first \code{n} indices of each of the first \code{k} set up 
    samples are kept.}
  \item{\code{signature(x = "SimControl")}}{currently returns the 
    object itself.}
  \item{\code{signature(x = "SimResults")}}{returns the first parts of 
    simulation results.  The method of \code{\link[utils]{head}} for the 
    \code{data.frame} in slot \code{values} is thereby called.}
  \item{\code{signature(x = "VirtualContControl")}}{currently returns the 
    object itself.}
  \item{\code{signature(x = "VirtualDataControl")}}{currently returns the 
    object itself.}
  \item{\code{signature(x = "VirtualNAControl")}}{currently returns the 
    object itself.}
  \item{\code{signature(x = "VirtualSampleControl")}}{currently returns the 
    object itself.}
  }
}
\author{Andreas Alfons}
\references{
Alfons, A., Templ, M. and Filzmoser, P. (2010) An Object-Oriented Framework for 
Statistical Simulation: The \R Package \pkg{simFrame}. \emph{Journal of 
Statistical Software}, \bold{37}(3), 1--36. URL 
\url{http://www.jstatsoft.org/v37/i03/}.
}
\seealso{
  \code{\link[utils]{head}}, \code{"\linkS4class{SampleSetup}"}, 
  \code{"\linkS4class{SimResults}"}
}
\examples{
## load data
data("eusilcP")

## class "SampleSetup"
# set up samples using group sampling
set <- setup(eusilcP, grouping = "hid", size = 1000, 
    k = 50, seed = 12345)
summary(set)
# get the first 10 indices of each of the first 5 samples
head(set, k = 5, n = 10)
}
\keyword{methods}
\keyword{manip}
