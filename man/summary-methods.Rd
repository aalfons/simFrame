\name{summary-methods}
\docType{methods}
\alias{summary-methods}
\alias{summary,SampleSetup-method}
\alias{summary,SimControl-method}
\alias{summary,SimResults-method}
\alias{summary,VirtualContControl-method}
\alias{summary,VirtualDataControl-method}
\alias{summary,VirtualNAControl-method}
\alias{summary,VirtualSampleControl-method}

\title{Methods for producing a summary of an object}
\description{
  Produce a summary an object.
}
\usage{
\S4method{summary}{SampleSetup}(object)

\S4method{summary}{SimControl}(object)

\S4method{summary}{SimResults}(object, \dots)

\S4method{summary}{VirtualContControl}(object)

\S4method{summary}{VirtualDataControl}(object)

\S4method{summary}{VirtualNAControl}(object)

\S4method{summary}{VirtualSampleControl}(object)
}
\arguments{
  \item{object}{an object.}
  \item{\dots}{additional arguments to be passed down to methods.}
}
\value{
  The form of the resulting object depends on the class of the argument 
  \code{object}.  See the \dQuote{Methods} section below for details.
}
\section{Methods}{
  \describe{
  \item{\code{signature(x = "SampleSetup")}}{returns an object of class 
    \code{SummarySampleSetup}, which contains information on the size of each 
    of the set up samples.}
  \item{\code{signature(x = "SimControl")}}{currently returns the 
    object itself.}
  \item{\code{signature(x = "SimResults")}}{produces a summary of the 
    simulation results by calling the method of \code{\link[base]{summary}} 
    for the \code{data.frame} in slot \code{values}.}
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
  \code{\link[base]{summary}}, \code{"\linkS4class{SampleSetup}"}, 
  \code{"\linkS4class{SummarySampleSetup}"}, \code{"\linkS4class{SimResults}"}
}
\examples{
## load data
data("eusilcP")

## class "SampleSetup"
# set up samples using group sampling
set <- setup(eusilcP, grouping = "hid", size = 1000, 
    k = 50, seed = 12345)
summary(set)
}
\keyword{methods}
