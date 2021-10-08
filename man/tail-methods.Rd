\name{tail-methods}
\docType{methods}
\alias{tail-methods}
\alias{tail,SampleSetup-method}
\alias{tail,SimControl-method}
\alias{tail,SimResults-method}
\alias{tail,Strata-method}
\alias{tail,VirtualContControl-method}
\alias{tail,VirtualDataControl-method}
\alias{tail,VirtualNAControl-method}
\alias{tail,VirtualSampleControl-method}
\title{Methods for returning the last parts of an object}
\description{
  Return the last parts of an object.
}
\usage{
\S4method{tail}{SampleSetup}(x, k = 6, n = 6, \dots)

\S4method{tail}{SimControl}(x)

\S4method{tail}{SimResults}(x, \dots)

\S4method{tail}{Strata}(x, \dots)

\S4method{tail}{VirtualContControl}(x)

\S4method{tail}{VirtualDataControl}(x)

\S4method{tail}{VirtualNAControl}(x)

\S4method{tail}{VirtualSampleControl}(x)
}
\arguments{
  \item{x}{an object.}
  \item{k}{for objects of class \code{"SampleSetup"}, the number of set up
    samples to be kept in the resulting object.}
  \item{n}{for objects of class \code{"SampleSetup"}, the number of indices
    to be kept in each of the set up samples in the resulting object.}
  \item{\dots}{additional arguments to be passed down to methods.}
}
\value{
  An object of the same class as \code{x}, but in general smaller.  See the
  \dQuote{Methods} section below for details.
}
\section{Methods}{
  \describe{
  \item{\code{signature(x = "SampleSetup")}}{returns the last parts of set up
    samples. The last \code{n} indices of each of the last \code{k} set up
    samples are kept.}
  \item{\code{signature(x = "SimControl")}}{currently returns the
    object itself.}
  \item{\code{signature(x = "SimResults")}}{returns the last parts of
    simulation results.  The method of \code{\link[utils:head]{tail}} for the
    \code{data.frame} in slot \code{values} is thereby called.}
  \item{\code{signature(x = "Strata")}}{returns the last parts of strata
    information.  The method of \code{\link[utils:head]{tail}} for the vector in
    slot \code{values} is thereby called and the slots \code{split} and
    \code{size} are adapted accordingly.}
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
Statistical Software}, \bold{37}(3), 1--36. \doi{10.18637/jss.v037.i03}.
}
\seealso{
  \code{\link[utils:head]{tail}}, \code{"\linkS4class{SampleSetup}"},
  \code{"\linkS4class{SimResults}"}, \code{"\linkS4class{Strata}"}
}
\examples{
## load data
data(eusilcP)

## class "SampleSetup"
# set up samples using group sampling
set <- setup(eusilcP, grouping = "hid", size = 1000, k = 50)
summary(set)
# get the last 10 indices of each of the last 5 samples
tail(set, k = 5, n = 10)

## class "Strata"
# set up samples using group sampling
strata <- stratify(eusilcP, "region")
summary(strata)
# get strata information for the last 10 observations
tail(strata, 10)
}
\keyword{methods}
\keyword{manip}
