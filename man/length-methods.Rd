\name{length-methods}
\docType{methods}
\alias{length-methods}
\alias{length,ContControl-method}
\alias{length,DataControl-method}
\alias{length,SampleSetup-method}
\alias{length,VirtualContControl-method}
\alias{length,VirtualDataControl-method}
\alias{length,VirtualNAControl-method}
\alias{length,VirtualSampleControl-method}

\title{Methods for getting the length of an object}
\description{
  Get the length of an object.
}
\usage{
\S4method{length}{ContControl}(x)

\S4method{length}{DataControl}(x)

\S4method{length}{SampleSetup}(x)

\S4method{length}{VirtualContControl}(x)

\S4method{length}{VirtualDataControl}(x)

\S4method{length}{VirtualNAControl}(x)

\S4method{length}{VirtualSampleControl}(x)
}
\arguments{
  \item{x}{an object.}
}
\value{
  An integer giving the length of the object.  See the \dQuote{Methods} section 
  below for details. 
}
\section{Methods}{
  \describe{
  \item{\code{signature(x = "ContControl")}}{get the number of 
    contamination settings to be used.}
  \item{\code{signature(x = "DataControl")}}{get the number of data 
    configurations to be used.}
  \item{\code{signature(x = "SampleSetup")}}{get the number of set up samples.}
  \item{\code{signature(x = "VirtualContControl")}}{get the number of 
    contamination levels to be used.}
  \item{\code{signature(x = "VirtualDataControl")}}{get the number of data 
    sizes to be used.}
  \item{\code{signature(x = "VirtualNAControl")}}{get the number of missing 
    value rates to be used (i.e., the number of rows of the matrix in slot 
    \code{NARate}).}
  \item{\code{signature(x = "VirtualSampleControl")}}{get the number of samples 
    to be set up.}
  }
}
\author{Andreas Alfons}
\seealso{
  \code{\link[base]{length}}
}
\examples{
## load data
data("eusilcP")

## class "SampleSetup"
# set up samples using group sampling
set <- setup(eusilcP, grouping = "hid", size = 1000, 
    k = 50, seed = 12345)
summary(set)
length(set)

## class "ContControl"
cc <- ContControl(target = "eqIncome", 
    epsilon = c(0, 0.0025, 0.005, 0.0075, 0.01), 
    dots = list(mean = 5e+05, sd = 10000), type = "CCAR")
length(cc)

## class "NAControl"
nc <- NAControl(target = "eqIncome", NARate = c(0.1, 0.2, 0.3))
length(nc)
}
\keyword{methods}
\keyword{attribute}
