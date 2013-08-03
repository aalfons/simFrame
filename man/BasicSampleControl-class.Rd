\name{BasicSampleControl-class}
\Rdversion{1.1}
\docType{class}
\alias{BasicSampleControl-class}
\alias{BasicSampleControl}
\alias{getCollect,BasicSampleControl-method}
\alias{getDots,BasicSampleControl-method}
\alias{getFun,BasicSampleControl-method}
\alias{getProb,BasicSampleControl-method}
\alias{getSize,BasicSampleControl-method}
\alias{show,BasicSampleControl-method}

\title{Class \code{"BasicSampleControl"}}
\description{
  Class for controlling the setup of samples using basic sampling designs.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{BasicSampleControl(\dots)} 
or \code{SampleControl(\dots, sub = "Basic")}.
}
\section{Slots}{
  \describe{
    \item{\code{design}:}{An optional character, integer or logical vector 
      specifying variables (columns) to be used for stratified sampling.}
    \item{\code{grouping}:}{An optional character, integer or logical vector 
      specifying a grouping variable (column) to be used for sampling whole 
      groups rather than individual observations.}
    \item{\code{collect}:}{A logical indicating how to sample groups of 
      observations.  If a grouping variable is specified and this is 
      \code{FALSE} (the default), groups are sampled directly.  If grouping 
      variable is specified and this is \code{TRUE}, individuals are sampled 
      in a first step.  In a second step, all individuals that belong to the 
      same group as any of the sampled individuals are collected and added to 
      the sample.  If no grouping variable is specified, this is ignored.}
    \item{\code{fun}:}{A function to be used for sampling (defaults to 
      \code{\link{srs}}).  It should return a vector containing the indices of 
      the sampled items (observations or groups).}
    \item{\code{size}:}{An optional non-negative integer giving the number of 
      items (observations or groups) to sample.  In case of stratified 
      sampling, a vector of non-negative integers, each giving the number of 
      items to sample from the corresponding stratum, may be supplied.}
    \item{\code{prob}:}{An optional character, integer or logical vector 
      specifying a variable (column) that contains probability weights.}
    \item{\code{dots}:}{A list containing additional arguments to be passed to 
      \code{fun}.}
    \item{\code{k}:}{A positive integer giving the number of samples to be set 
      up.}
    \item{\code{seed}:}{An integer to be used as initial seed of the random 
      number generator.  If not supplied explicitly, the seed will be based on 
      the date and time of construction.}
  }
}
\section{Details}{
  There are some restrictions on the argument names of the function supplied to 
  \code{fun}.  If it needs population data as input, the corresponding argument 
  should be called \code{x} and should expect a data frame.  If the sampling 
  method only needs the population size as input, the argument should be called 
  \code{N}.  Note that \code{fun} is not expected to have both \code{x} and 
  \code{N} as arguments, and that the latter is much faster for stratified 
  sampling or group sampling.  Furthermore, if the function has arguments for 
  sample size and probability weights, they should be called \code{size} and 
  \code{prob}, respectively.  Note that a function with \code{prob} as its only 
  argument is perfectly valid (for probability proportional to size 
  sampling).  Further arguments of \code{fun} may be supplied as a list via the 
  slot \code{dots}.
}
\section{Extends}{
Class \code{"\linkS4class{SampleControl}"}, directly.
Class \code{"\linkS4class{VirtualSampleControl}"}, distance 2.
Class \code{"\linkS4class{OptSampleControl}"}, distance 3.
}
\section{Accessor methods}{
  The following accessor methods are available in addition to the inherited 
  ones:
  
  \describe{
    \item{\code{getCollect}}{\code{signature(x = "BasicSampleControl")}: get 
      slot \code{collect}.}
    \item{\code{getFun}}{\code{signature(x = "BasicSampleControl")}: get slot 
      \code{fun}.}
    \item{\code{getSize}}{\code{signature(x = "BasicSampleControl")}: get slot 
      \code{size}.}
    \item{\code{getProb}}{\code{signature(x = "BasicSampleControl")}: get slot 
      \code{prob}.}
    \item{\code{getDots}}{\code{signature(x = "BasicSampleControl")}: get slot 
      \code{dots}.}
  }
}
\section{Methods}{
  The following methods are available in addition to the inherited ones:
  
  \describe{
    \item{\code{setup}}{\code{signature(x = "data.frame", 
      control = "BasicSampleControl")}: set up multiple samples.}
    \item{\code{show}}{\code{signature(object = "BasicSampleControl")}: print 
      the object on the \R console.}
  }
}
\section{UML class diagram}{
  A slightly simplified UML class diagram of the framework can be found in 
  Figure 1 of the package vignette \emph{An Object-Oriented Framework for 
  Statistical Simulation: The \R Package \code{simFrame}}.  Use 
  \code{vignette("simFrame-intro")} to view this vignette.
}
\author{Andreas Alfons}
\references{
Alfons, A., Templ, M. and Filzmoser, P. (2010) An Object-Oriented Framework for 
Statistical Simulation: The \R Package \pkg{simFrame}. \emph{Journal of 
Statistical Software}, \bold{37}(3), 1--36. URL 
\url{http://www.jstatsoft.org/v37/i03/}.
}
\seealso{
  \code{"\linkS4class{TwoStageSampleControl}"}, 
  \code{"\linkS4class{SampleSetup}"}, 
  \code{\link{setup}}, \code{\link{draw}}
}
\examples{
data("eusilcP")

## simple random sampling
srsc <- BasicSampleControl(size = 20)
draw(eusilcP[, c("id", "eqIncome")], srsc)

## group sampling
gsc <- BasicSampleControl(grouping = "hid", size = 10)
draw(eusilcP[, c("hid", "hid", "eqIncome")], gsc)

## stratified simple random sampling
ssrsc <- BasicSampleControl(design = "region", 
    size = c(2, 5, 5, 3, 4, 5, 3, 5, 2))
draw(eusilcP[, c("id", "region", "eqIncome")], ssrsc)

## stratified group sampling
sgsc <- BasicSampleControl(design = "region", grouping = "hid", 
    size = c(2, 5, 5, 3, 4, 5, 3, 5, 2))
draw(eusilcP[, c("hid", "id", "region", "eqIncome")], sgsc)
}
\keyword{classes}
