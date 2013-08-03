\name{VirtualSampleControl-class}
\Rdversion{1.1}
\docType{class}
\alias{VirtualSampleControl-class}
\alias{getK,VirtualSampleControl-method}
\alias{getSeed,VirtualSampleControl-method}
\alias{show,VirtualSampleControl-method}

\title{Class \code{"VirtualSampleControl"}}
\description{
  Virtual superclass for controlling the setup of samples.
}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}
\section{Slots}{
  \describe{
    \item{\code{k}:}{A positive integer giving the number of samples to be set 
      up.}
    \item{\code{seed}:}{An integer to be used as initial seed of the random 
      number generator.  If not supplied explicitly, the seed will be based on 
      the date and time of construction.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{OptSampleControl}"}, directly.
}
\section{Accessor methods}{
  \describe{
    \item{\code{getK}}{\code{signature(x = "VirtualSampleControl")}: get slot 
      \code{k}.}
    \item{\code{getSeed}}{\code{signature(x = "VirtualSampleControl")}: get 
      slot \code{seed}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{draw}}{\code{signature(x = "data.frame", 
      setup = "VirtualSampleControl")}: draw a sample.}
    \item{\code{head}}{\code{signature(x = "VirtualSampleControl")}: currently 
      returns the object itself.}
    \item{\code{length}}{\code{signature(x = "VirtualSampleControl")}: get the 
      number of samples to be set up.}
    \item{\code{runSimulation}}{\code{signature(x = "data.frame", 
      setup = "VirtualSampleControl", nrep = "missing", 
      control = "SimControl")}: run a simulation experiment.}
    \item{\code{runSimulation}}{\code{signature(x = "VirtualDataControl", 
      setup = "VirtualSampleControl", nrep = "numeric", 
      control = "SimControl")}: run a simulation experiment.}
    \item{\code{show}}{\code{signature(object = "VirtualSampleControl")}: print 
      the object on the \R console.}
    \item{\code{summary}}{\code{signature(object = "VirtualSampleControl")}: 
      currently returns the object itself.}
    \item{\code{tail}}{\code{signature(x = "VirtualSampleControl")}: currently 
      returns the object itself.}
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
  \code{"\linkS4class{BasicSampleControl}"}, 
  \code{"\linkS4class{TwoStageSampleControl}"}, 
  \code{"\linkS4class{SampleSetup}"}, 
  \code{\link{setup}}, \code{\link{draw}}
}
\keyword{classes}
