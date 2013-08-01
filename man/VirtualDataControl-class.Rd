\name{VirtualDataControl-class}
\Rdversion{1.1}
\docType{class}
\alias{VirtualDataControl-class}
\alias{getSize,VirtualDataControl-method}
\alias{head,VirtualDataControl-method}
\alias{length,VirtualDataControl-method}
\alias{runSimulation,VirtualDataControl,missing,numeric,SimControl-method}
\alias{runSimulation,VirtualDataControl,VirtualSampleControl,numeric,SimControl-method}
\alias{show,VirtualDataControl-method}
\alias{summary,VirtualDataControl-method}
\alias{tail,VirtualDataControl-method}

\title{Class \code{"VirtualDataControl"}}
\description{
  Virtual superclass for controlling model-based generation of data.
}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}
\section{Extends}{
Class \code{"\linkS4class{OptDataControl}"}, directly.
}
\section{Accessor methods}{
  \describe{
    \item{\code{getSize}}{\code{signature(x = "DataControl")}: get slot 
      \code{size}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{head}}{\code{signature(x = "VirtualContControl")}: currently 
      returns the object itself.}
    \item{\code{length}}{\code{signature(x = "VirtualDataControl")}: get the 
      number of data sizes to be used.}
    \item{runSimulation}{\code{signature(x = "VirtualDataControl", 
      setup = "missing", nrep = "numeric", control = "SimControl")}: run a 
      simulation experiment.}
    \item{runSimulation}{\code{signature(x = "VirtualDataControl", 
      setup = "VirtualSampleControl", nrep = "numeric", 
      control = "SimControl")}: run a simulation experiment.}
    \item{show}{\code{signature(object = "VirtualDataControl")}: print the 
      object on the \R console.}
    \item{\code{summary}}{\code{signature(object = "VirtualContControl")}: 
      currently returns the object itself.}
    \item{\code{tail}}{\code{signature(x = "VirtualContControl")}: currently 
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
  \code{"\linkS4class{DataControl}"}, \code{\link{generate}}
}
\keyword{classes}
