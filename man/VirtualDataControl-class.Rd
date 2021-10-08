\name{VirtualDataControl-class}
\Rdversion{1.1}
\docType{class}
\alias{VirtualDataControl-class}
%% aliases to avoid confusion due to capitalization
\alias{virtualDataControl-class}
\alias{virtualdataControl-class}
\alias{virtualdatacontrol-class}
\alias{virtualDatacontrol-class}
\alias{VirtualdataControl-class}
\alias{VirtualDatacontrol-class}
\alias{Virtualdatacontrol-class}

\title{Class "VirtualDataControl"}
\description{
  Virtual superclass for controlling model-based generation of data.
}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}
\section{Extends}{
Class \code{"\linkS4class{OptDataControl}"}, directly.
}
\section{Methods}{
  \describe{
    \item{\code{clusterRunSimulation}}{\code{signature(cl = "ANY", 
      x = "VirtualDataControl", setup = "missing", nrep = "numeric", 
      control = "SimControl")}: run a simulation experiment on a cluster.}
    \item{\code{clusterRunSimulation}}{\code{signature(cl = "ANY", 
      x = "VirtualDataControl", setup = "VirtualSampleControl", 
      nrep = "numeric", control = "SimControl")}: run a simulation experiment 
      on a cluster.}
    \item{\code{head}}{\code{signature(x = "VirtualContControl")}: currently 
      returns the object itself.}
    \item{\code{runSimulation}}{\code{signature(x = "VirtualDataControl", 
      setup = "missing", nrep = "numeric", control = "SimControl")}: run a 
      simulation experiment.}
    \item{\code{runSimulation}}{\code{signature(x = "VirtualDataControl", 
      setup = "missing", nrep = "missing", control = "SimControl")}: run a 
      simulation experiment.}
    \item{\code{runSimulation}}{\code{signature(x = "VirtualDataControl", 
      setup = "VirtualSampleControl", nrep = "numeric", 
      control = "SimControl")}: run a simulation experiment.}
    \item{\code{runSimulation}}{\code{signature(x = "VirtualDataControl", 
      setup = "VirtualSampleControl", nrep = "missing", 
      control = "SimControl")}: run a simulation experiment.}
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
\examples{
showClass("VirtualDataControl")
}
\keyword{classes}
