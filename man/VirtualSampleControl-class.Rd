\name{VirtualSampleControl-class}
\Rdversion{1.1}
\docType{class}
\alias{VirtualSampleControl-class}
\alias{getK,VirtualSampleControl-method}
\alias{setK,VirtualSampleControl-method}
\alias{show,VirtualSampleControl-method}
%% aliases to avoid confusion due to capitalization
\alias{virtualSampleControl-class}
\alias{virtualsampleControl-class}
\alias{virtualsamplecontrol-class}
\alias{virtualSamplecontrol-class}
\alias{VirtualsampleControl-class}
\alias{VirtualSamplecontrol-class}
\alias{Virtualsamplecontrol-class}

\title{Class "VirtualSampleControl"}
\description{
  Virtual superclass for controlling the setup of samples.
}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}
\section{Slots}{
  \describe{
    \item{\code{k}:}{Object of class \code{"numeric"}, a single positive 
      integer giving the number of samples to be set up.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{OptSampleControl}"}, directly.
}
\section{Accessor and mutator methods}{
  \describe{
    \item{\code{getK}}{\code{signature(x = "VirtualSampleControl")}: get slot 
      \code{k}.}
    \item{\code{setK}}{\code{signature(x = "VirtualSampleControl")}: set slot 
      \code{k}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{clusterRunSimulation}}{\code{signature(cl = "ANY", 
      x = "data.frame", setup = "VirtualSampleControl", nrep = "missing", 
      control = "SimControl")}: run a simulation experiment on a cluster.}
    \item{\code{clusterRunSimulation}}{\code{signature(cl = "ANY", 
      x = "VirtualDataControl", setup = "VirtualSampleControl", 
      nrep = "numeric", control = "SimControl")}: run a simulation experiment 
      on a cluster.}
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
    \item{\code{runSimulation}}{\code{signature(x = "VirtualDataControl", 
      setup = "VirtualSampleControl", nrep = "missing", 
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
  \code{"\linkS4class{SampleControl}"}, \code{"\linkS4class{TwoStageControl}"}, 
  \code{"\linkS4class{SampleSetup}"}, \code{\link{setup}}, \code{\link{draw}}
}
\examples{
showClass("VirtualSampleControl")
}
\keyword{classes}
