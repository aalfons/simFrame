\name{SampleControl-class}
\Rdversion{1.1}
\docType{class}
\alias{SampleControl-class}
\alias{getDesign,SampleControl-method}
\alias{getGrouping,SampleControl-method}
\alias{show,SampleControl-method}

\title{Class \code{"SampleControl"}}
\description{
  Virtual class for controlling the setup of samples. It is used internally and 
  is not expected to be extended by users.
}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}
\section{Slots}{
  \describe{
    \item{\code{design}:}{An optional character, integer or logical vector 
      specifying variables (columns) to be used for stratified sampling.}
    \item{\code{grouping}:}{A character, integer or logical vector specifying 
      grouping variables (columns) to be used for (multi-stage) sampling of 
      whole groups rather than individual observations.}
    \item{\code{k}:}{A positive integer giving the number of samples to be set 
      up.}
    \item{\code{seed}:}{An optional integer to be used as initial seed of the 
      random number generator.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{VirtualSampleControl}"}, directly.
Class \code{"\linkS4class{OptSampleControl}"}, distance 2.
}
\section{Accessor methods}{
  The following accessor methods are available in addition to the inherited 
  ones:
  
  \describe{
    \item{\code{getDesign}}{\code{signature(x = "SampleControl")}: get slot 
      \code{design}.}
    \item{\code{getGrouping}}{\code{signature(x = "SampleControl")}: get slot 
      \code{grouping}.}
   }
}
\section{Methods}{
  The following methods are available in addition to the inherited ones:
  
  \describe{
    \item{\code{show}}{\code{signature(object = "SampleControl")}: print the 
      object on the \R console.}
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
