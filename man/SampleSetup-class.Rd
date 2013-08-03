\name{SampleSetup-class}
\Rdversion{1.1}
\docType{class}
\alias{SampleSetup-class}
\alias{SampleSetup}
\alias{getCall,SampleSetup-method}
\alias{getControl,SampleSetup-method}
\alias{getIndices,SampleSetup-method}
\alias{getProb,SampleSetup-method}
\alias{show,SampleSetup-method}

\title{Class \code{"SampleSetup"}}
\description{Class for set up samples.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{SampleSetup(\dots)}.  However, 
the constructor function is not supposed to be called directly by the user.

Instead, objects are expected to be created by the function \code{\link{setup}}. 
}
\section{Slots}{
  \describe{
    \item{\code{indices}:}{A list in which each component contains the indices 
      of the sampled observations.}
    \item{\code{prob}:}{An optional numeric vector giving the inclusion 
      probabilities.}
    \item{\code{control}:}{The control object (inheriting from class 
      \code{"\linkS4class{VirtualSampleControl}"}) used to set up the samples.}
    \item{\code{call}:}{The function call used to set up the samples, or 
      \code{NULL}.}
  }
}
\section{Accessor methods}{
  \describe{
    \item{\code{getIndices}}{\code{signature(x = "SampleSetup")}: get slot 
      \code{indices}.}
    \item{\code{getProb}}{\code{signature(x = "SampleSetup")}: get slot 
      \code{prob}.}
    \item{\code{getControl}}{\code{signature(x = "SampleSetup")}: get slot 
      \code{control}.}
    \item{\code{getCall}}{\code{signature(x = "SampleSetup")}: get slot 
      \code{call}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{draw}}{\code{signature(x = "data.frame", 
      setup = "SampleSetup")}: draw a sample.}
    \item{\code{head}}{\code{signature(x = "SampleSetup")}: returns the first 
      parts of set up samples.}
    \item{\code{length}}{\code{signature(x = "SampleSetup")}: get the number of 
      set up samples.}
    \item{\code{runSimulation}}{\code{signature(x = "data.frame", 
      setup = "SampleSetup", nrep = "missing", control = "SimControl")}: run a 
      simulation experiment.}
    \item{\code{show}}{\code{signature(object = "SampleSetup")}: print set up 
      samples on the \R console.}
    \item{\code{summary}}{\code{signature(object = "SampleSetup")}: produce a 
      summary of set up samples.}
    \item{\code{tail}}{\code{signature(x = "SampleSetup")}: returns the last 
      parts of set up samples.}
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
  \code{\link{setup}}, \code{\link{draw}}
}
\keyword{classes}
