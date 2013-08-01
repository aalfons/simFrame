\name{SimResults-class}
\Rdversion{1.1}
\docType{class}
\alias{SimResults-class}
\alias{SimResults}
\alias{aggregate,SimResults-method}
\alias{autoplot,SimResults-method}
\alias{fortify,SimResults-method}
\alias{getCall,SimResults-method}
\alias{getColnames,SimResults-method}
\alias{getControl,SimResults-method}
\alias{getInfo,SimResults-method}
\alias{getNrep,SimResults-method}
\alias{getValues,SimResults-method}
\alias{head,SimResults-method}
\alias{plot,SimResults,missing-method}
\alias{show,SimResults-method}
\alias{simPlot,SimResults-method}
\alias{subset,SimResults-method}
\alias{summary,SimResults-method}
\alias{tail,SimResults-method}

\title{Class \code{"SimResults"}}
\description{Class for simulation results.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{SimResults(\dots)}.  However, 
the constructor function is not supposed to be called directly by the user.

Instead, objects are expected to be created by the function 
\code{\link{runSimulation}}. 
}
\section{Slots}{
  \describe{
    \item{\code{values}:}{Object of class \code{"data.frame"} containing the 
      simulation results.}
    \item{\code{colnames}:}{Object of class \code{"character"} giving the names 
      of the columns of \code{values} that contain the actual simulation 
      results.}
    \item{\code{info}:}{Object of class \code{"numeric"} giving the number of 
      configurations for different controls of the simulation experiment.}
    \item{\code{dataControl}:}{Object of class \code{"OptDataControl"}; the 
      control object used for data generation in model-based simulation, or 
      \code{NULL}.}
    \item{\code{sampleControl}:}{Object of class \code{"OptSampleControl"}; the 
      control object used for sampling in design-based simulation, or 
      \code{NULL}.}
    \item{\code{nrep}:}{Object of class \code{"numeric"} giving the number of 
      repetitions of the simulation experiment (for model-based simulation or 
      simulation based on real data).}
    \item{\code{control}:}{Object of class \code{"SimControl"}; the control 
      object used for running the simulations.}
    \item{\code{call}:}{Object of class \code{"OptCall"}; the function call 
       used to run the simulation experiment, or \code{NULL}.}
  }
}
\section{Accessor methods}{
  \describe{
    \item{\code{getValues}}{\code{signature(x = "SimResults")}: get slot 
      \code{values}.}
    \item{\code{getColnames}}{\code{signature(x = "SimResults")}: get slot 
      \code{colnames}.}
    \item{\code{getInfo}}{\code{signature(x = "SimResults")}: get slot 
      \code{info}.}
    \item{\code{getControl}}{\code{signature(x = "SimResults")}: get control 
      objects of a simulation experiment.}
    \item{\code{getNrep}}{\code{signature(x = "SimResults")}: get slot 
      \code{nrep}.}
    \item{\code{getCall}}{\code{signature(x = "SimResults")}: get slot 
      \code{call}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{aggregate}}{\code{signature(x = "SimResults")}: aggregate 
      simulation results.}
    \item{autoplot}{\code{signature(object = "SimResults")}: selects a suitable 
      graphical representation of the simulation results automatically.}
    \item{fortify}{\code{signature(model = "SimResults")}: convert simulation 
      results to a \code{data.frame} for plotting.}
    \item{\code{head}}{\code{signature(x = "SimResults")}: returns the first 
      parts of simulation results.}
    \item{\code{plot}}{\code{signature(x = "SimResults", y = "missing")}: 
      selects a suitable graphical representation of the simulation results 
      automatically.}
    \item{\code{show}}{\code{signature(object = "SimResults")}: print 
      simulation results on the \R console.}
    \item{simPlot}{\code{signature(object = "SimResults")}: selects a suitable 
      graphical representation of the simulation results automatically.}
    \item{subset}{\code{signature(x = "SimResults")}: take a subset of 
      simulation results.}
    \item{\code{summary}}{\code{signature(x = "SimResults")}: produce a summary 
      of simulation results.}
    \item{\code{tail}}{\code{signature(x = "SimResults")}: returns the last 
      parts of simulation results.}
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
  \code{\link{runSimulation}}, \code{\link{simPlot}}
}
\keyword{classes}
