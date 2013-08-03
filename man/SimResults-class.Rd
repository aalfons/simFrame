\name{SimResults-class}
\Rdversion{1.1}
\docType{class}
\alias{SimResults-class}
\alias{SimResults}
\alias{getCall,SimResults-method}
\alias{getColnames,SimResults-method}
\alias{getInfo,SimResults-method}
\alias{getNrep,SimResults-method}
\alias{getValues,SimResults-method}
\alias{show,SimResults-method}

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
    \item{\code{values}:}{A data frame containing the simulation results.}
    \item{\code{colnames}:}{A character vector giving the names of the columns 
      of \code{values} that contain the actual simulation results.}
    \item{\code{info}:}{A numeric vector giving the number of configurations 
      for different controls of the simulation experiment.}
    \item{\code{dataControl}:}{The control object used for data generation 
      (inheriting from class \code{"\linkS4class{VirtualDataControl}"}), or 
      \code{NULL}.}
    \item{\code{sampleControl}:}{The control object used for sampling 
      (inheriting from class \code{"\linkS4class{VirtualSampleControl}"}), or 
      \code{NULL}.}
    \item{\code{nrep}:}{An integer giving the number of repetitions of the 
      simulation experiment (if applicable).}
    \item{\code{control}:}{The control object of class 
      \code{"\linkS4class{SimControl}"} used for running the simulations.}
    \item{\code{call}:}{The function call used to run the simulation 
      experiment, or \code{NULL}.}
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
    \item{\code{autoplot}}{\code{signature(object = "SimResults")}: selects a 
      suitable graphical representation of the simulation results 
      automatically.}
    \item{\code{fortify}}{\code{signature(model = "SimResults")}: convert 
      simulation results to a data frame for plotting.}
    \item{\code{head}}{\code{signature(x = "SimResults")}: returns the first 
      parts of simulation results.}
    \item{\code{plot}}{\code{signature(x = "SimResults", y = "missing")}: 
      selects a suitable graphical representation of the simulation results 
      automatically.}
    \item{\code{show}}{\code{signature(object = "SimResults")}: print 
      simulation results on the \R console.}
    \item{\code{simPlot}}{\code{signature(object = "SimResults")}: selects a 
      suitable graphical representation of the simulation results 
      automatically.}
    \item{\code{subset}}{\code{signature(x = "SimResults")}: take a subset of 
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
