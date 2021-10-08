\name{SimResults-class}
\Rdversion{1.1}
\docType{class}
\alias{SimResults-class}
\alias{SimResults}
\alias{getValues,SimResults-method}
\alias{getAdd,SimResults-method}
\alias{getDesign,SimResults-method}
\alias{getColnames,SimResults-method}
\alias{getEpsilon,SimResults-method}
\alias{getNArate,SimResults-method}
\alias{getDataControl,SimResults-method}
\alias{getSampleControl,SimResults-method}
\alias{getNrep,SimResults-method}
\alias{getControl,SimResults-method}
\alias{getSeed,SimResults-method}
\alias{getCall,SimResults-method}
\alias{show,SimResults-method}
%% aliases to avoid confusion due to capitalization
\alias{simResults-class}
\alias{simresults-class}
\alias{Simresults-class}
\alias{simResults}
\alias{simresults}
\alias{Simresults}

\title{Class "SimResults"}
\description{Class for simulation results.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SimResults", \dots)} or
\code{SimResults(\dots)}.

However, objects are expected to be created by the function
\code{\link{runSimulation}} or \code{\link{clusterRunSimulation}}, these
constructor functions are not supposed to be called by the user.
}
\section{Slots}{
  \describe{
    \item{\code{values}:}{Object of class \code{"data.frame"} containing the
      simulation results.}
    \item{\code{add}:}{Object of class \code{"list"} containing additional
      simulation results, e.g., statistical models.}
    \item{\code{design}:}{Object of class \code{"character"} giving the
      variables (columns) defining the domains used in the simulation
      experiment.}
    \item{\code{colnames}:}{Object of class \code{"character"} giving the names
      of the columns of \code{values} that contain the actual simulation
      results.}
    \item{\code{epsilon}:}{Object of class \code{"numeric"} containing the
      contamination levels used in the simulation experiment.}
    \item{\code{NArate}:}{Object of class \code{"NumericMatrix"} containing the
      missing value rates used in the simulation experiment.}
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
    \item{\code{seed}:}{Object of class \code{"list"} containing the seeds of
      the random number generator before and after the simulation experiment,
      respectively (for replication of the results).}
    \item{\code{call}:}{Object of class \code{"SimCall"}; the function call
       used to run the simulation experiment, or \code{NULL}.}
  }
}
\section{Accessor methods}{
  \describe{
    \item{\code{getValues}}{\code{signature(x = "SimResults")}: get slot
      \code{values}.}
    \item{\code{getAdd}}{\code{signature(x = "SimResults")}: get slot
      \code{add}.}
    \item{\code{getDesign}}{\code{signature(x = "SimResults")}: get slot
      \code{design}.}
    \item{\code{getColnames}}{\code{signature(x = "SimResults")}: get slot
      \code{colnames}.}
    \item{\code{getEpsilon}}{\code{signature(x = "SimResults")}: get slot
      \code{epsilon}.}
    \item{\code{getNArate}}{\code{signature(x = "SimResults")}: get slot
      \code{NArate}.}
    \item{\code{getDataControl}}{\code{signature(x = "SimResults")}: get slot
      \code{dataControl}.}
    \item{\code{getSampleControl}}{\code{signature(x = "SimResults")}: get slot
      \code{sampleControl}.}
    \item{\code{getNrep}}{\code{signature(x = "SimResults")}: get slot
      \code{nrep}.}
    \item{\code{getControl}}{\code{signature(x = "SimResults")}: get slot
      \code{control}.}
    \item{\code{getSeed}}{\code{signature(x = "SimResults")}: get slot
      \code{seed}.}
    \item{\code{getCall}}{\code{signature(x = "SimResults")}: get slot
      \code{call}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{aggregate}}{\code{signature(x = "SimResults")}: aggregate
      simulation results.}
    \item{\code{head}}{\code{signature(x = "SimResults")}: returns the first
      parts of simulation results.}
    \item{\code{plot}}{\code{signature(x = "SimResults", y = "missing")}:
      selects a suitable graphical representation of the simulation results
      automatically.}
    \item{\code{show}}{\code{signature(object = "SimResults")}: print
      simulation results on the \R console.}
    \item{\code{simBwplot}}{\code{signature(x = "SimResults")}: conditional
      box-and-whisker plot of simulation results.}
    \item{\code{simDensityplot}}{\code{signature(x = "SimResults")}:
      conditional kernel density plot of simulation results.}
    \item{\code{simXyplot}}{\code{signature(x = "SimResults")}: conditional x-y
      plot of simulation results.}
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
\note{
  There are no mutator methods available since the slots are not supposed to
  be changed by the user.

  Furthermore, the slots \code{dataControl}, \code{sampleControl}, \code{nrep}
  and \code{control} were added in version 0.3.
}
\author{Andreas Alfons}
\references{
Alfons, A., Templ, M. and Filzmoser, P. (2010) An Object-Oriented Framework for
Statistical Simulation: The \R Package \pkg{simFrame}. \emph{Journal of
Statistical Software}, \bold{37}(3), 1--36. \doi{10.18637/jss.v037.i03}.
}
\seealso{
  \code{\link{runSimulation}}, \code{\link{simBwplot}},
  \code{\link{simDensityplot}}, \code{\link{simXyplot}}
}
\examples{
showClass("SimResults")
}
\keyword{classes}
