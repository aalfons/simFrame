\name{SampleSetup-class}
\Rdversion{1.1}
\docType{class}
\alias{SampleSetup-class}
\alias{SampleSetup}
\alias{getIndices,SampleSetup-method}
\alias{getProb,SampleSetup-method}
%\alias{getDesign,SampleSetup-method}
%\alias{getGrouping,SampleSetup-method}
%\alias{getCollect,SampleSetup-method}
%\alias{getFun,SampleSetup-method}
\alias{getControl,SampleSetup-method}
\alias{getSeed,SampleSetup-method}
\alias{getCall,SampleSetup-method}
\alias{show,SampleSetup-method}
%% aliases to avoid confusion due to capitalization
\alias{sampleSetup-class}
\alias{samplesetup-class}
\alias{Samplesetup-class}
\alias{sampleSetup}
\alias{samplesetup}
\alias{Samplesetup}

\title{Class "SampleSetup"}
\description{Class for set up samples.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SampleSetup", \dots)} or
\code{SampleSetup(\dots)}.

However, objects are expected to be created by the function \code{\link{setup}}
or \code{\link{clusterSetup}}, these constructor functions are not supposed to
be called by the user.
}
\section{Slots}{
  \describe{
    \item{\code{indices}:}{Object of class \code{"list"}; each list element
      contains the indices of the sampled observations.}
    \item{\code{prob}:}{Object of class \code{"numeric"} giving the
      inclusion probabilities.}
%    \item{\code{design}:}{Object of class \code{"character"} giving the
%      variables (columns) defining the strata of the sample design.}
%    \item{\code{grouping}:}{Object of class \code{"character"} giving the
%      variable (column) used for sampling whole groups rather than
%      individual observations.}
%    \item{\code{collect}:}{Object of class \code{"logical"} indicating whether
%      groups were collected after sampling individuals or sampled directly.}
%    \item{\code{fun}:}{Object of class \code{"function"} used for sampling.}
    \item{\code{control}:}{Object of class \code{"VirtualSampleControl"}; the
      control object used to set up the samples.}
    \item{\code{seed}:}{Object of class \code{"list"} containing the seeds of
      the random number generator before and after setting up the samples,
      respectively (for replication purposes).}
    \item{\code{call}:}{Object of class \code{"SimCall"}; the function call
      used to set up the samples, or \code{NULL}.}
  }
}
\section{Accessor methods}{
  \describe{
    \item{\code{getIndices}}{\code{signature(x = "SampleSetup")}: get slot
      \code{indices}.}
    \item{\code{getProb}}{\code{signature(x = "SampleSetup")}: get slot
      \code{prob}.}
%    \item{\code{getDesign}}{\code{signature(x = "SampleSetup")}: get slot
%      \code{design}.}
%    \item{\code{getGrouping}}{\code{signature(x = "SampleSetup")}: get slot
%      \code{grouping}.}
%    \item{\code{getCollect}}{\code{signature(x = "SampleSetup")}: get slot
%      \code{collect}.}
%    \item{\code{getFun}}{\code{signature(x = "SampleSetup")}: get slot
%      \code{fun}.}
    \item{\code{getControl}}{\code{signature(x = "SampleSetup")}: get slot
      \code{control}.}
    \item{\code{getSeed}}{\code{signature(x = "SampleSetup")}: get slot
      \code{seed}.}
    \item{\code{getCall}}{\code{signature(x = "SampleSetup")}: get slot
      \code{call}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{clusterRunSimulation}}{\code{signature(cl = "ANY",
      x = "data.frame", setup = "SampleSetup", nrep = "missing",
      control = "SimControl")}: run a simulation experiment on a cluster.}
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
\note{
  There are no mutator methods available since the slots are not supposed to
  be changed by the user.

%  Furthermore, the slots \code{grouping} and \code{fun} were named \code{group}
%  and \code{method}, respectively, prior to version 0.2.  Renaming the slots
%  was necessary since accessor and mutator functions were introduced in this
%  version and functions named \code{getGroup}, \code{getMethod} and
%  \code{setMethod} already exist.

  Furthermore, the slot \code{seed} was added in version 0.2, and the slot
  \code{control} was added in version 0.3.  Since the control object used to
  set up the samples is now stored, the redundant slots \code{design},
  \code{grouping}, \code{collect} and \code{fun} were removed.  This has been
  done as preparation for additional control classes for sampling, which will
  be introduced in future versions.
}
\author{Andreas Alfons}
\references{
Alfons, A., Templ, M. and Filzmoser, P. (2010) An Object-Oriented Framework for
Statistical Simulation: The \R Package \pkg{simFrame}. \emph{Journal of
Statistical Software}, \bold{37}(3), 1--36. \doi{10.18637/jss.v037.i03}.
}
\seealso{
  \code{"\linkS4class{SampleControl}"}, \code{"\linkS4class{TwoStageControl}"},
  \code{"\linkS4class{VirtualSampleControl}"},
  \code{\link{setup}}, \code{\link{draw}}
}
\examples{
showClass("SampleSetup")
}
\keyword{classes}
