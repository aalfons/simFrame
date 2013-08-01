\name{TwoStageSampleControl-class}
\Rdversion{1.1}
\docType{class}
\alias{TwoStageSampleControl-class}
\alias{TwoStageSampleControl}
\alias{getDesign,TwoStageSampleControl-method}
\alias{getDots,TwoStageSampleControl-method}
\alias{getFun,TwoStageSampleControl-method}
\alias{getGrouping,TwoStageSampleControl-method}
\alias{getProb,TwoStageSampleControl-method}
\alias{getSize,TwoStageSampleControl-method}
\alias{setup,data.frame,TwoStageSampleControl-method}
\alias{show,TwoStageSampleControl-method}

\title{Class \code{"TwoStageSampleControl"}}
\description{
  Class for controlling the setup of samples using a two-stage procedure.
}
\section{Objects from the Class}{
Objects can be created via the constructor \code{TwoStageControl} or by calls 
of the form \code{SampleControl(\dots, type = "Basic")}.
}
\section{Slots}{
  \describe{
    \item{\code{design}:}{Object of class \code{"BasicVector"} specifying 
      variables (columns) to be used for stratified sampling in the first 
      stage.}
    \item{\code{grouping}:}{Object of class \code{"BasicVector"} specifying 
      grouping variables (columns) to be used for sampling primary sampling 
      units (PSUs) and secondary sampling units (SSUs), respectively.}
    \item{\code{fun}:}{Object of class \code{"list"}; a list of length two 
      containing the functions to be used for sampling in the first and second 
      stage, respectively (defaults to \code{\link{srs}} for both stages).  The 
      functions should return a vector containing the indices of the sampled 
      items.}
    \item{\code{size}:}{Object of class \code{"list"}; a list of length two, 
      where each component contains an optional non-negative integer giving the 
      number of items to sample in the first and second stage, respectively.  
      In case of stratified sampling in the first stage, a vector of 
      non-negative integers, each giving the number of PSUs to sample from the 
      corresponding stratum, may be supplied.  For the second stage, a vector 
      of non-negative integers giving the number of items to sample from each 
      PSU may be used.}
    \item{\code{prob}:}{Object of class \code{"list"}; a list of length two, 
      with components optionally specifying variables (columns) that contain 
      probability weights for the first and second stage, respectively.}
    \item{\code{dots}:}{Object of class \code{"list"}; a list of length two, 
      where each component is again a list containing additional arguments to 
      be passed to the corresponding function for sampling in \code{fun}.}
    \item{\code{k}:}{Object of class \code{"numeric"}; a single positive 
      integer giving the number of samples to be set up.}
    \item{\code{seed}:}{Object of class \code{"numeric"} to be used as initial 
      seed of the random number generator.  If not supplied explicitly, the 
      seed will be based on the date and time of constrution.}
  }
}
\usage{
TwoStageSampleControl(\dots, fun1 = srs, fun2 = srs, 
                      size1 = NULL, size2 = NULL, 
                      prob1 = character(), prob2 = character(), 
                      dots1 = list(), dots2 = list())
}
\arguments{
  \item{\dots}{the slots for the new object (see below).}
  \item{fun1}{the function to be used for sampling in the first stage (the 
    first list component of slot \code{fun}).}
  \item{fun2}{the function to be used for sampling in the second stage (the 
    second list component of slot \code{fun}).}
  \item{size1}{the number of PSUs to sample in the first stage (the first list 
    component of slot \code{size}).}
  \item{size2}{the number of items to sample in the second stage (the second 
    list component of slot \code{size}).}
  \item{prob1}{the probability weights for the first stage (the first list 
    component of slot \code{prob}).}
  \item{prob2}{the probability weights for the second stage (the second list 
    component of slot \code{prob}).}
  \item{dots1}{additional arguments to be passed to the function for sampling 
    in the first stage (the first list component of slot \code{dots}).}
  \item{dots2}{additional arguments to be passed to the function for sampling 
    in the second stage (the second list component of slot \code{dots}).}
}
\section{Details}{
  There are some restrictions on the argument names of the functions for 
  sampling in \code{fun}.  If the sampling method needs population data as 
  input, the corresponding argument should be called \code{x} and should expect 
  a \code{data.frame}.  If it only needs the population size as input, the 
  argument should be called \code{N}.  Note that the function is not expected 
  to have both \code{x} and \code{N} as arguments, and that the latter is 
  typically much faster.  Furthermore, if the function has arguments for sample 
  size and probability weights, they should be called \code{size} and 
  \code{prob}, respectively.  Note that a function with \code{prob} as its only 
  argument is perfectly valid (for probability proportional to size sampling).  
  Further arguments may be supplied as a list via the slot \code{dots}.
}
\section{Extends}{
Class \code{"\linkS4class{VirtualSampleControl}"}, directly.
Class \code{"\linkS4class{OptSampleControl}"}, distance 2.
}
\section{Accessor methods}{
  The following accessor methods are available in addition to the inherited 
  ones:
  
  \describe{
    \item{\code{getDesign}}{\code{signature(x = "TwoStageControl")}: get slot 
      \code{design}.}
    \item{\code{getGrouping}}{\code{signature(x = "TwoStageControl")}: get slot 
      \code{grouping}.}
    \item{\code{getCollect}}{\code{signature(x = "TwoStageControl")}: get slot 
      \code{collect}.}
    \item{\code{getFun}}{\code{signature(x = "TwoStageControl")}: get slot 
      \code{fun}.}
    \item{\code{getSize}}{\code{signature(x = "TwoStageControl")}: get slot 
      \code{size}.}
    \item{\code{getProb}}{\code{signature(x = "TwoStageControl")}: get slot 
      \code{prob}.}
    \item{\code{getDots}}{\code{signature(x = "TwoStageControl")}: get slot 
      \code{dots}.}
  }
}
\section{Methods}{
  The following methods are available in addition to the inherited ones:
  
  \describe{
    \item{setup}{\code{signature(x = "data.frame", 
      control = "TwoStageSampleControl")}: set up multiple samples.}
    \item{\code{show}}{\code{signature(object = "TwoStageSampleControl")}: 
      print the object on the \R console.}
  }
}
\section{UML class diagram}{
  A slightly simplified UML class diagram of the framework can be found in 
  Figure 1 of the package vignette \emph{An Object-Oriented Framework for 
  Statistical Simulation: The \R Package \code{simFrame}}.  Use 
  \code{vignette("simFrame-intro")} to view this vignette.
}
\author{Andreas Alfons}
\seealso{
  \code{"\linkS4class{BasicSampleControl}"}, 
  \code{"\linkS4class{SampleSetup}"}, 
  \code{\link{setup}}, \code{\link{draw}}
}
% TODO: examples for two-stage sampling
\keyword{classes}
