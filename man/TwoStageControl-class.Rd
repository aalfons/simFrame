\name{TwoStageControl-class}
\Rdversion{1.1}
\docType{class}
\alias{TwoStageControl-class}
\alias{TwoStageControl}
\alias{getDesign,TwoStageControl-method}
\alias{setDesign,TwoStageControl-method}
\alias{getGrouping,TwoStageControl-method}
\alias{setGrouping,TwoStageControl-method}
\alias{show,TwoStageControl-method}
%% aliases to avoid confusion due to capitalization
\alias{twoStageControl-class}
\alias{twostageControl-class}
\alias{twoStagecontrol-class}
\alias{twostagecontrol-class}
\alias{TwostageControl-class}
\alias{Twostagecontrol-class}
\alias{TwoStagecontrol-class}
\alias{twoStageControl}
\alias{twostageControl}
\alias{twoStagecontrol}
\alias{twostagecontrol}
\alias{TwostageControl}
\alias{Twostagecontrol}
\alias{TwoStagecontrol}

\title{Class "TwoStageControl"}
\description{
  Class for controlling the setup of samples using a two-stage procedure.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("TwoStageControl", ...)} 
or via the constructor \code{TwoStageControl}.
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
      where each component gives optional probability weights for the first and 
      second stage, respectively.  Each component may thereby be a numerical 
      vector, or a character string or integer vector specifying a variable 
      (column) that contains the probability weights.}
    \item{\code{dots}:}{Object of class \code{"list"}; a list of length two, 
      where each component is again a list containing additional arguments to 
      be passed to the corresponding function for sampling in \code{fun}.}
    \item{\code{k}:}{Object of class \code{"numeric"}; a single positive 
      integer giving the number of samples to be set up.}
  }
}
\usage{
TwoStageControl(\dots, fun1 = srs, fun2 = srs, size1 = NULL, 
                size2 = NULL, prob1 = NULL, prob2 = NULL, 
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
Class \code{"\linkS4class{OptSampleControl}"}, by class "VirtualSampleControl", distance 2.
}
\section{Accessor and mutator methods}{
  In addition to the accessor and mutator methods for the slots inherited from 
  \code{"\linkS4class{VirtualSampleControl}"}, the following are available:
  
  \describe{
    \item{\code{getDesign}}{\code{signature(x = "TwoStageControl")}: get slot 
      \code{design}.}
    \item{\code{setDesign}}{\code{signature(x = "TwoStageControl")}: set slot 
      \code{design}.}
    \item{\code{getGrouping}}{\code{signature(x = "TwoStageControl")}: get slot 
      \code{grouping}.}
    \item{\code{setGrouping}}{\code{signature(x = "TwoStageControl")}: set slot 
      \code{grouping}.}
    \item{\code{getCollect}}{\code{signature(x = "TwoStageControl")}: get slot 
      \code{collect}.}
    \item{\code{setCollect}}{\code{signature(x = "TwoStageControl")}: set slot 
      \code{collect}.}
    \item{\code{getFun}}{\code{signature(x = "TwoStageControl")}: get slot 
      \code{fun}.}
    \item{\code{setFun}}{\code{signature(x = "TwoStageControl")}: set slot 
      \code{fun}.}
    \item{\code{getSize}}{\code{signature(x = "TwoStageControl")}: get slot 
      \code{size}.}
    \item{\code{setSize}}{\code{signature(x = "TwoStageControl")}: set slot 
      \code{size}.}
    \item{\code{getProb}}{\code{signature(x = "TwoStageControl")}: get slot 
      \code{prob}.}
    \item{\code{setProb}}{\code{signature(x = "TwoStageControl")}: set slot 
      \code{prob}.}
    \item{\code{getDots}}{\code{signature(x = "TwoStageControl")}: get slot 
      \code{dots}.}
    \item{\code{setDots}}{\code{signature(x = "TwoStageControl")}: set slot 
      \code{dots}.}
  }
}
\section{Methods}{
  In addition to the methods inherited from 
  \code{"\linkS4class{VirtualSampleControl}"}, the following are available:
  
  \describe{
    \item{\code{clusterSetup}}{\code{signature(cl = "ANY", x = "data.frame", 
      control = "TwoStageControl")}: set up multiple samples on a cluster.}
    \item{\code{setup}}{\code{signature(x = "data.frame", 
      control = "TwoStageControl")}: set up multiple samples.}
    \item{\code{show}}{\code{signature(object = "TwoStageControl")}: print the 
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
\seealso{
  \code{"\linkS4class{VirtualSampleControl}"}, 
  \code{"\linkS4class{SampleControl}"}, \code{"\linkS4class{SampleSetup}"}, 
  \code{\link{setup}}, \code{\link{draw}}
}
\examples{
% TODO: examples for two-stage sampling
showClass("TwoStageControl")
}
\keyword{classes}
