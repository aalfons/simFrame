\name{SampleControl-class}
\Rdversion{1.1}
\docType{class}
\alias{SampleControl-class}
\alias{SampleControl}
\alias{getDesign,SampleControl-method}
\alias{setDesign,SampleControl-method}
\alias{getGrouping,SampleControl-method}
\alias{setGrouping,SampleControl-method}
\alias{getCollect,SampleControl-method}
\alias{setCollect,SampleControl-method}
\alias{getFun,SampleControl-method}
\alias{setFun,SampleControl-method}
\alias{getSize,SampleControl-method}
\alias{setSize,SampleControl-method}
\alias{getProb,SampleControl-method}
\alias{setProb,SampleControl-method}
\alias{getDots,SampleControl-method}
\alias{setDots,SampleControl-method}
\alias{show,SampleControl-method}
%% aliases to avoid confusion due to capitalization
\alias{sampleControl-class}
\alias{samplecontrol-class}
\alias{Samplecontrol-class}
\alias{sampleControl}
\alias{samplecontrol}
\alias{Samplecontrol}

\title{Class "SampleControl"}
\description{Class for controlling the setup of samples.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SampleControl", \dots)}
or \code{SampleControl(\dots)}.
}
\section{Slots}{
  \describe{
    \item{\code{design}:}{Object of class \code{"BasicVector"} specifying
      variables (columns) to be used for stratified sampling.}
    \item{\code{grouping}:}{Object of class \code{"BasicVector"} specifying a
      grouping variable (column) to be used for sampling whole groups rather
      than individual observations.}
    \item{\code{collect}:}{Object of class \code{"logical"}; if  a grouping
      variable is specified and this is \code{FALSE} (which is the default
      value), groups are sampled directly.  If grouping variable is specified
      and this is \code{TRUE}, individuals are sampled in a first step.  In a
      second step, all individuals that belong to the same group as any of the
      sampled individuals are collected and added to the sample.  If no
      grouping variable is specified, this is ignored.}
    \item{\code{fun}:}{Object of class \code{"function"} to be used for
      sampling (defaults to \code{\link{srs}}).  It should return a vector
      containing the indices of the sampled items (observations or groups).}
    \item{\code{size}:}{Object of class \code{"OptNumeric"}; an optional
      non-negative integer giving the number of items (observations or groups)
      to sample.  In case of stratified sampling, a vector of non-negative
      integers, each giving the number of items to sample from the
      corresponding stratum, may be supplied.}
    \item{\code{prob}:}{Object of class \code{"OptBasicVector"}; an optional
      numeric vector giving the probability weights, or a character string or
      logical vector specifying a variable (column) that contains the
      probability weights.}
    \item{\code{dots}:}{Object of class \code{"list"} containing additional
      arguments to be passed to \code{fun}.}
    \item{\code{k}:}{Object of class \code{"numeric"}; a single positive
      integer giving the number of samples to be set up.}
  }
}
\section{Details}{
  There are some restrictions on the argument names of the function
  supplied to \code{fun}.  If it needs population data as input,
  the corresponding argument should be called \code{x} and should expect
  a \code{data.frame}.  If the sampling method only needs the population size
  as input, the argument should be called \code{N}.  Note that \code{fun} is
  not expected to have both \code{x} and \code{N} as arguments, and that the
  latter is much faster for stratified sampling or group sampling.
  Furthermore, if the function has arguments for sample size and probability
  weights, they should be called \code{size} and \code{prob}, respectively.
  Note that a function with \code{prob} as its only argument is perfectly valid
  (for probability proportional to size sampling).  Further arguments of
  \code{fun} may be supplied as a list via the slot \code{dots}.
}
\section{Extends}{
Class \code{"\linkS4class{VirtualSampleControl}"}, directly.
Class \code{"\linkS4class{OptSampleControl}"}, by class "VirtualSampleControl", distance 2.
}
\section{Accessor and mutator methods}{
  In addition to the accessor and mutator methods for the slots inherited from
  \code{"\linkS4class{VirtualSampleControl}"}, the following are available:

  \describe{
    \item{\code{getDesign}}{\code{signature(x = "SampleControl")}: get slot
      \code{design}.}
    \item{\code{setDesign}}{\code{signature(x = "SampleControl")}: set slot
      \code{design}.}
    \item{\code{getGrouping}}{\code{signature(x = "SampleControl")}: get slot
      \code{grouping}.}
    \item{\code{setGrouping}}{\code{signature(x = "SampleControl")}: set slot
      \code{grouping}.}
    \item{\code{getCollect}}{\code{signature(x = "SampleControl")}: get slot
      \code{collect}.}
    \item{\code{setCollect}}{\code{signature(x = "SampleControl")}: set slot
      \code{collect}.}
    \item{\code{getFun}}{\code{signature(x = "SampleControl")}: get slot
      \code{fun}.}
    \item{\code{setFun}}{\code{signature(x = "SampleControl")}: set slot
      \code{fun}.}
    \item{\code{getSize}}{\code{signature(x = "SampleControl")}: get slot
      \code{size}.}
    \item{\code{setSize}}{\code{signature(x = "SampleControl")}: set slot
      \code{size}.}
    \item{\code{getProb}}{\code{signature(x = "SampleControl")}: get slot
      \code{prob}.}
    \item{\code{setProb}}{\code{signature(x = "SampleControl")}: set slot
      \code{prob}.}
    \item{\code{getDots}}{\code{signature(x = "SampleControl")}: get slot
      \code{dots}.}
    \item{\code{setDots}}{\code{signature(x = "SampleControl")}: set slot
      \code{dots}.}
  }
}
\section{Methods}{
  In addition to the methods inherited from
  \code{"\linkS4class{VirtualSampleControl}"}, the following are available:

  \describe{
    \item{\code{clusterSetup}}{\code{signature(cl = "ANY", x = "data.frame",
      control = "SampleControl")}: set up multiple samples on a cluster.}
    \item{\code{setup}}{\code{signature(x = "data.frame",
      control = "SampleControl")}: set up multiple samples.}
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
\note{
  The slots \code{grouping} and \code{fun} were named \code{group} and
  \code{method}, respectively, prior to version 0.2.  Renaming the slots was
  necessary since accessor and mutator functions were introduced in this
  version and functions named \code{getGroup}, \code{getMethod} and
  \code{setMethod} already exist.
}
\author{Andreas Alfons}
\references{
Alfons, A., Templ, M. and Filzmoser, P. (2010) An Object-Oriented Framework for
Statistical Simulation: The \R Package \pkg{simFrame}. \emph{Journal of
Statistical Software}, \bold{37}(3), 1--36. \doi{10.18637/jss.v037.i03}.
}
\seealso{
  \code{"\linkS4class{VirtualSampleControl}"},
  \code{"\linkS4class{TwoStageControl}"}, \code{"\linkS4class{SampleSetup}"},
  \code{\link{setup}}, \code{\link{draw}}
}
\examples{
data(eusilcP)

## simple random sampling
srsc <- SampleControl(size = 20)
draw(eusilcP[, c("id", "eqIncome")], srsc)

## group sampling
gsc <- SampleControl(grouping = "hid", size = 10)
draw(eusilcP[, c("hid", "hid", "eqIncome")], gsc)

## stratified simple random sampling
ssrsc <- SampleControl(design = "region",
    size = c(2, 5, 5, 3, 4, 5, 3, 5, 2))
draw(eusilcP[, c("id", "region", "eqIncome")], ssrsc)

## stratified group sampling
sgsc <- SampleControl(design = "region", grouping = "hid",
    size = c(2, 5, 5, 3, 4, 5, 3, 5, 2))
draw(eusilcP[, c("hid", "id", "region", "eqIncome")], sgsc)
}
\keyword{classes}
