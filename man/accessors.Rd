\name{accessors}
\docType{methods}
\alias{getAux}
\alias{getAux-methods}
\alias{getCall}
\alias{getCall-methods}
\alias{getCollect}
\alias{getCollect-methods}
\alias{getColnames}
\alias{getColnames-methods}
\alias{getControl}
\alias{getControl-methods}
\alias{getControl,SimControl-method}
\alias{getControl,SimResults-method}
\alias{getDesign}
\alias{getDesign-methods}
\alias{getDots}
\alias{getDots-methods}
\alias{getDots,TwoStageSampleControl-method}
\alias{getEpsilon}
\alias{getEpsilon-methods}
\alias{getFun}
\alias{getFun-methods}
\alias{getFun,TwoStageSampleControl-method}
\alias{getGrouping}
\alias{getGrouping-methods}
\alias{getIndices}
\alias{getIndices-methods}
\alias{getInfo}
\alias{getInfo-methods}
\alias{getIntoContamination}
\alias{getIntoContamination-methods}
\alias{getK}
\alias{getK-methods}
\alias{getNARate}
\alias{getNARate-methods}
\alias{getNrep}
\alias{getNrep-methods}
\alias{getProb}
\alias{getProb-methods}
\alias{getProb,TwoStageSampleControl-method}
\alias{getSeed}
\alias{getSeed-methods}
\alias{getSize}
\alias{getSize-methods}
\alias{getSize,TwoStageSampleControl-method}
\alias{getTarget}
\alias{getTarget-methods}
\alias{getTuning}
\alias{getTuning-methods}
\alias{getType}
\alias{getType-methods}
\alias{getValues}
\alias{getValues-methods}

\title{Accessor functions for objects}
\description{
  Extract information from objects via accessor functions.  Mutator functions 
  are not available since objects are not supposed to be manipulated directly 
  by the user.
}
\usage{
getAux(x)

getCall(x, \dots)

getCollect(x)

getColnames(x)

getControl(x, \dots)
\S4method{getControl}{SimControl}(x, which = c("cont", "NA"))
\S4method{getControl}{SimResults}(x, which = c("sim", "data", "sample"))

getDesign(x)

getDots(x, \dots)
\S4method{getDots}{TwoStageSampleControl}(x, stage = NULL)

getEpsilon(x)

getFun(x, \dots)
\S4method{getFun}{TwoStageSampleControl}(x, stage = NULL)

getGrouping(x)

getIndices(x)

getInfo(x)

getIntoContamination(x)

getK(x)

getNARate(x)

getNrep(x)

getProb(x, \dots)
\S4method{getProb}{TwoStageSampleControl}(x, stage = NULL)

getSeed(x)

getSize(x, \dots)
\S4method{getSize}{TwoStageSampleControl}(x, stage = NULL)

getTarget(x)

getTuning(x)

getType(x)

getValues(x)
}
\arguments{
  \item{x}{an object.}
  \item{stage}{optional integer; for certain slots of 
    \code{"\linkS4class{TwoStageSampleControl}"}, this allows to access or 
    modify only the list component for the specified stage.  Use \code{1} for 
    the first stage and \code{2} for the second stage.}
  \item{which}{a character string specifying which control object to extract.  
    For the \code{"\linkS4class{SimControl}"} method, possible values are 
    \code{"cont"} (the default) and \code{"NA"} for the control objects used 
    for contamination inserting missing values, respectively.  For the 
    \code{"\linkS4class{SimResults}"} method, possible values are \code{"sim"} 
    (the default), \code{"data"} and \code{"sample"} for the control objects 
    used for running the simulations, data generation and sampling, 
    respectively.}
  \item{\dots}{additional arguments to be passed down to methods.}
}
\value{
  The requested information, typically a slot of \code{x}.
}
\section{Methods for function \code{getAux}}{
  \describe{
  \item{\code{signature(x = "RandomContControl")}}{}
  \item{\code{signature(x = "NAControl")}}{}
  }
}
\section{Methods for function \code{getCall}}{
  \describe{
  \item{\code{signature(x = "SampleSetup")}}{}
  \item{\code{signature(x = "SimResults")}}{}
  }
}
\section{Methods for function \code{getCollect}}{
  \describe{
  \item{\code{signature(x = "BasicSampleControl")}}{}
  }
}
\section{Methods for function \code{getColnames}}{
  \describe{
  \item{\code{signature(x = "DataControl")}}{}
  \item{\code{signature(x = "SimResults")}}{}
  }
}
\section{Methods for function \code{getControl}}{
  \describe{
  \item{\code{signature(x = "SampleSetup")}}{}
  \item{\code{signature(x = "SimControl")}}{}
  \item{\code{signature(x = "SimResults")}}{}
  }
}
\section{Methods for function \code{getDesign}}{
  \describe{
  \item{\code{signature(x = "SampleControl")}}{}
  \item{\code{signature(x = "SimControl")}}{}
  }
}
\section{Methods for function \code{getDots}}{
  \describe{
  \item{\code{signature(x = "DataControl")}}{}
  \item{\code{signature(x = "BasicSampleControl")}}{}
  \item{\code{signature(x = "TwoStageSampleControl")}}{}
  \item{\code{signature(x = "ContControl")}}{}
  \item{\code{signature(x = "SimControl")}}{}
  }
}
\section{Methods for function \code{getEpsilon}}{
  \describe{
  \item{\code{signature(x = "VirtualContControl")}}{}
  }
}
\section{Methods for function \code{getFun}}{
  \describe{
  \item{\code{signature(x = "DataControl")}}{}
  \item{\code{signature(x = "BasicSampleControl")}}{}
  \item{\code{signature(x = "TwoStageSampleControl")}}{}
  \item{\code{signature(x = "BasicContControl")}}{}
  \item{\code{signature(x = "ContControl")}}{}
  \item{\code{signature(x = "SimControl")}}{}
  }
}
\section{Methods for function \code{getGrouping}}{
  \describe{
  \item{\code{signature(x = "SampleControl")}}{}
  \item{\code{signature(x = "RandomContControl")}}{}
  \item{\code{signature(x = "NAControl")}}{}
  }
}
\section{Methods for function \code{getIndices}}{
  \describe{
  \item{\code{signature(x = "DataControl")}}{}
  \item{\code{signature(x = "SampleSetup")}}{}
  \item{\code{signature(x = "ContControl")}}{}
  }
}
\section{Methods for function \code{getInfo}}{
  \describe{
  \item{\code{signature(x = "SimResults")}}{}
  }
}
\section{Methods for function \code{getIntoContamination}}{
  \describe{
  \item{\code{signature(x = "NAControl")}}{}
  }
}
\section{Methods for function \code{getK}}{
  \describe{
  \item{\code{signature(x = "VirtualSampleControl")}}{}
  }
}
\section{Methods for function \code{getNARate}}{
  \describe{
  \item{\code{signature(x = "VirtualNAControl")}}{}
  }
}
\section{Methods for function \code{getNrep}}{
  \describe{
  \item{\code{signature(x = "SimResults")}}{}
  }
}
\section{Methods for function \code{getProb}}{
  \describe{
  \item{\code{signature(x = "BasicSampleControl")}}{}
  \item{\code{signature(x = "TwoStageSampleControl")}}{}
  \item{\code{signature(x = "SampleSetup")}}{}
  }
}
\section{Methods for function \code{getSeed}}{
  \describe{
  \item{\code{signature(x = "VirtualSampleControl")}}{}
  \item{\code{signature(x = "SimControl")}}{}
  }
}
\section{Methods for function \code{getSize}}{
  \describe{
  \item{\code{signature(x = "VirtualDataControl")}}{}
  \item{\code{signature(x = "BasicSampleControl")}}{}
  \item{\code{signature(x = "TwoStageSampleControl")}}{}
  \item{\code{signature(x = "SummarySampleSetup")}}{}
  }
}
\section{Methods for function \code{getTarget}}{
  \describe{
  \item{\code{signature(x = "VirtualContControl")}}{}
  \item{\code{signature(x = "VirtualNAControl")}}{}
  }
}
\section{Methods for function \code{getTuning}}{
  \describe{
  \item{\code{signature(x = "DataControl")}}{}
  \item{\code{signature(x = "ContControl")}}{}
  }
}
\section{Methods for function \code{getType}}{
  \describe{
  \item{\code{signature(x = "ContControl")}}{}
  }
}
\section{Methods for function \code{getValues}}{
  \describe{
  \item{\code{signature(x = "SimResults")}}{}
  }
}
\author{Andreas Alfons}
\references{
Alfons, A., Templ, M. and Filzmoser, P. (2010) An Object-Oriented Framework for 
Statistical Simulation: The \R Package \pkg{simFrame}. \emph{Journal of 
Statistical Software}, \bold{37}(3), 1--36. URL 
\url{http://www.jstatsoft.org/v37/i03/}.
}
\examples{
nc <- NAControl(NARate = 0.05)
getNARate(nc)

nc <- NAControl(NARate = c(0.01, 0.03, 0.05, 0.07, 0.09))
getNARate(nc)
}
\keyword{classes}
\keyword{methods}
