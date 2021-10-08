\name{accessors}
\Rdversion{1.1}
\docType{methods}
\alias{getAdd}
\alias{getAdd-methods}
\alias{getAux}
\alias{getAux-methods}
\alias{setAux}
\alias{setAux-methods}
\alias{getCall}
\alias{getCall-methods}
\alias{getCollect}
\alias{getCollect-methods}
\alias{setCollect}
\alias{setCollect-methods}
\alias{getColnames}
\alias{getColnames-methods}
\alias{setColnames}
\alias{setColnames-methods}
\alias{getContControl}
\alias{getContControl-methods}
\alias{setContControl}
\alias{setContControl-methods}
\alias{getControl}
\alias{getControl-methods}
\alias{getDataControl}
\alias{getDataControl-methods}
\alias{getDesign}
\alias{getDesign-methods}
\alias{setDesign}
\alias{setDesign-methods}
\alias{getDistribution}
\alias{getDistribution-methods}
\alias{setDistribution}
\alias{setDistribution-methods}
\alias{getDots}
\alias{getDots-methods}
\alias{getDots,TwoStageControl-method}
\alias{setDots}
\alias{setDots-methods}
\alias{setDots,TwoStageControl-method}
\alias{getEpsilon}
\alias{getEpsilon-methods}
\alias{setEpsilon}
\alias{setEpsilon-methods}
\alias{getFun}
\alias{getFun-methods}
\alias{getFun,TwoStageControl-method}
\alias{setFun}
\alias{setFun-methods}
\alias{setFun,TwoStageControl-method}
\alias{getGrouping}
\alias{getGrouping-methods}
\alias{setGrouping}
\alias{setGrouping-methods}
\alias{getIndices}
\alias{getIndices-methods}
\alias{getIntoContamination}
\alias{getIntoContamination-methods}
\alias{setIntoContamination}
\alias{setIntoContamination-methods}
\alias{getK}
\alias{getK-methods}
\alias{setK}
\alias{setK-methods}
\alias{getLegend}
\alias{getLegend-methods}
\alias{getNAControl}
\alias{getNAControl-methods}
\alias{setNAControl}
\alias{setNAControl-methods}
\alias{getNArate}
\alias{getNArate-methods}
\alias{setNArate}
\alias{setNArate-methods}
\alias{getNr}
\alias{getNr-methods}
\alias{getNrep}
\alias{getNrep-methods}
\alias{getProb}
\alias{getProb-methods}
\alias{getProb,TwoStageControl-method}
\alias{setProb}
\alias{setProb-methods}
\alias{setProb,TwoStageControl-method}
\alias{getSAE}
\alias{getSAE-methods}
\alias{setSAE}
\alias{setSAE-methods}
\alias{getSampleControl}
\alias{getSampleControl-methods}
\alias{getSeed}
\alias{getSeed-methods}
\alias{getSize}
\alias{getSize-methods}
\alias{getSize,TwoStageControl-method}
\alias{setSize}
\alias{setSize-methods}
\alias{setSize,TwoStageControl-method}
\alias{getSplit}
\alias{getSplit-methods}
\alias{getTarget}
\alias{getTarget-methods}
\alias{setTarget}
\alias{setTarget-methods}
\alias{getValues}
\alias{getValues-methods}

\title{Accessor and mutator functions for objects}
\description{
  Get values of slots of objects via accessor functions and set values via 
  mutator functions.  If no mutator methods are available, the slots of the 
  corresponding objects are not supposed to be changed by the user.
}
\usage{
getAdd(x)

getAux(x)
setAux(x, aux)

getCall(x, \dots)

getCollect(x)
setCollect(x, collect)

getColnames(x)
setColnames(x, colnames)

getContControl(x)
setContControl(x, contControl)

getControl(x)

getDataControl(x)

getDesign(x)
setDesign(x, design)

getDistribution(x)
setDistribution(x, distribution)

getDots(x, \dots)
setDots(x, dots, \dots)

\S4method{getDots}{TwoStageControl}(x, stage = NULL)
\S4method{setDots}{TwoStageControl}(x, dots, stage = NULL)

getEpsilon(x)
setEpsilon(x, epsilon)

getFun(x, \dots)
setFun(x, fun, \dots)

\S4method{getFun}{TwoStageControl}(x, stage = NULL)
\S4method{setFun}{TwoStageControl}(x, fun, stage = NULL)

getGrouping(x)
setGrouping(x, grouping)

getIndices(x)

getIntoContamination(x)
setIntoContamination(x, intoContamination)

getK(x)
setK(x, k)

getLegend(x)

getNAControl(x)
setNAControl(x, NAControl)

getNArate(x)
setNArate(x, NArate)

getNr(x)

getNrep(x)

getProb(x, \dots)
setProb(x, prob, \dots)

\S4method{getProb}{TwoStageControl}(x, stage = NULL)
\S4method{setProb}{TwoStageControl}(x, prob, stage = NULL)

getSAE(x)
setSAE(x, SAE)

getSampleControl(x)

getSeed(x)

getSize(x, \dots)
setSize(x, size, \dots)

\S4method{getSize}{TwoStageControl}(x, stage = NULL)
\S4method{setSize}{TwoStageControl}(x, size, stage = NULL)

getSplit(x)

getTarget(x)
setTarget(x, target)

getValues(x)
}
\arguments{
  \item{x}{an object.}
  \item{aux}{a character string specifying an auxiliary variable (see 
    \code{"\linkS4class{ContControl}"} and \code{"\linkS4class{NAControl}"}).}
  \item{collect}{a logical indicating whether groups should be collected after 
    sampling individuals or sampled directly (see 
    \code{"\linkS4class{SampleControl}"}).}
  \item{colnames}{a character vector specifying column names (see 
    \code{"\linkS4class{DataControl}"}).}
  \item{contControl}{an object of class \code{"\linkS4class{ContControl}"} (see 
    \code{"\linkS4class{SimControl}"}).}
  \item{design}{a character vector specifying columns to be used for 
    stratification (see \code{"\linkS4class{SampleControl}"}, 
    \code{"\linkS4class{TwoStageControl}"} and 
    \code{"\linkS4class{SimControl}"}).}
  \item{distribution}{a function generating data (see 
    \code{"\linkS4class{DataControl}"} and 
    \code{"\linkS4class{DCARContControl}"}).}
  \item{dots}{additional arguments to be passed to a function (see 
    \code{"\linkS4class{DataControl}"}, \code{"\linkS4class{DARContControl}"}, 
    \code{"\linkS4class{DCARContControl}"}, 
    \code{"\linkS4class{SampleControl}"}, 
    \code{"\linkS4class{TwoStageControl}"} and 
    \code{"\linkS4class{SimControl}"}).}
  \item{epsilon}{a numeric vector giving contamination levels (see 
    \code{"\linkS4class{VirtualContControl}"}).}
  \item{fun}{a function (see 
    \code{"\linkS4class{DARContControl}"}, 
    \code{"\linkS4class{SampleControl}"}, 
    \code{"\linkS4class{TwoStageControl}"} and 
    \code{"\linkS4class{SimControl}"}).}
  \item{grouping}{a character string specifying a grouping variable (see 
    \code{"\linkS4class{ContControl}"}, \code{"\linkS4class{NAControl}"}, 
     \code{"\linkS4class{SampleControl}"} and 
     \code{"\linkS4class{TwoStageControl}"}).}
  \item{intoContamination}{a logical indicating whether missing values should 
    also be inserted into contaminated observations (see 
    \code{"\linkS4class{NAControl}"}).}
  \item{k}{a single positive integer giving the number of samples to be set up 
    (see \code{"\linkS4class{VirtualSampleControl}"}).}
  \item{NAControl}{an object of class \code{"\linkS4class{NAControl}"} (see 
    \code{"\linkS4class{SimControl}"}).}
  \item{NArate}{a numeric vector or matrix giving missing value rates (see 
    \code{"\linkS4class{VirtualNAControl}"}).}
  \item{prob}{a numeric vector giving probability weights (see 
    \code{"\linkS4class{SampleControl}"} and 
    \code{"\linkS4class{TwoStageControl}"}).}
  \item{SAE}{a logical indicating whether small area estimation will be used in 
    the simulation experiment (see \code{"\linkS4class{SimControl}"}).}
  \item{size}{a non-negative integer or a vector of non-negative integers (see 
    \code{"\linkS4class{DataControl}"}, \code{"\linkS4class{SampleControl}"} 
    and \code{"\linkS4class{TwoStageControl}"}).}
  \item{stage}{optional integer; for certain slots of 
    \code{"\linkS4class{TwoStageControl}"}, this allows to access or modify 
    only the list component for the specified stage.  Use \code{1} for the 
    first stage and \code{2} for the second stage.}
  \item{target}{a character vector specifying target columns (see 
    \code{"\linkS4class{VirtualContControl}"} and 
    \code{"\linkS4class{VirtualNAControl}"}).}
  \item{\dots}{only used to allow for the \code{stage} argument in accessor and 
    mutator methods for \code{"\linkS4class{TwoStageControl}"}. Otherwise no 
    additional arguments are available.}
}
\value{
  For accessor functions, the corresponding slot of \code{x} is returned.
  
  For mutator functions, the corresponding slot of \code{x} is replaced.
}
\section{Methods for function getAdd}{
  \describe{
  \item{\code{signature(x = "SimResults")}}{}
  }
}
\section{Methods for functions getAux and setAux}{
  \describe{
  \item{\code{signature(x = "ContControl")}}{}
  \item{\code{signature(x = "NAControl")}}{}
  }
}
\section{Methods for function getCall}{
  \describe{
  \item{\code{signature(x = "SampleSetup")}}{}
  \item{\code{signature(x = "SimResults")}}{}
  \item{\code{signature(x = "Strata")}}{}
  }
}
%\section{Methods for function getCollect}{
%  \describe{
%  \item{\code{signature(x = "SampleControl")}}{}
%  \item{\code{signature(x = "SampleSetup")}}{}
%  }
%}
%\section{Methods for function setCollect}{
%  \describe{
%  \item{\code{signature(x = "SampleControl")}}{}
%  }
%}
\section{Methods for functions getCollect and setCollect}{
  \describe{
  \item{\code{signature(x = "SampleControl")}}{}
  }
}
\section{Methods for function getColnames}{
  \describe{
  \item{\code{signature(x = "DataControl")}}{}
  \item{\code{signature(x = "SimResults")}}{}
  }
}
\section{Methods for function setColnames}{
  \describe{
  \item{\code{signature(x = "DataControl")}}{}
  }
}
\section{Methods for functions getContControl and setContControl}{
  \describe{
  \item{\code{signature(x = "SimControl")}}{}
  }
}
\section{Methods for function getControl}{
  \describe{
  \item{\code{signature(x = "SampleSetup")}}{}
  \item{\code{signature(x = "SimResults")}}{}
  }
}
\section{Methods for function getDataControl}{
  \describe{
  \item{\code{signature(x = "SimResults")}}{}
  }
}
\section{Methods for function getDesign}{
  \describe{
  \item{\code{signature(x = "SampleControl")}}{}
  \item{\code{signature(x = "TwoStageControl")}}{}
  \item{\code{signature(x = "SimControl")}}{}
  \item{\code{signature(x = "SimResults")}}{}
  \item{\code{signature(x = "Strata")}}{}
  }
}
\section{Methods for function setDesign}{
  \describe{
  \item{\code{signature(x = "SampleControl")}}{}
  \item{\code{signature(x = "TwoStageControl")}}{}
  \item{\code{signature(x = "SimControl")}}{}
  }
}
\section{Methods for functions getDistribution and setDistribution}{
  \describe{
  \item{\code{signature(x = "DataControl")}}{}
  \item{\code{signature(x = "DCARContControl")}}{}
  }
}
\section{Methods for functions getDots and setDots}{
  \describe{
  \item{\code{signature(x = "DataControl")}}{}
  \item{\code{signature(x = "DARContControl")}}{}
  \item{\code{signature(x = "DCARContControl")}}{}
  \item{\code{signature(x = "SampleControl")}}{}
  \item{\code{signature(x = "TwoStageControl")}}{}
  \item{\code{signature(x = "SimControl")}}{}
  }
}
\section{Methods for function getEpsilon}{
  \describe{
  \item{\code{signature(x = "SimResults")}}{}
  \item{\code{signature(x = "VirtualContControl")}}{}
  }
}
\section{Methods for function setEpsilon}{
  \describe{
  \item{\code{signature(x = "VirtualContControl")}}{}
  }
}
%\section{Methods for function getFun}{
%  \describe{
%  \item{\code{signature(x = "DARContControl")}}{}
%  \item{\code{signature(x = "SampleControl")}}{}
%  \item{\code{signature(x = "SampleSetup")}}{}
%  \item{\code{signature(x = "SimControl")}}{}
%  }
%}
%\section{Methods for function setFun}{
%  \describe{
%  \item{\code{signature(x = "DARContControl")}}{}
%  \item{\code{signature(x = "SampleControl")}}{}
%  \item{\code{signature(x = "SimControl")}}{}
%  }
%}
\section{Methods for functions getFun and setFun}{
  \describe{
  \item{\code{signature(x = "DARContControl")}}{}
  \item{\code{signature(x = "SampleControl")}}{}
  \item{\code{signature(x = "TwoStageControl")}}{}
  \item{\code{signature(x = "SimControl")}}{}
  }
}
%\section{Methods for function getGrouping}{
%  \describe{
%  \item{\code{signature(x = "ContControl")}}{}
%  \item{\code{signature(x = "NAControl")}}{}
%  \item{\code{signature(x = "SampleControl")}}{}
%  \item{\code{signature(x = "SampleSetup")}}{}
%  }
%}
%\section{Methods for function setGrouping}{
%  \describe{
%  \item{\code{signature(x = "ContControl")}}{}
%  \item{\code{signature(x = "NAControl")}}{}
%  \item{\code{signature(x = "SampleControl")}}{}
%  }
%}
\section{Methods for functions getGrouping and setGrouping}{
  \describe{
  \item{\code{signature(x = "ContControl")}}{}
  \item{\code{signature(x = "NAControl")}}{}
  \item{\code{signature(x = "SampleControl")}}{}
  \item{\code{signature(x = "TwoStageControl")}}{}
  }
}
\section{Methods for function getIndices}{
  \describe{
  \item{\code{signature(x = "SampleSetup")}}{}
  }
}
\section{Methods for functions getIntoContamination and setIntoContamination}{
  \describe{
  \item{\code{signature(x = "NAControl")}}{}
  }
}
\section{Methods for functions getK and setK}{
  \describe{
  \item{\code{signature(x = "VirtualSampleControl")}}{}
  }
}
\section{Methods for function getLegend}{
  \describe{
  \item{\code{signature(x = "Strata")}}{}
  }
}
\section{Methods for functions getNAControl and setNAControl}{
  \describe{
  \item{\code{signature(x = "SimControl")}}{}
  }
}
\section{Methods for function getNArate}{
  \describe{
  \item{\code{signature(x = "SimResults")}}{}
  \item{\code{signature(x = "VirtualNAControl")}}{}
  }
}
\section{Methods for function setNArate}{
  \describe{
  \item{\code{signature(x = "VirtualNAControl")}}{}
  }
}
\section{Methods for function getNr}{
  \describe{
  \item{\code{signature(x = "Strata")}}{}
  }
}
\section{Methods for function getNrep}{
  \describe{
  \item{\code{signature(x = "SimResults")}}{}
  }
}
\section{Methods for function getProb}{
  \describe{
  \item{\code{signature(x = "SampleControl")}}{}
  \item{\code{signature(x = "TwoStageControl")}}{}
  \item{\code{signature(x = "SampleSetup")}}{}
  }
}
\section{Methods for function setProb}{
  \describe{
  \item{\code{signature(x = "SampleControl")}}{}
  \item{\code{signature(x = "TwoStageControl")}}{}
  }
}
\section{Methods for functions getSAE and setSAE}{
  \describe{
  \item{\code{signature(x = "SimControl")}}{}
  }
}
\section{Methods for function getSampleControl}{
  \describe{
  \item{\code{signature(x = "SimResults")}}{}
  }
}
\section{Methods for function getSeed}{
  \describe{
  \item{\code{signature(x = "SampleSetup")}}{}
  \item{\code{signature(x = "SimResults")}}{}
  }
}
\section{Methods for function getSize}{
  \describe{
  \item{\code{signature(x = "DataControl")}}{}
  \item{\code{signature(x = "SampleControl")}}{}
  \item{\code{signature(x = "TwoStageControl")}}{}
  \item{\code{signature(x = "Strata")}}{}
  \item{\code{signature(x = "SummarySampleSetup")}}{}
  }
}
\section{Methods for function setSize}{
  \describe{
  \item{\code{signature(x = "DataControl")}}{}
  \item{\code{signature(x = "SampleControl")}}{}
  \item{\code{signature(x = "TwoStageControl")}}{}
  }
}
\section{Methods for function getSplit}{
  \describe{
  \item{\code{signature(x = "Strata")}}{}
  }
}
\section{Methods for functions getTarget and setTarget}{
  \describe{
  \item{\code{signature(x = "VirtualContControl")}}{}
  \item{\code{signature(x = "VirtualNAControl")}}{}
  }
}
\section{Methods for function getValues}{
  \describe{
  \item{\code{signature(x = "SimResults")}}{}
  \item{\code{signature(x = "Strata")}}{}
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
nc <- NAControl(NArate = 0.05)
getNArate(nc)

setNArate(nc, c(0.01, 0.03, 0.05, 0.07, 0.09))
getNArate(nc)

% TODO: examples for "TwoStageControl"
}
\keyword{classes}
\keyword{methods}
