\name{simFrame-package}
\alias{simFrame-package}
\alias{simFrame}
\docType{package}
\title{
Simulation framework
}
\description{
A general framework for statistical simulation.
}
\details{
\tabular{ll}{
Package: \tab simFrame\cr
Version: \tab 0.6.0\cr
Date: \tab 2013-07-13\cr
Depends: \tab R (>= 2.14.0), Rcpp (>= 0.8.6), parallel, ggplot2 (>= 0.9.3)\cr
Imports: \tab methods, stats4\cr
Suggests: \tab laeken, mvtnorm, robCompositions, sampling\cr
LinkingTo: \tab Rcpp\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}

Index:
\preformatted{
BasicContControl-class
                        Class '"BasicContControl"'
BasicSampleControl-class
                        Class '"BasicSampleControl"'
BasicVector-class       Class '"BasicVector"'
CARContControl-class    Class '"CARContControl"'
CCARContControl-class   Class '"CCARContControl"'
ContControl             Create contamination control objects
ContControl-class       Class '"ContControl"'
DataControl-class       Class '"DataControl"'
ListOrDataFrame-class   Class '"ListOrDataFrame"'
NAControl-class         Class '"NAControl"'
NumericMatrix-class     Class '"NumericMatrix"'
OptCall-class           Class '"OptCall"'
OptCharacter-class      Class '"OptCharacter"'
OptContControl-class    Class '"OptContControl"'
OptDataControl-class    Class '"OptDataControl"'
OptNAControl-class      Class '"OptNAControl"'
OptNumeric-class        Class '"OptNumeric"'
OptSampleControl-class
                        Class '"OptSampleControl"'
RandomContControl-class
                        Class '"RandomContControl"'
SampleControl           Create sample control objects
SampleSetup-class       Class '"SampleSetup"'
SimControl-class        Class '"SimControl"'
SimResults-class        Class '"SimResults"'
SummarySampleSetup-class
                        Class '"SummarySampleSetup"'
TwoStageSampleControl-class
                        Class '"TwoStageSampleControl"'
VirtualContControl-class
                        Class '"VirtualContControl"'
VirtualDataControl-class
                        Class '"VirtualDataControl"'
VirtualNAControl-class
                        Class '"VirtualNAControl"'
VirtualSampleControl-class
                        Class '"VirtualSampleControl"'
aggregate-methods       Method for aggregating simulation results
contaminate             Contaminate data
draw                    Draw a sample
eusilcP                 Synthetic EU-SILC data
fortify-methods         Convert simulation results into a 'data.frame'
                        for plotting
generate                Generate data
getAux                  Accessor functions for objects
head-methods            Methods for returning the first parts of an
                        object
inclusionProb           Inclusion probabilities
length-methods          Methods for getting the length of an object
runSimulation           Run a simulation experiment
sampling                Random sampling
setNA                   Set missing values
setup                   Set up multiple samples
simFrame-package        Simulation framework
simPlot                 Plot simulation results
subset-methods          Subsetting simulation results
summary-methods         Methods for producing a summary of an object
tail-methods            Methods for returning the last parts of an
                        object
}
}
\author{
Andreas Alfons [aut, cre]

Maintainer: Andreas Alfons <alfons@ese.eur.nl>
}
\references{
Alfons, A., Templ, M. and Filzmoser, P. (2010) An Object-Oriented Framework for 
Statistical Simulation: The \R Package \pkg{simFrame}. \emph{Journal of 
Statistical Software}, \bold{37}(3), 1--36. URL 
\url{http://www.jstatsoft.org/v37/i03/}.
}
\keyword{package}
