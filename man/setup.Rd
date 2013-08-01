\name{setup}
\docType{methods}
\alias{setup}
\alias{setup-methods}
\alias{setup,data.frame,BasicSampleControl-method}
\alias{setup,data.frame,character-method}
\alias{setup,data.frame,missing-method}
\alias{setup,data.frame,TwoStageSampleControl-method}

\title{Set up multiple samples}
\description{
Generic function for setting up multiple samples.
}
\usage{
setup(x, control, \dots)
}
\arguments{
  \item{x}{the data to sample from.}
  \item{control}{a control object inheriting from the virtual class 
    \code{"VirtualSampleControl"} or a character string specifying such a 
    control class.  If missing, the constructor \code{\link{SampleControl}} is 
    called to generate the control object.}
  \item{\dots}{if \code{control} is a character string or missing, the slots of 
    the control object may be supplied as additional arguments.  See 
    \code{"\linkS4class{BasicSampleControl}"} and 
    \code{"\linkS4class{TwoStageSampleControl}"} for details on the slots.}
}
\details{
  A fundamental design principle of the framework in the case of design-based 
  simulation studies is that the sampling procedure is separated from the 
  simulation procedure.  Two main advantages arise from setting up all samples 
  in advance. 
  
  First, the repeated sampling reduces overall computation time dramatically in 
  certain situations, since computer-intensive tasks like stratification need 
  to be performed only once.  This is particularly relevant for large 
  population data.  In close-to-reality simulation studies carried out in 
  research projects in survey statistics, often up to 10000 samples are drawn 
  from a population of millions of individuals with stratified sampling 
  designs.  For such large data sets, stratification takes a considerable 
  amount of time and is a very memory-intensive task.  If the samples are taken 
  on-the-fly, i.e., in every simulation run one sample is drawn, the function 
  to take the stratified sample would typically split the population into the 
  different strata in each of the 10000 simulation runs.  If all samples are 
  drawn in advance, on the other hand, the population data need to be split 
  only once and all 10000 samples can be taken from the respective strata 
  together.
  
  Second, the samples can be stored permanently, which simplifies the 
  reproduction of simulation results and may help to maximize comparability of 
  results obtained by different partners in a research project.  In particular, 
  this is useful for large population data, when complex sampling techniques 
  may be very time-consuming.  In research projects involving different 
  partners, usually different groups investigate different kinds of estimators. 
  If the two groups use not only the same population data, but also the same 
  previously set up samples, their results are highly comparable.
  
  The control classes \code{"BasicSampleControl"} and 
  \code{"TwoStageSampleControl"} are highly flexible and cover many frequently 
  used sampling designs such as stratified (two-stage) sampling with a 
  specified sampling method.  Hence it is often sufficient to implement the 
  desired sampling method for the simple non-stratified case to extend the 
  existing framework.  See \code{"\linkS4class{BasicSampleControl}"} and 
  \code{"\linkS4class{TwoStageSampleControl}"} for some restrictions on the 
  argument names of such a function, which should return a vector containing 
  the indices of the sampled observations.
  
  Nevertheless, for very complex sampling procedures, it is possible to define 
  a control class \code{"MySampleControl"} extending 
  \code{"\linkS4class{VirtualSampleControl}"}, and the corresponding method 
  \code{setup(x, control)} with signature \code{'data.frame, MySampleControl'}.  
  In order to optimize computational performance, it is necessary to 
  efficiently set up multiple samples.  Thereby the slot \code{k} of 
  \code{"VirtualSampleControl"} needs to be used to control the number of 
  samples, and the resulting object must be of class 
  \code{"\linkS4class{SampleSetup}"}.
}
\value{
  An object of class \code{"SampleSetup"}.
}
\section{Methods}{
  \describe{
  \item{\code{x = "data.frame", control = "character"}}{set up multiple samples 
    using a control class specified by the character string \code{control}.  
    The slots of the control object may be supplied as additional arguments.}
  \item{\code{x = "data.frame", control = "missing"}}{set up multiple samples 
    using a control object constructed by \code{\link{SampleControl}}.  Its 
    slots may be supplied as additional arguments.}
  \item{\code{x = "data.frame", control = "BasicSampleControl"}}{set up 
    multiple samples for basic sampling designs.}
  \item{\code{x = "data.frame", control = "TwoStageSampleControl"}}{set up 
    multiple samples for two-stage sampling designs.}
  }
}
\author{Andreas Alfons}
\references{
Alfons, A., Templ, M. and Filzmoser, P. (2010) An Object-Oriented Framework for 
Statistical Simulation: The \R Package \pkg{simFrame}. \emph{Journal of 
Statistical Software}, \bold{37}(3), 1--36. URL 
\url{http://www.jstatsoft.org/v37/i03/}.
}
\seealso{
  \code{\link{draw}}, 
  \code{"\linkS4class{BasicSampleControl}"}, 
  \code{"\linkS4class{TwoStageSampleControl}"}, 
  \code{"\linkS4class{SampleSetup}"}
}
\examples{
data("eusilcP")    # load data

## simple random sampling
srss <- setup(eusilcP, size = 20, k = 4, seed = 12345)
summary(srss)
draw(eusilcP[, c("id", "eqIncome")], srss, i = 1)

## group sampling
gss <- setup(eusilcP, grouping = "hid", size = 10, 
    k = 4, seed = 12345)
summary(gss)
draw(eusilcP[, c("hid", "id", "eqIncome")], gss, i = 2)

## stratified simple random sampling
ssrss <- setup(eusilcP, design = "region", 
    size = c(2, 5, 5, 3, 4, 5, 3, 5, 2), 
    k = 4, seed = 12345)
summary(ssrss)
draw(eusilcP[, c("id", "region", "eqIncome")], ssrss, i = 3)

## stratified group sampling
sgss <- setup(eusilcP, design = "region", 
    grouping = "hid", size = c(2, 5, 5, 3, 4, 5, 3, 5, 2), 
    k = 4, seed = 12345)
summary(sgss)
draw(eusilcP[, c("hid", "id", "region", "eqIncome")], sgss, i = 4)
}
\keyword{distribution}
\keyword{methods}
