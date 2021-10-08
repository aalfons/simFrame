\name{runSimulation}
\Rdversion{1.1}
\docType{methods}
\alias{runSimulation}
\alias{runSimulation-methods}
\alias{runSimulation,ANY,ANY,ANY,missing-method}
\alias{runSimulation,data.frame,missing,missing,SimControl-method}
\alias{runSimulation,data.frame,missing,numeric,SimControl-method}
\alias{runSimulation,data.frame,SampleSetup,missing,SimControl-method}
\alias{runSimulation,data.frame,VirtualSampleControl,missing,SimControl-method}
\alias{runSimulation,VirtualDataControl,missing,missing,SimControl-method}
\alias{runSimulation,VirtualDataControl,missing,numeric,SimControl-method}
\alias{runSimulation,VirtualDataControl,VirtualSampleControl,missing,SimControl-method}
\alias{runSimulation,VirtualDataControl,VirtualSampleControl,numeric,SimControl-method}
\alias{runSim}
%% aliases to avoid confusion due to capitalization
\alias{runsimulation}
\alias{Runsimulation}
\alias{RunSimulation}
\alias{runsimulation-methods}
\alias{Runsimulation-methods}
\alias{RunSimulation-methods}

\title{Run a simulation experiment}
\description{
Generic function for running a simulation experiment.
}
\usage{
runSimulation(x, setup, nrep, control, contControl = NULL, 
              NAControl = NULL, design = character(), fun, \dots, 
              SAE = FALSE)

runSim(\dots)
}
\arguments{
  \item{x}{a \code{data.frame} (for design-based simulation or simulation based 
    on real data) or a control object for data generation inheriting from 
    \code{"VirtualDataControl"} (for model-based simulation or mixed simulation 
    designs).}
  \item{setup}{an object of class \code{"SampleSetup"}, containing previously 
    set up samples, or a control class for setting up samples inheriting 
    from \code{"VirtualSampleControl"}.}
  \item{nrep}{a non-negative integer giving the number of repetitions of the 
    simulation experiment (for model-based simulation, mixed simulation designs 
    or simulation based on real data).}
  \item{control}{a control object of class \code{"SimControl"}}    
  \item{contControl}{an object of a class inheriting from 
    \code{"VirtualContControl"}, controlling contamination in the simulation 
    experiment.}
  \item{NAControl}{an object of a class inheriting from 
    \code{"VirtualNAControl"}, controlling the insertion of missing values in 
    the simulation experiment.}
  \item{design}{a character vector specifying variables (columns) to be used 
    for splitting the data into domains.  The simulations, including 
    contamination and the insertion of missing values (unless \code{SAE=TRUE}), 
    are then performed on every domain.}
  \item{fun}{a function to be applied in each simulation run.}
  \item{\dots}{for \code{runSimulation}, additional arguments to be passed 
    to \code{fun}.  For \code{runSim}, arguments to be passed to 
    \code{runSimulation}.}
  \item{SAE}{a logical indicating whether small area estimation will be used in 
    the simulation experiment.}
}
\details{
  For convenience, the slots of \code{control} may be supplied as arguments.
  
  There are some requirements for slot \code{fun} of the control object 
  \code{control}.  The function must return a numeric vector, or a list with 
  the two components \code{values} (a numeric vector) and \code{add} 
  (additional results of any class, e.g., statistical models).  Note that the 
  latter is computationally slightly more expensive.  A \code{data.frame} is 
  passed to \code{fun} in every simulation run.  The corresponding argument 
  must be called \code{x}.  If comparisons with the original data need to be 
  made, e.g., for evaluating the quality of imputation methods, the function 
  should have an argument called \code{orig}.  If different domains are used 
  in the simulation, the indices of the current domain can be passed to the 
  function via an argument called \code{domain}.

  For small area estimation, the following points have to be kept in mind.  The 
  \code{design} for splitting the data must be supplied and \code{SAE} 
  must be set to \code{TRUE}.  However, the data are not actually split into 
  the specified domains.  Instead, the whole data set (sample) is passed to 
  \code{fun}.  Also contamination and missing values are added to the whole 
  data (sample).  Last, but not least, the function must have a \code{domain} 
  argument so that the current domain can be extracted from the whole data 
  (sample).
  
  In every simulation run, \code{fun} is evaluated using \code{try}.  Hence 
  no results are lost if computations fail in any of the simulation runs.
  
  \code{runSim} is a wrapper for \code{runSimulation}.
}
\value{
  An object of class \code{"SimResults"}.
}
\section{Methods}{
  \describe{
  \item{\code{x = "ANY", setup = "ANY", nrep = "ANY", control = "missing"}}{
    convenience wrapper that allows the slots of \code{control} to be supplied 
    as arguments}
  \item{\code{x = "data.frame", setup = "missing", nrep = "missing", 
    control = "SimControl"}}{run a simulation experiment based on real data 
    without repetitions (probably useless, but for completeness).}
  \item{\code{x = "data.frame", setup = "missing", nrep = "numeric", 
    control = "SimControl"}}{run a simulation experiment based on real data 
    with repetitions.}
  \item{\code{x = "data.frame", setup = "SampleSetup", nrep = "missing", 
    control = "SimControl"}}{run a design-based simulation experiment with 
    previously set up samples.}
  \item{\code{x = "data.frame", setup = "VirtualSampleControl", 
    nrep = "missing", control = "SimControl"}}{run a design-based simulation 
    experiment.}
  \item{\code{x = "VirtualDataControl", setup = "missing", nrep = "missing", 
    control = "SimControl"}}{run a model-based simulation experiment without 
    repetitions (probably useless, but for completeness).}
  \item{\code{x = "VirtualDataControl", setup = "missing", nrep = "numeric", 
    control = "SimControl"}}{run a model-based simulation experiment with 
    repetitions.}
  \item{\code{x = "VirtualDataControl", setup = "VirtualSampleControl", 
    nrep = "missing", control = "SimControl"}}{run a simulation experiment 
    using a mixed simulation design without repetitions (probably useless, but 
    for completeness).}
  \item{\code{x = "VirtualDataControl", setup = "VirtualSampleControl", 
    nrep = "numeric", control = "SimControl"}}{run a simulation experiment 
    using a mixed simulation design with repetitions.}
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
  \code{"\linkS4class{SimControl}"}, \code{"\linkS4class{SimResults}"}, 
  \code{\link{simBwplot}}, \code{\link{simDensityplot}}, \code{\link{simXyplot}}
}
\examples{
#### design-based simulation
set.seed(12345)  # for reproducibility
data(eusilcP)    # load data

## control objects for sampling and contamination
sc <- SampleControl(size = 500, k = 50)
cc <- DARContControl(target = "eqIncome", epsilon = 0.02, 
    fun = function(x) x * 25)

## function for simulation runs
sim <- function(x) {
    c(mean = mean(x$eqIncome), trimmed = mean(x$eqIncome, 0.02))
}

## run simulation and explore results
results <- runSimulation(eusilcP, 
    sc, contControl = cc, fun = sim)
head(results)
aggregate(results)
tv <- mean(eusilcP$eqIncome)  # true population mean
plot(results, true = tv)



#### model-based simulation
set.seed(12345)  # for reproducibility

## function for generating data
rgnorm <- function(n, means) {
    group <- sample(1:2, n, replace=TRUE)
    data.frame(group=group, value=rnorm(n) + means[group])
}

## control objects for data generation and contamination
means <- c(0, 0.25)
dc <- DataControl(size = 500, distribution = rgnorm, 
    dots = list(means = means))
cc <- DCARContControl(target = "value", 
    epsilon = 0.02, dots = list(mean = 15))

## function for simulation runs
sim <- function(x) {
    c(mean = mean(x$value), 
        trimmed = mean(x$value, trim = 0.02), 
        median = median(x$value))
}

## run simulation and explore results
results <- runSimulation(dc, nrep = 50, 
    contControl = cc, design = "group", fun = sim)
head(results)
aggregate(results)
plot(results, true = means)
}
\keyword{design}
\keyword{methods}
