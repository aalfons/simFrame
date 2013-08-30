\name{SimControl-class}
\Rdversion{1.1}
\docType{class}
\alias{SimControl-class}
\alias{SimControl}
\alias{getDesign,SimControl-method}
\alias{getDots,SimControl-method}
\alias{getFun,SimControl-method}
\alias{getSeed,SimControl-method}
\alias{show,SimControl-method}

\title{Class \code{"SimControl"}}
\description{Class for controlling how simulation runs are performed.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{SimControl(\dots)}.
}
\section{Slots}{
  \describe{
    \item{\code{contControl}:}{A control object for contamination (inheriting 
      from class \code{"\linkS4class{VirtualContControl}"}), or \code{NULL}.}
    \item{\code{NAControl}:}{A control object for inserting missing values 
      (inheriting from class \code{"\linkS4class{VirtualNAControl}"}), or 
      \code{NULL}.}
    \item{\code{design}:}{An optional character vector specifying variables 
      (columns) to be used for splitting the data into domains.  The 
      simulations, including contamination and the insertion of missing 
      values, are then performed on every domain.}
    \item{\code{fun}:}{A function to be applied in each simulation run.}
    \item{\code{dots}:}{A list containing additional arguments to be passed to 
      \code{fun}.}
    \item{\code{seed}:}{An integer to be used as initial seed of the random 
      number generator.  If not supplied explicitly, the seed will be based on 
      the date and time of construction.}
  }
}
\section{Details}{
  There are some requirements for \code{fun}.  Most importantly, it must return 
  a numeric vector.  A data frame is passed to \code{fun} in every simulation 
  run.  The corresponding argument must be called \code{x}.  If comparisons 
  with a tuning parameter from data generation need to made, the function 
  should have an argument of the same name as the tuning parameter.  If 
  comparisons with the original data need to be made, e.g., for evaluating the 
  quality of imputation methods, the function should have an argument called 
  \code{orig}.  If different domains are used in the simulation, the indices of 
  the current domain can be passed to the function via an argument called 
  \code{domain}.
  
  In every simulation run, \code{fun} is evaluated using \code{try}.  Hence 
  no results are lost if computations fail in any of the simulation runs.
}
\section{Accessor methods}{
  \describe{
    \item{\code{getControl}}{\code{signature(x = "SimControl")}: get the 
      control object for contamination or inserting missing values.}
    \item{\code{getDesign}}{\code{signature(x = "SimControl")}: get slot 
      \code{design}.}
    \item{\code{getFun}}{\code{signature(x = "SimControl")}: get slot 
      \code{fun}.}
    \item{\code{getDots}}{\code{signature(x = "SimControl")}: get slot 
      \code{dots}.}
    \item{getSeed}{\code{signature(x = "SimControl")}: get slot \code{seed}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{head}}{\code{signature(x = "SimControl")}: currently returns 
      the object itself.}
    \item{\code{runSimulation}}{\code{signature(x = "data.frame", 
      setup = "ANY", nrep = "ANY", control = "SimControl")}: run a 
      simulation experiment.}
    \item{\code{runSimulation}}{\code{signature(x = "data.frame", 
      setup = "SampleSetup", nrep = "ANY", control = "SimControl")}: run a 
      simulation experiment.}
    \item{\code{runSimulation}}{\code{signature(x = "data.frame", 
      setup = "VirtualSampleControl", nrep = "ANY", 
      control = "SimControl")}: run a simulation experiment.}
    \item{\code{runSimulation}}{\code{signature(x = "VirtualDataControl", 
      setup = "ANY", nrep = "ANY", control = "SimControl")}: run a 
      simulation experiment.}
    \item{\code{runSimulation}}{\code{signature(x = "VirtualDataControl", 
      setup = "VirtualSampleControl", nrep = "ANY", 
      control = "SimControl")}: run a simulation experiment.}
    \item{\code{show}}{\code{signature(object = "SimControl")}: print the 
      object on the \R console.}
    \item{\code{summary}}{\code{signature(object = "SimControl")}: currently 
      returns the object itself.}
    \item{\code{tail}}{\code{signature(x = "SimControl")}: currently returns 
      the object itself.}
  }
}
\section{UML class diagram}{
  A slightly simplified UML class diagram of the framework can be found in 
  Figure 1 of the package vignette \emph{An Object-Oriented Framework for 
  Statistical Simulation: The \R Package \code{simFrame}}.  Use 
  \code{vignette("simFrame-intro")} to view this vignette.
}
\author{Andreas Alfons}
\references{
Alfons, A., Templ, M. and Filzmoser, P. (2010) An Object-Oriented Framework for 
Statistical Simulation: The \R Package \pkg{simFrame}. \emph{Journal of 
Statistical Software}, \bold{37}(3), 1--36. URL 
\url{http://www.jstatsoft.org/v37/i03/}.
}
\seealso{
  \code{\link{runSimulation}}, \code{"\linkS4class{SimResults}"}
}
\examples{
#### design-based simulation
data("eusilcP")  # load data

## control objects for sampling and contamination
sc <- SampleControl(size = 100, k = 50, seed = 12345)
cc <- ContControl(target = "eqIncome", epsilon = 0.02, 
    fun = function(x) x * 10, type = "CAR")

## function for simulation runs
sim <- function(x) {
    c(mean = mean(x$eqIncome), trimmed = mean(x$eqIncome, 0.05))
}

## combine these to "SimControl" object and run simulation
ctrl <- SimControl(contControl = cc, fun = sim, seed = 12345)
results <- runSimulation(eusilcP, sc, control = ctrl)

## explore results
head(results)
aggregate(results)
tv <- mean(eusilcP$eqIncome)  # true population mean
plot(results) + geom_hline(yintercept=tv)



#### model-based simulation

## function for generating data
rgnorm <- function(n, means) {
    group <- sample(1:2, n, replace=TRUE)
    data.frame(group=group, value=rnorm(n) + means[group])
}

## control objects for data generation and contamination
means <- c(0, 0.25)
dc <- DataControl(size = 100, fun = rgnorm, 
    dots = list(means = means))
cc <- ContControl(target = "value", epsilon = 0.02, 
    dots = list(mean = 10), type = "CCAR")

## function for simulation runs
sim <- function(x) {
    c(mean = mean(x$value), 
        trimmed = mean(x$value, trim = 0.05), 
        median = median(x$value))
}

## combine these to "SimControl" object and run simulation
ctrl <- SimControl(contControl = cc, design = "group", 
    fun = sim, seed = 12345)
results <- runSimulation(dc, nrep = 50, control = ctrl)

## explore results
head(results)
aggregate(results)
tv <- data.frame(mean=means, group=1:2)
plot(results) + geom_hline(aes(yintercept=mean), data=tv)
}
\keyword{classes}
