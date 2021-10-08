\name{SimControl-class}
\Rdversion{1.1}
\docType{class}
\alias{SimControl-class}
\alias{SimControl}
\alias{getContControl,SimControl-method}
\alias{setContControl,SimControl-method}
\alias{getNAControl,SimControl-method}
\alias{setNAControl,SimControl-method}
\alias{getDesign,SimControl-method}
\alias{setDesign,SimControl-method}
\alias{getFun,SimControl-method}
\alias{setFun,SimControl-method}
\alias{getDots,SimControl-method}
\alias{setDots,SimControl-method}
\alias{getSAE,SimControl-method}
\alias{setSAE,SimControl-method}
\alias{show,SimControl-method}
%% aliases to avoid confusion due to capitalization
\alias{simControl-class}
\alias{simcontrol-class}
\alias{Simcontrol-class}
\alias{simControl}
\alias{simcontrol}
\alias{Simcontrol}

\title{Class "SimControl"}
\description{Class for controlling how simulation runs are performed.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SimControl", \dots)} or
\code{SimControl(\dots)}.
}
\section{Slots}{
  \describe{
    \item{\code{contControl}:}{Object of class \code{"OptContControl"}; a
      control object for contamination, or \code{NULL}.}
    \item{\code{NAControl}:}{Object of class \code{"OptNAControl"}; a control
      object for inserting missing values, or \code{NULL}.}
    \item{\code{design}:}{Object of class \code{"character"} specifying
      variables (columns) to be used for splitting the data into domains.  The
      simulations, including contamination and the insertion of missing values
      (unless \code{SAE=TRUE}), are then performed on every domain.}
    \item{\code{fun}:}{Object of class \code{"function"} to be applied in each
      simulation run.}
    \item{\code{dots}:}{Object of class \code{"list"} containing additional
      arguments to be passed to \code{fun}.}
    \item{\code{SAE}:}{Object of class \code{"logical"} indicating whether
      small area estimation will be used in the simulation experiment.}
  }
}
\section{Details}{
  There are some requirements for \code{fun}.  It must return a numeric vector,
  or a list with the two components \code{values} (a numeric vector) and
  \code{add} (additional results of any class, e.g., statistical models).
  Note that the latter is computationally slightly more expensive.  A
  \code{data.frame} is passed to \code{fun} in every simulation run.  The
  corresponding argument must be called \code{x}.  If comparisons with the
  original data need to be made, e.g., for evaluating the quality of imputation
  methods, the function should have an argument called \code{orig}.  If
  different domains are used in the simulation, the indices of the current
  domain can be passed to the function via an argument called \code{domain}.

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
}
\section{Accessor and mutator methods}{
  \describe{
    \item{\code{getContControl}}{\code{signature(x = "SimControl")}: get slot
      \code{ContControl}.}
    \item{\code{setContControl}}{\code{signature(x = "SimControl")}: set slot
      \code{ContControl}.}
    \item{\code{getNAControl}}{\code{signature(x = "SimControl")}: get slot
      \code{NAControl}.}
    \item{\code{setNAControl}}{\code{signature(x = "SimControl")}: set slot
      \code{NAControl}.}
    \item{\code{getDesign}}{\code{signature(x = "SimControl")}: get slot
      \code{design}.}
    \item{\code{setDesign}}{\code{signature(x = "SimControl")}: set slot
      \code{design}.}
    \item{\code{getFun}}{\code{signature(x = "SimControl")}: get slot
      \code{fun}.}
    \item{\code{setFun}}{\code{signature(x = "SimControl")}: set slot
      \code{fun}.}
    \item{\code{getDots}}{\code{signature(x = "SimControl")}: get slot
      \code{dots}.}
    \item{\code{setDots}}{\code{signature(x = "SimControl")}: set slot
      \code{dots}.}
    \item{\code{getSAE}}{\code{signature(x = "SimControl")}: get slot
      \code{SAE}.}
    \item{\code{setSAE}}{\code{signature(x = "SimControl")}: set slot
      \code{SAE}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{clusterRunSimulation}}{\code{signature(cl = "ANY",
      x = "data.frame", setup = "missing", nrep = "numeric",
      control = "SimControl")}: run a simulation experiment on a cluster.}
    \item{\code{clusterRunSimulation}}{\code{signature(cl = "ANY",
      x = "data.frame", setup = "VirtualSampleControl", nrep = "missing",
      control = "SimControl")}: run a simulation experiment on a cluster.}
    \item{\code{clusterRunSimulation}}{\code{signature(cl = "ANY",
      x = "data.frame", setup = "SampleSetup", nrep = "missing",
      control = "SimControl")}: run a simulation experiment on a cluster.}
    \item{\code{clusterRunSimulation}}{\code{signature(cl = "ANY",
      x = "VirtualDataControl", setup = "missing", nrep = "numeric",
      control = "SimControl")}: run a simulation experiment on a cluster.}
    \item{\code{clusterRunSimulation}}{\code{signature(cl = "ANY",
      x = "VirtualDataControl", setup = "VirtualSampleControl",
      nrep = "numeric", control = "SimControl")}: run a simulation experiment
      on a cluster.}
    \item{\code{head}}{\code{signature(x = "SimControl")}: currently returns
      the object itself.}
    \item{\code{runSimulation}}{\code{signature(x = "data.frame",
      setup = "VirtualSampleControl", nrep = "missing",
      control = "SimControl")}: run a simulation experiment.}
    \item{\code{runSimulation}}{\code{signature(x = "data.frame",
      setup = "SampleSetup", nrep = "missing", control = "SimControl")}: run a
      simulation experiment.}
    \item{\code{runSimulation}}{\code{signature(x = "data.frame",
      setup = "missing", nrep = "numeric", control = "SimControl")}: run a
      simulation experiment.}
    \item{\code{runSimulation}}{\code{signature(x = "data.frame",
      setup = "missing", nrep = "missing", control = "SimControl")}: run a
      simulation experiment.}
    \item{\code{runSimulation}}{\code{signature(x = "VirtualDataControl",
      setup = "missing", nrep = "numeric", control = "SimControl")}: run a
      simulation experiment.}
    \item{\code{runSimulation}}{\code{signature(x = "VirtualDataControl",
      setup = "missing", nrep = "missing", control = "SimControl")}: run a
      simulation experiment.}
    \item{\code{runSimulation}}{\code{signature(x = "VirtualDataControl",
      setup = "VirtualSampleControl", nrep = "numeric",
      control = "SimControl")}: run a simulation experiment.}
    \item{\code{runSimulation}}{\code{signature(x = "VirtualDataControl",
      setup = "VirtualSampleControl", nrep = "missing",
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
Statistical Software}, \bold{37}(3), 1--36. \doi{10.18637/jss.v037.i03}.
}
\seealso{
  \code{\link{runSimulation}}, \code{"\linkS4class{SimResults}"}
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

## combine these to "SimControl" object and run simulation
ctrl <- SimControl(contControl = cc, fun = sim)
results <- runSimulation(eusilcP, sc, control = ctrl)

## explore results
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

## combine these to "SimControl" object and run simulation
ctrl <- SimControl(contControl = cc, design = "group", fun = sim)
results <- runSimulation(dc, nrep = 50, control = ctrl)

## explore results
head(results)
aggregate(results)
plot(results, true = means)
}
\keyword{classes}
