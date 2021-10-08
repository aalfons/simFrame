\name{simXyplot}
\Rdversion{1.1}
\docType{methods}
\alias{simXyplot}
\alias{simXyplot-methods}
\alias{simXyplot,SimResults-method}
%% aliases to avoid confusion due to capitalization
\alias{simxyplot}
\alias{SimXyplot}
\alias{Simxyplot}
\alias{simxyplot-methods}
\alias{SimXyplot-methods}
\alias{Simxyplot-methods}

\title{X-Y plots}
\description{
Generic function for producing x-y plots.  For simulation results, the average
results are plotted against the corresponding contamination levels or missing
value rates.
}
\usage{
simXyplot(x, \dots)

\S4method{simXyplot}{SimResults}(x, true = NULL, epsilon, NArate,
          select, cond = c("Epsilon", "NArate"),
          average = c("mean", "median"), \dots)
}
\arguments{
  \item{x}{the object to be plotted.  For plotting simulation results, this
    must be an object of class \code{"SimResults"}.}
  \item{true}{a numeric vector giving the true values.  If supplied,
    reference lines are drawn in the corresponding panels.}
  \item{epsilon}{a numeric vector specifying contamination levels.  If
    supplied, the values corresponding to these contamination levels are
    extracted from the simulation results and plotted.}
  \item{NArate}{a numeric vector specifying missing value rates.  If
    supplied, the values corresponding to these missing value rates are
    extracted from the simulation results and plotted.}
  \item{select}{a character vector specifying the columns to be plotted.  It
    must be a subset of the \code{colnames} slot of \code{x}, which is the
    default.}
  \item{cond}{a character string; for simulation results with multiple
    contamination levels and multiple missing value rates, this specifies
    the column of the simulation results to be used for producing conditional
    x-y plots.  If \code{"Epsilon"}, conditional plots are produced for the
    different contamination levels.  If \code{"NArate"}, conditional plots are
    produced for the different missing value rates.  The default is to use
    whichever results in less plots.}
  \item{average}{a character string specifying how the averages should be
    computed.  Possible values are \code{"mean"} for the mean (the default)
    or \code{"median"} for the median.}
  \item{\dots}{additional arguments to be passed down to methods and eventually
    to \code{\link[lattice]{xyplot}}.}
}
\details{
  For simulation results with multiple contamination levels and multiple
  missing value rates, conditional x-y plots are produced, as specified by
  \code{cond}.
}
\value{
  An object of class \code{"trellis"}.  The
  \code{\link[lattice:update.trellis]{update}} method can be used to update
  components of the object and the \code{\link[lattice:print.trellis]{print}}
  method (usually called by default) will plot it on an appropriate plotting
  device.
}
\section{Methods}{
  \describe{
  \item{\code{x = "SimResults"}}{produce x-y plots of simulation results.}
  }
}
\note{
  Functionality for producing conditional x-y plots (including the argument
  \code{cond}) was added in version 0.2.  Prior to that, the function gave an
  error message if simulation results with multiple contamination levels and
  multiple missing value rates were supplied.

  The argument \code{average} that specifies how the averages are computed
  was added in version 0.1.2.  Prior to that, the mean has always been used.
}
\author{Andreas Alfons}
\references{
Alfons, A., Templ, M. and Filzmoser, P. (2010) An Object-Oriented Framework for
Statistical Simulation: The \R Package \pkg{simFrame}. \emph{Journal of
Statistical Software}, \bold{37}(3), 1--36. \doi{10.18637/jss.v037.i03}.
}
\seealso{
  \code{\link{simBwplot}}, \code{\link{simDensityplot}},
  \code{\link[lattice]{xyplot}}, \code{"\linkS4class{SimResults}"}
}
\examples{
#### design-based simulation
set.seed(12345)  # for reproducibility
data(eusilcP)    # load data

## control objects for sampling and contamination
sc <- SampleControl(size = 500, k = 50)
cc <- DARContControl(target = "eqIncome",
    epsilon = seq(0, 0.05, by = 0.01),
    fun = function(x) x * 25)

## function for simulation runs
sim <- function(x) {
    c(mean = mean(x$eqIncome), trimmed = mean(x$eqIncome, 0.05))
}

## run simulation
results <- runSimulation(eusilcP,
    sc, contControl = cc, fun = sim)

## plot results
tv <- mean(eusilcP$eqIncome)  # true population mean
simXyplot(results, true = tv)



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
    epsilon = seq(0, 0.05, by = 0.01),
    dots = list(mean = 15))

## function for simulation runs
sim <- function(x) {
    c(mean = mean(x$value),
        trimmed = mean(x$value, trim = 0.05),
        median = median(x$value))
}

## run simulation
results <- runSimulation(dc, nrep = 50,
    contControl = cc, design = "group", fun = sim)

## plot results
simXyplot(results, true = means)
}
\keyword{hplot}
\keyword{methods}
