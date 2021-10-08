\name{simDensityplot}
\Rdversion{1.1}
\docType{methods}
\alias{simDensityplot}
\alias{simDensityplot-methods}
\alias{simDensityplot,SimResults-method}
%% aliases to avoid confusion due to capitalization
\alias{simdensityplot}
\alias{SimDensityplot}
\alias{Simdensityplot}
\alias{simdensityplot-methods}
\alias{SimDensityplot-methods}
\alias{Simdensityplot-methods}

\title{Kernel density plots}
\description{
Generic function for producing kernel density plots.
}
\usage{
simDensityplot(x, \dots)

\S4method{simDensityplot}{SimResults}(x, true = NULL, epsilon, NArate, select, \dots)
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
  \item{\dots}{additional arguments to be passed down to methods and eventually
    to \code{\link[lattice:histogram]{densityplot}}.}
}
\details{
  For simulation results with multiple contamination levels or missing value
  rates, conditional kernel density plots are produced.
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
  \item{\code{x = "SimResults"}}{produce kernel density plots of simulation
    results.}
  }
}
\note{
  Functionality for producing conditional kernel density plots was added in
  version 0.2.  Prior to that, the function gave an error message if simulation
  results with multiple contamination levels or missing value rates were
  supplied.
}
\author{Andreas Alfons}
\references{
Alfons, A., Templ, M. and Filzmoser, P. (2010) An Object-Oriented Framework for
Statistical Simulation: The \R Package \pkg{simFrame}. \emph{Journal of
Statistical Software}, \bold{37}(3), 1--36. \doi{10.18637/jss.v037.i03}.
}
\seealso{
  \code{\link{simBwplot}}, \code{\link{simXyplot}},
  \code{\link[lattice:histogram]{densityplot}},
  \code{"\linkS4class{SimResults}"}
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

## run simulation
results <- runSimulation(eusilcP,
    sc, contControl = cc, fun = sim)

## plot results
tv <- mean(eusilcP$eqIncome)  # true population mean
simDensityplot(results, true = tv)



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

## run simulation
results <- runSimulation(dc, nrep = 50,
    contControl = cc, design = "group", fun = sim)

## plot results
simDensityplot(results, true = means)
}
\keyword{hplot}
\keyword{methods}
