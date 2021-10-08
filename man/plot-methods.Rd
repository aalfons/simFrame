\name{plot-methods}
\docType{methods}
\alias{plot-methods}
\alias{plot,SimResults,missing-method}

\title{Plot simulation results}
\description{
Plot simulation results.  A suitable plot function is selected 
automatically, depending on the structure of the results.
}
\usage{
\S4method{plot}{SimResults,missing}(x, y , \dots)
}
\arguments{
  \item{x}{the simulation results.}
  \item{y}{not used.}
  \item{\dots}{further arguments to be passed to the selected plot function.}
}
\section{Details}{
  The results of simulation experiments with at most one contamination level 
  and at most one missing value rate are visualized by (conditional) 
  box-and-whisker plots.  For simulations involving different contamination 
  levels or missing value rates, the average results are plotted against the 
  contamination levels or missing value rates.
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
  \item{\code{x = "SimResults", y = "missing"}}{plot simulation results.}
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
  \code{\link{simBwplot}}, \code{\link{simDensityplot}}, 
  \code{\link{simXyplot}}, \code{"\linkS4class{SimResults}"}
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

## run simulation
results <- runSimulation(dc, nrep = 50, 
    contControl = cc, design = "group", fun = sim)

## plot results
plot(results, true = means)
}
\keyword{methods}
\keyword{hplot}
