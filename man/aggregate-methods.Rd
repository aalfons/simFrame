\name{aggregate-methods}
\docType{methods}
\alias{aggregate-methods}
\alias{aggregate,SimResults-method}

\title{Method for aggregating simulation results}
\description{
  Aggregate simulation results, i.e, split the data into subsets if applicable 
  and compute summary statistics.
}
\usage{
\S4method{aggregate}{SimResults}(x, select = NULL, FUN = mean, \dots)
}
\arguments{
  \item{x}{the simulation results to be aggregated, i.e., an object of class 
    \code{"\linkS4class{SimResults}"}.}
  \item{select}{an integer, character or logical vector specifying the columns 
    to be aggregated.  It must specify a subset of the \code{colnames} slot of 
    the simulation results, which is the default.}
  \item{FUN}{a function to compute the summary statistics (defaults to 
    \code{mean}).}
  \item{\dots}{additional arguments to be passed down to methods.}
}
\section{Details}{
  The summary statistics of the simulation results are computed using the 
  \code{\link[stats]{aggregate}} method for the data frame in slot 
  \code{values}.
}
\value{
  A data frame containing the summary statistics for the different subsets of 
  the simulation results. 
}
\section{Methods}{
  \describe{
  \item{\code{signature(x = "SimResults")}}{aggregate simulation results.}
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
  \code{\link[stats]{aggregate}}, \code{"\linkS4class{SimResults}"}
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

## run simulation
results <- runSimulation(eusilcP, sc, contControl = cc, 
    fun = sim, seed = 12345)

## aggregate
aggregate(results)            # means of results
aggregate(results, FUN = sd)  # standard deviations of results


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

## run simulation
results <- runSimulation(dc, nrep = 50, contControl = cc, 
    design = "group", fun = sim, seed = 12345)

## aggregate
aggregate(results)            # means of results
aggregate(results, FUN = sd)  # standard deviations of results
}
\keyword{methods}
\keyword{category}
