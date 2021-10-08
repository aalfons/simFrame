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
    \code{"SimResults"}.}
  \item{select}{a character vector specifying the columns to be aggregated.  It 
    must be a subset of the \code{colnames} slot of \code{x}, which is the 
    default.}
  \item{FUN}{a scalar function to compute the summary statistics (defaults to 
    \code{mean}).}
  \item{\dots}{additional arguments to be passed down to 
    \code{\link[stats]{aggregate}} or \code{\link{apply}}.}
}
\section{Details}{
  If contamination or missing values have been inserted or the simulations have 
  been split into different domains, \code{\link[stats]{aggregate}} is called 
  to compute the summary statistics for the respective subsets.
  
  Otherwise, \code{\link{apply}} is called to compute the summary statistics 
  for each column specified by \code{select}.
}
\value{
  If contamination or missing values have been inserted or the simulations have 
  been split into different domains, a \code{data.frame} is returned, otherwise 
  a vector. 
}
\section{Methods}{
  \describe{
  \item{\code{x = "SimResults"}}{aggregate simulation results.}
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
  \code{\link[stats]{aggregate}}, \code{\link{apply}}, 
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

## aggregate
aggregate(results)  # means of results
aggregate(results, FUN = sd)  # standard deviations of results


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

## aggregate
aggregate(results)  # means of results
aggregate(results, FUN = sd)  # standard deviations of results
}
\keyword{methods}
\keyword{category}
