\name{fortify-methods}
\docType{methods}
\alias{fortify-methods}
\alias{fortify,SimResults-method}

\title{Convert simulation results into a data frame for plotting}
\description{
  Extract all necessary information for plotting from simulation results.  It 
  is thereby possible to construct data frames for box plots, density plots, as 
  well as plots of the average results against some tuning parameter.  Although 
  a suitable plot is selected automatically depending on the structure of the 
  results, the default behavior can be overriden.
}
\usage{
\S4method{fortify}{SimResults}(model, data = NULL, cont = NULL, 
        NARate = NULL, select = NULL, 
        method = c("box", "density", "line"), 
        average = c("mean", "median"), 
        se = TRUE, \dots)
}
\arguments{
  \item{model}{the simulation results to be converted, i.e., an object of class 
    \code{"\linkS4class{SimResults}"}.}

  \item{data}{an optional integer or logical index vector specifying the data 
    configurations for which to convert simulation results.}

  \item{cont}{an optional integer or logical index vector specifying the 
    contamination settings for which to convert simulation results.}

  \item{NARate}{an optional integer or logical index vector specifying the 
    missing value rates for which to convert simulation results.}

  \item{select}{an optional integer, character or logical vector specifying the 
    columns to be converted.  It must specify a subset of the \code{colnames} 
    slot of the simulation results, which is the default.}

  \item{method}{a character string specifying for which plot to construct the 
    data frame.  Possible values are \code{"box"} for a box plot, 
    \code{"density"} for a density plot or \code{"line"} for a plot of the 
    average results against some tuning parameter.  If not specified, a 
    suitable plot is selected automatically.}

  \item{average}{if \code{method} is \code{"line"}, a character string 
    specifying how the averages should be computed.  Possible values are 
    \code{"mean"} for the mean (the default) or \code{"median"} for the 
    median.}

  \item{se}{if \code{method} is \code{"line"}, a logical indicating whether 
    to display an approximate confidence interval (defaults to \code{TRUE}).}

  \item{\dots}{additional arguments are currently ignored.}
}
\value{
  A data frame containing the columns listed below, and possibly other 
  columns with information on the different controls of the simulation 
  experiment.  Additional information for plotting is stored in the attributes 
  \code{"mapping"} (default aesthetic mapping), \code{"geom"} (function 
  drawing the visual representation), \code{"facets"} (default faceting 
  formula) and \code{"method"} (type of plot).

  \item{Method}{a factor containing the names for the simulation results.}

  \item{Value}{the (average) simulation results.}

  \item{Lower}{the lower end points of the approximate confidence intervals for 
    the averages (only returned if \code{method} is \code{"line"}).}

  \item{Upper}{the upper end points of the approximate confidence intervals for 
    the averages (only returned if \code{method} is \code{"line"}).}
}
\section{Methods}{
  \describe{
  \item{\code{model = "SimResults"}}{convert simulation results.}
  }
}
\author{
  Andreas Alfons
}
\references{
Alfons, A., Templ, M. and Filzmoser, P. (2010) An Object-Oriented Framework for 
Statistical Simulation: The \R Package \pkg{simFrame}. \emph{Journal of 
Statistical Software}, \bold{37}(3), 1--36. URL 
\url{http://www.jstatsoft.org/v37/i03/}.
}
\seealso{
  \code{\link{simPlot}}, \code{"\linkS4class{SimResults}"}
}
\examples{
#### design-based simulation
data("eusilcP")  # load data

## control objects for sampling and contamination
sc <- SampleControl(size = 100, k = 50, seed = 12345)
cc <- ContControl(target = "eqIncome", 
    epsilon = seq(0, 0.05, by = 0.01), 
    fun = function(x) x * 10, type = "CAR")

## function for simulation runs
sim <- function(x) {
    c(mean = mean(x$eqIncome), trimmed = mean(x$eqIncome, 0.05))
}

## run simulation
results <- runSimulation(eusilcP, sc, contControl = cc, 
    fun = sim, seed = 12345)

## explore results
tv <- mean(eusilcP$eqIncome)  # true population mean
# all results
df <- fortify(results)
df
simPlot(df) + geom_hline(yintercept=tv)
# subset
df <- fortify(results, cont = 3)
simPlot(df) + geom_hline(yintercept=tv)



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
cc <- ContControl(target = "value", 
    epsilon = seq(0, 0.05, by = 0.01), 
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

## explore results
tv <- data.frame(mean=means, group=1:2)
# all results
df <- fortify(results)
df
simPlot(df) + geom_hline(aes(yintercept=mean), data=tv)
# subset
df <- fortify(results, cont = 3)
simPlot(df) + geom_hline(aes(yintercept=mean), data=tv)
}
\keyword{methods}
\keyword{utilities}
