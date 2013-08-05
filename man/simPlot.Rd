\name{simPlot}
\docType{methods}
\alias{simPlot}
\alias{simPlot-methods}
\alias{simPlot,data.frame-method}
\alias{simPlot,SimResults-method}
\alias{autoplot,SimResults-method}
\alias{plot,SimResults,missing-method}

\title{Plot simulation results}
\description{
  Plot simulation results.  It is thereby possible to produce box plots, 
  density plots, as well as to plot the average results against some tuning 
  parameter.  Although a suitable plot is selected automatically depending on 
  the structure of the results, the default behavior can be overriden.
}
\usage{
\S4method{simPlot}{SimResults}(object, data = NULL, cont = NULL, 
        miss = NULL, select = NULL, 
        method = c("box", "density", "line"), 
        average = c("mean", "median"), 
        se = TRUE, \dots)

\S4method{simPlot}{data.frame}(object, mapping = attr(object, "mapping"), 
        facets = attr(object, "facets"), 
        labels = NULL, \dots)

\S4method{autoplot}{SimResults}(object, \dots)

\S4method{plot}{SimResults,missing}(x, y , \dots)
}
\arguments{
  \item{object, x}{the simulation results to be plotted, i.e., an object of 
    class \code{"\linkS4class{SimResults}"}.  For \code{simPlot}, this may also 
    be a data frame constructed from such an object via 
    \code{\link[simFrame:fortify-methods]{fortify}}.}
  
  \item{y}{not used.}

  \item{data}{an optional integer or logical index vector specifying the data 
    configurations for which to plot simulation results.}

  \item{cont}{an optional integer or logical index vector specifying the 
    contamination settings for which to plot simulation results.}

  \item{miss}{an optional integer or logical index vector specifying the 
    missing data settings for which to plot simulation results.}

  \item{select}{an optional integer, character or logical vector specifying the 
    columns to be plotted.  It must specify a subset of the \code{colnames} 
    slot of the simulation results, which is the default.}

  \item{method}{a character string specifying which plot to produce.  Possible 
    values are \code{"box"} for a box plot, \code{"density"} for a density plot 
    or \code{"line"} for plotting the average results against some tuning 
    parameter.  If not specified, a suitable plot is selected automatically.}

  \item{average}{if \code{method} is \code{"line"}, a character string 
    specifying how the averages should be computed.  Possible values are 
    \code{"mean"} for the mean (the default) or \code{"median"} for the 
    median.}

  \item{se}{if \code{method} is \code{"line"}, a logical indicating whether 
    to display an approximate confidence interval (defaults to \code{FALSE}).}

  \item{mapping}{an aesthetic mapping to override the default behavior (see 
    \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_string}}).}

  \item{facets}{a faceting formula to override the default behavior.  If 
    supplied, \code{\link[ggplot2]{facet_wrap}} or 
    \code{\link[ggplot2]{facet_grid}} is called depending on whether the 
    formula is one-sided or two-sided.}

  \item{labels}{an optional character vector specifying specifying labels for 
    the simulation results to be used in the plot instead of the column names.}

  \item{\dots}{additional arguments to be passed down, eventually to the 
    function drawing the visual representation (see 
    \code{\link[ggplot2]{geom_boxplot}}, \code{\link[ggplot2]{geom_density}}, 
    \code{\link[ggplot2]{geom_smooth}} or \code{\link[ggplot2]{geom_line}}).}
}
\value{
  An object of class \code{"ggplot"} (see \code{\link[ggplot2]{ggplot}}).
}
\section{Methods for function \code{simPlot}}{
  \describe{
  \item{\code{signature(object = "SimResults")}}{plot simulation results.}
  \item{\code{signature(object = "data.frame")}}{plot simulation 
    results that have been converted to a data frame via 
    \code{\link[simFrame:fortify-methods]{fortify}}.}
  }
}
\section{Methods for function \code{autoplot}}{
  \describe{
  \item{\code{signature(object = "SimResults")}}{plot simulation results.}
  }
}
\section{Methods for function \code{plot}}{
  \describe{
  \item{\code{signature(x = "SimResults", y = "missing")}}{plot simulation 
    results.}
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
  \code{\link[simFrame:fortify-methods]{fortify}}, 
  \code{"\linkS4class{SimResults}"}
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

## plot results
tv <- mean(eusilcP$eqIncome)  # true population mean
# all results
plot(results) + geom_hline(yintercept=tv)
# subset
plot(results, cont = 3) + geom_hline(yintercept=tv)



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

## plot results
tv <- data.frame(mean=means, group=1:2)
# all results
plot(results) + geom_hline(aes(yintercept=mean), data=tv)
# subset
plot(results, cont = 3) + geom_hline(aes(yintercept=mean), data=tv)
}
\keyword{methods}
\keyword{hplot}
