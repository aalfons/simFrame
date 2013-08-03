\name{subset-methods}
\docType{methods}
\alias{subset-methods}
\alias{subset,SimResults-method}

\title{Subsetting simulation results}
\description{
  Extract subsets of simulation results.
}
\usage{
\S4method{subset}{SimResults}(x, data = NULL, cont = NULL, miss = NULL, 
       select = NULL, \dots)
}
\arguments{
  \item{x}{the simulation results from which to extract subsets, i.e., an 
    object of class \code{"\linkS4class{SimResults}"}.}

  \item{data}{an optional integer or logical index vector specifying the data 
    configurations for which to keep the simulation results.}

  \item{cont}{an optional integer or logical index vector specifying the 
    contamination settings for which to keep the simulation results.}

  \item{miss}{an optional integer or logical index vector specifying the 
    missing data settings for which to keep the simulation results.}

  \item{select}{an optional integer, character or logical vector specifying the 
    columns to be extracted.  It must specify a subset of the \code{colnames} 
    slot of the simulation results, which is the default.}

  \item{\dots}{additional arguments are currently ignored.}
}
\value{
  An object similar to \code{x} containing just the selected results.
}
\section{Methods}{
  \describe{
  \item{\code{signature(x = "SimResults")}}{extract subsets of simulation 
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

## explore results
tv <- mean(eusilcP$eqIncome)  # true population mean
# all results
plot(results) + geom_hline(yintercept=tv)
# subset
sub <- subset(results, cont = c(1:2, 6))
plot(sub) + geom_hline(yintercept=tv)



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
plot(results) + geom_hline(aes(yintercept=mean), data=tv)
# subset
sub <- subset(results, cont = c(1:2, 6))
plot(sub) + geom_hline(aes(yintercept=mean), data=tv)
}
\keyword{methods}
\keyword{utilities}

