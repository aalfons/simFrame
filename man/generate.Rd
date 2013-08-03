\name{generate}
\docType{methods}
\alias{generate}
\alias{generate-methods}
\alias{generate,character-method}
\alias{generate,DataControl-method}
\alias{generate,missing-method}

\title{Generate data}
\description{
Generic function for generating data based on a (distribution) model.
}
\usage{
generate(control, \dots)

\S4method{generate}{DataControl}(control, i = 1)
}
\arguments{
  \item{control}{a control object inheriting from the virtual class 
    \code{"\linkS4class{VirtualDataControl}"} or a character string specifying 
    such a control class (the default being 
    \code{"\linkS4class{DataControl}"}).}
  \item{i}{an integer giving the index of the data configuration to be used.}
  \item{\dots}{if \code{control} is a character string or missing, the slots of 
    the control object may be supplied as additional arguments.  See 
    \code{"\linkS4class{DataControl}"} for details on the slots.}
}
\details{
  The control class \code{"DataControl"} is quite simple but general.  For 
  user-defined data generation, it often suffices to implement a function and 
  use it as slot \code{fun} in the \code{"DataControl"} object.  See 
  \code{"\linkS4class{DataControl}"} for some requirements for such a function.
  
  However, if more specialized data generation models are required, the 
  framework can be extended by defining a control class \code{"MyDataControl"} 
  extending \code{"\linkS4class{VirtualDataControl}"} and the corresponding 
  method \code{generate(control, i = 1)} with signature 
  \code{'MyDataControl'}.  It may also be necessary to implement a method 
  \code{length(x)} with signature \code{'MyDataControl'} that returns the 
  number of different data configurations.  If, e.g., a specific distribution 
  or mixture of distributions is frequently used in simulation experiments, a 
  distinct control class may be more convenient for the user.
}
\value{
  A data frame.
}
\section{Methods}{
  \describe{
  \item{\code{signature(control = "character")}}{generate data using a control 
    class specified by the character string \code{control}.  The slots of the 
    control object may be supplied as additional arguments.}
  \item{\code{signature(control = "missing")}}{generate data using a control 
    object of class \code{"\linkS4class{DataControl}"}.  Its slots may be 
    supplied as additional arguments.}
  \item{\code{signature(control = "DataControl")}}{generate data as defined by 
    the control object \code{control}.}
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
  \code{"\linkS4class{DataControl}"}
}
\examples{
library("mvtnorm")
mean <- rep(0, 2)
sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)

# using a control object
dc <- DataControl(size = 10, fun = rmvnorm, 
    dots = list(mean = mean, sigma = sigma))
generate(dc)

# supply slots of control object as arguments
generate(size = 10, fun = rmvnorm, 
    dots = list(mean = mean, sigma = sigma))
}
\keyword{distribution}
\keyword{methods}
