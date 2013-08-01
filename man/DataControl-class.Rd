\name{DataControl-class}
\Rdversion{1.1}
\docType{class}
\alias{DataControl-class}
\alias{DataControl}
\alias{getColnames,DataControl-method}
\alias{getDistribution,DataControl-method}
\alias{getDots,DataControl-method}
\alias{getIndices,DataControl-method}
\alias{getTuning,DataControl-method}
\alias{show,DataControl-method}

\title{Class \code{"DataControl"}}
\description{Class for controlling model-based generation of data.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{DataControl(\dots)}.
}
\section{Slots}{
  \describe{
    \item{\code{size}:}{Object of class \code{"numeric"} giving the number of 
      observations to be generated.}
    \item{\code{tuning}:}{Object of class \code{"ListOrDataFrame"} giving 
      tuning parameters for data generation.  Lists are converted to data 
      frames with \code{\link{expand.grid}} during initialization.}
    \item{\code{indices}:}{Object of class \code{"NumericMatrix"} in which the 
      first column contains the indices of the data sizes and the second column 
      contains the row indices of the tuning parameters for the different data 
      configurations.}
    \item{\code{distribution}:}{Object of class \code{"function"} generating 
      the data, e.g., \code{\link[stats:Normal]{rnorm}} (the default) or 
      \code{\link[mvtnorm:Mvnorm]{rmvnorm}}.  It should take a positive integer 
      as its first argument, giving the number of observations to be generated, 
      and return an object that can be coerced to a \code{data.frame}.}
    \item{\code{dots}:}{Object of class \code{"list"} containing additional 
      arguments to be passed to \code{distribution}.}
    \item{\code{colnames}:}{Object of class \code{"OptCharacter"} ; a character 
      vector to be used as column names for the generated \code{data.frame}, or 
      \code{NULL}.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{VirtualDataControl}"}, directly.
Class \code{"\linkS4class{OptDataControl}"}, distance 2.
}
\section{Accessor methods}{
  The following accessor methods are available in addition to the inherited 
  ones:
  
  \describe{
    \item{\code{getTuning}}{\code{signature(x = "DataControl")}: get slot 
      \code{tuning}.}
    \item{\code{getIndices}}{\code{signature(x = "DataControl")}: get slot 
      \code{indices}.}
    \item{\code{getDistribution}}{\code{signature(x = "DataControl")}: get slot 
      \code{distribution}.}
    \item{\code{getDots}}{\code{signature(x = "DataControl")}: get slot 
      \code{dots}.}
    \item{\code{getColnames}}{\code{signature(x = "DataControl")}: get slot 
      \code{colnames}.}
  }
}
\section{Methods}{
  The following methods are available in addition to the inherited ones:
  
  \describe{
    \item{\code{generate}}{\code{signature(control = "DataControl")}: generate 
      data.}
    \item{\code{length}}{\code{signature(x = "DataControl")}: get the number 
      of different data configurations to be used.}
    \item{\code{show}}{\code{signature(object = "DataControl")}: print the 
      object on the \R console.}
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
Statistical Software}, \bold{37}(3), 1--36. URL 
\url{http://www.jstatsoft.org/v37/i03/}.
}
\seealso{
  \code{\link{generate}}
}
\examples{
library("mvtnorm")
mean <- rep(0, 2)
sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
dc <- DataControl(size = 10, distribution = rmvnorm, 
    dots = list(mean = mean, sigma = sigma))
generate(dc)
}
\keyword{classes}
