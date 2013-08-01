\name{VirtualNAControl-class}
\Rdversion{1.1}
\docType{class}
\alias{VirtualNAControl-class}
\alias{getNARate,VirtualNAControl-method}
\alias{getTarget,VirtualNAControl-method}
\alias{show,VirtualNAControl-method}

\title{Class \code{"VirtualNAControl"}}
\description{
  Virtual superclass for controlling the insertion of missing values in a 
  simulation experiment.
}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}
\section{Slots}{
  \describe{
    \item{\code{target}:}{Object of class \code{"OptCharacter"}; a character 
      vector specifying the variables (columns) in which missing values should 
      be inserted, or \code{NULL} to insert missing values in all variables 
      (except the additional ones generated internally).}
    \item{\code{NARate}:}{Object of class \code{"NumericMatrix"} giving the 
      missing value rates, which may be selected individually for the target 
      variables. In case of a vector, the same missing value rates are used for 
      all target variables. In case of a matrix, on the other hand, the missing 
      value rates to be used for each target variable are given by the 
      respective column.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{OptNAControl}"}, directly.
}
\section{Accessor methods}{
  \describe{
    \item{\code{getTarget}}{\code{signature(x = "VirtualNAControl")}: get slot 
      \code{target}.}
    \item{\code{getNARate}}{\code{signature(x = "VirtualNAControl")}: get slot 
      \code{NARate}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{head}}{\code{signature(x = "VirtualNAControl")}: currently 
      returns the object itself.}
    \item{\code{length}}{\code{signature(x = "VirtualNAControl")}: get the 
      number of missing value rates to be used (the length in case of a vector 
      or the number of rows in case of a matrix).}
    \item{\code{show}}{\code{signature(object = "VirtualNAControl")}: print the 
      object on the \R console.}
    \item{\code{summary}}{\code{signature(object = "VirtualNAControl")}: 
      currently returns the object itself.}
    \item{\code{tail}}{\code{signature(x = "VirtualNAControl")}: currently 
      returns the object itself.}
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
  \code{"\linkS4class{NAControl}"}, \code{\link{setNA}}
}
\keyword{classes}
