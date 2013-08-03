\name{ContControl-class}
\Rdversion{1.1}
\docType{class}
\alias{ContControl-class}
\alias{getDots,ContControl-method}
\alias{getFun,ContControl-method}
\alias{getIndices,ContControl-method}
\alias{getTuning,ContControl-method}
\alias{getType,ContControl-method}
\alias{show,ContControl-method}

\title{Class \code{"ContControl"}}
\description{
  Virtual class for controlling contamination in a simulation experiment. It is 
  used internally and is not expected to be extended by users.
}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}
\section{Slots}{
  \describe{
    \item{\code{target}:}{A character vector specifying specifying the 
      variables (columns) to be contaminated, or \code{NULL} to contaminate 
      all variables (except additional ones generated internally).}
    \item{\code{epsilon}:}{A numeric vector giving the contamination levels.}
    \item{\code{tuning}:}{A data frame giving tuning parameters for the 
      contamination.  A list is converted to a data frame during initialization 
      via \code{\link{expand.grid}}.}
    \item{\code{indices}:}{An integer matrix in which the first column contains 
      the indices of the contamination levels and the second column contains 
      the row indices of the tuning parameters for the different contamination 
      settings.}
    \item{\code{fun}:}{A function generating the values of the contaminated 
      data.  As its first argument, it should take either the number of 
      observations to be created (if \code{type} is \code{"CCAR"}) or the 
      original values of the observations to be contaminated (if \code{type} 
      is \code{"CAR"}).  Furthermore, it should return an object that can be 
      coerced to a data frame, containing the contaminated data.}
    \item{\code{dots}:}{A list containing additional arguments to be passed to 
      \code{fun}.}
    \item{\code{type}:}{A character string specifying whether observations 
      should be contaminated completely at random (\code{"CCAR"}) or 
      contaminated at random (\code{"CAR"}).}
  }
}
\section{Extends}{
Class \code{"\linkS4class{VirtualContControl}"}, directly.
Class \code{"\linkS4class{OptContControl}"}, distance 2.
}
\section{Accessor methods}{
  The following accessor methods are available in addition to the inherited 
  ones:
  
  \describe{
    \item{\code{getTuning}}{\code{signature(x = "ContControl")}: get slot 
      \code{tuning}.}
    \item{\code{getIndices}}{\code{signature(x = "ContControl")}: get slot 
      \code{indices}.}
    \item{\code{getFun}}{\code{signature(x = "ContControl")}: get slot 
      \code{fun}.}
    \item{\code{getDots}}{\code{signature(x = "ContControl")}: get slot 
      \code{dots}.}
    \item{\code{getType}}{\code{signature(x = "ContControl")}: get slot 
      \code{type}.}
  }
}
\section{Methods}{
  The following methods are available in addition to the inherited ones:
  
  \describe{
    \item{\code{length}}{\code{signature(x = "ContControl")}: get the number 
    of different contamination settings to be used.}
    \item{\code{show}}{\code{signature(object = "ContControl")}: print the 
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
  \code{"\linkS4class{BasicContControl}"}, 
  \code{"\linkS4class{RandomContControl}"}, 
  \code{\link{contaminate}}
}
\keyword{classes}
