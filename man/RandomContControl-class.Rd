\name{RandomContControl-class}
\Rdversion{1.1}
\docType{class}
\alias{RandomContControl-class}
\alias{contaminate,data.frame,RandomContControl-method}
\alias{getAux,RandomContControl-method}
\alias{getGrouping,RandomContControl-method}
\alias{show,RandomContControl-method}

\title{Class \code{"RandomContControl"}}
\description{
  Virtual class for controlling contamination in a simulation experiment (used 
  internally).
}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}
\section{Slots}{
  \describe{
    \item{\code{target}:}{Object of class \code{"OptCharacter"}; a character 
      vector specifying specifying the variables (columns) to be contaminated, 
      or \code{NULL} to contaminate all variables (except the additional ones 
      generated internally).}
    \item{\code{epsilon}:}{Object of class \code{"numeric"} giving the 
      contamination levels.}
    \item{\code{tuning}:}{Object of class \code{"ListOrDataFrame"} giving 
      tuning parameters for the contamination.  Lists are converted to data 
      frames with \code{\link{expand.grid}} during initialization.}
    \item{\code{indices}:}{Object of class \code{"NumericMatrix"} in which the 
      first column contains the indices of the contamination levels and the 
      second column contains the row indices of the tuning parameters for the 
      different contamination settings.}
    \item{\code{grouping}:}{Object of class \code{"character"} specifying a 
      grouping variable (column) to be used for contaminating whole groups 
      rather than individual observations.}
    \item{\code{aux}:}{Object of class \code{"character"} specifying an 
      auxiliary variable (column) whose values are used as probability weights 
      for selecting the items (observations or groups) to be contaminated.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{ContControl}"}, directly.
Class \code{"\linkS4class{VirtualContControl}"}, distance 2.
Class \code{"\linkS4class{OptContControl}"}, distance 3.
}
\section{Accessor methods}{
  The following accessor methods are available in addition to the inherited 
  ones:
  
  \describe{
    \item{\code{getGrouping}}{\code{signature(x = "RandomContControl")}: get 
      slot \code{grouping}.}
    \item{\code{getAux}}{\code{signature(x = "RandomContControl")}: get slot 
      \code{aux}.}
  }
}
\section{Methods}{
  The following methods are available in addition to the inherited ones:
  
  \describe{
    \item{\code{contaminate}}{\code{signature(x = "data.frame", 
      control = "RandomContControl")}: contaminate data.}
    \item{\code{show}}{\code{signature(object = "RandomContControl")}: print 
      the object on the \R console.}
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
  \code{"\linkS4class{CCARContControl}"}, \code{"\linkS4class{CARContControl}"}, 
  \code{\link{contaminate}}
}
\keyword{classes}
