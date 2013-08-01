\name{ContControl-class}
\Rdversion{1.1}
\docType{class}
\alias{ContControl-class}
\alias{getIndices,ContControl-method}
\alias{getTuning,ContControl-method}
\alias{length,ContControl-method}
\alias{show,ContControl-method}

\title{Class \code{"ContControl"}}
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
  \code{"\linkS4class{CCARContControl}"}, 
  \code{"\linkS4class{CARContControl}"}, 
  \code{\link{contaminate}}
}
\keyword{classes}
