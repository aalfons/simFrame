\name{BasicContControl-class}
\Rdversion{1.1}
\docType{class}
\alias{BasicContControl-class}
\alias{BasicContControl}

\title{Class \code{"BasicContControl"}}
\description{
  Class for controlling contamination in a simulation experiment.  The values 
  of the first observations will either be \emph{contaminated completely at 
  random} (CCAR; i.e., not depending on the original values) or 
  \emph{contaminated at random} (CAR; i.e., depending on the original values).
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{BasicContControl(\dots)} or 
\code{ContControl(\dots, sub = "Basic")}.
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
Class \code{"\linkS4class{ContControl}"}, directly.
Class \code{"\linkS4class{VirtualContControl}"}, distance 2.
Class \code{"\linkS4class{OptContControl}"}, distance 3.
}
\section{Accessor methods}{
  Accessor methods are inherited.
}
\section{Methods}{
  The following methods are available in addition to the inherited ones:
  
  \describe{
    \item{\code{contaminate}}{\code{signature(x = "data.frame", 
      control = "BasicContControl")}: contaminate data.}
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
  \code{\link{contaminate}}
}
\examples{
x <- generate(size = 10)
cc <- ContControl(epsilon = 0.2, fun = function(x) x * 10,
    type = "CAR", sub = "Basic")
contaminate(x, cc)
}
\keyword{classes}
