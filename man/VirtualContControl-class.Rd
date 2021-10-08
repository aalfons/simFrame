\name{VirtualContControl-class}
\Rdversion{1.1}
\docType{class}
\alias{VirtualContControl-class}
\alias{getTarget,VirtualContControl-method}
\alias{setTarget,VirtualContControl-method}
\alias{getEpsilon,VirtualContControl-method}
\alias{setEpsilon,VirtualContControl-method}
\alias{show,VirtualContControl-method}
%% aliases to avoid confusion due to capitalization
\alias{virtualContControl-class}
\alias{virtualcontControl-class}
\alias{virtualcontcontrol-class}
\alias{virtualContcontrol-class}
\alias{VirtualcontControl-class}
\alias{VirtualContcontrol-class}
\alias{Virtualcontcontrol-class}

\title{Class "VirtualContControl"}
\description{
  Virtual superclass for controlling contamination in a simulation experiment.
}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}
\section{Slots}{
  \describe{
    \item{\code{target}:}{Object of class \code{"OptCharacter"}; a character 
      vector specifying specifying the variables (columns) to be contaminated, 
      or \code{NULL} to contaminate all variables  (except the additional ones 
      generated internally).}
    \item{\code{epsilon}:}{Object of class \code{"numeric"} giving the 
      contamination levels.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{OptContControl}"}, directly.
}
\section{Accessor and mutator methods}{
  \describe{
    \item{\code{getTarget}}{\code{signature(x = "VirtualContControl")}: get 
      slot \code{target}.}
    \item{\code{setTarget}}{\code{signature(x = "VirtualContControl")}: set 
      slot \code{target}.}
    \item{\code{getEpsilon}}{\code{signature(x = "VirtualContControl")}: get 
      slot \code{epsilon}.}
    \item{\code{setEpsilon}}{\code{signature(x = "VirtualContControl")}: set 
      slot \code{epsilon}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{head}}{\code{signature(x = "VirtualContControl")}: currently 
      returns the object itself.}
    \item{\code{length}}{\code{signature(x = "VirtualContControl")}: get the 
      number of contamination levels to be used.}
    \item{\code{show}}{\code{signature(object = "VirtualContControl")}: print 
      the object on the \R console.}
    \item{\code{summary}}{\code{signature(object = "VirtualContControl")}: 
      currently returns the object itself.}
    \item{\code{tail}}{\code{signature(x = "VirtualContControl")}: currently 
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
  \code{"\linkS4class{DCARContControl}"}, \code{"\linkS4class{DARContControl}"}, 
  \code{"\linkS4class{ContControl}"}, \code{\link{contaminate}}
}
\examples{
showClass("VirtualContControl")
}
\keyword{classes}
