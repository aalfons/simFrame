\name{ContControl-class}
\Rdversion{1.1}
\docType{class}
\alias{ContControl-class}
\alias{getGrouping,ContControl-method}
\alias{setGrouping,ContControl-method}
\alias{getAux,ContControl-method}
\alias{setAux,ContControl-method}
\alias{show,ContControl-method}
%% aliases to avoid confusion due to capitalization
\alias{contControl-class}
\alias{contcontrol-class}
\alias{Contcontrol-class}

\title{Class "ContControl"}
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
    \item{\code{grouping}:}{Object of class \code{"character"} specifying a
      grouping variable (column) to be used for contaminating whole groups
      rather than individual observations.}
    \item{\code{aux}:}{Object of class \code{"character"} specifying an
      auxiliary variable (column) whose values are used as probability weights
      for selecting the items (observations or groups) to be contaminated.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{VirtualContControl}"}, directly.
Class \code{"\linkS4class{OptContControl}"}, by class "VirtualContControl",
distance 2.
}
\section{Accessor and mutator methods}{
  In addition to the accessor and mutator methods for the slots inherited from
  \code{"\linkS4class{VirtualContControl}"}, the following are available:

  \describe{
    \item{\code{getGrouping}}{\code{signature(x = "ContControl")}: get slot
      \code{grouping}.}
    \item{\code{setGrouping}}{\code{signature(x = "ContControl")}: set slot
      \code{grouping}.}
    \item{\code{getAux}}{\code{signature(x = "ContControl")}: get slot
      \code{aux}.}
    \item{\code{setAux}}{\code{signature(x = "ContControl")}: set slot
      \code{aux}.}
  }
}
\section{Methods}{
  In addition to the methods inherited from
  \code{"\linkS4class{VirtualContControl}"}, the following are available:

  \describe{
    \item{\code{contaminate}}{\code{signature(x = "data.frame",
      control = "ContControl")}: contaminate data.}
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
\note{
  The slot \code{grouping} was named \code{group} prior to version 0.2.
  Renaming the slot was necessary since accessor and mutator functions were
  introduced in this version and a function named \code{getGroup} already
  exists.
}
\author{Andreas Alfons}
\references{
Alfons, A., Templ, M. and Filzmoser, P. (2010) An Object-Oriented Framework for
Statistical Simulation: The \R Package \pkg{simFrame}. \emph{Journal of
Statistical Software}, \bold{37}(3), 1--36. \doi{10.18637/jss.v037.i03}.
}
\seealso{
  \code{"\linkS4class{DCARContControl}"}, \code{"\linkS4class{DARContControl}"},
  \code{"\linkS4class{VirtualContControl}"}, \code{\link{contaminate}}
}
\examples{
showClass("ContControl")
}
\keyword{classes}
