\name{BasicVector-class}
\Rdversion{1.1}
\docType{class}
\alias{BasicVector-class}
%% aliases to avoid confusion due to capitalization
\alias{basicVector-class}
\alias{basicvector-class}
\alias{Basicvector-class}

\title{Class "BasicVector"}
\description{Virtual class used internally for convenience.}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}
\section{Extends}{
Class \code{"\linkS4class{OptBasicVector}"}, directly.
}
\section{Methods}{
  \describe{
    \item{\code{getStrataLegend}}{\code{signature(x = "data.frame", 
      design = "BasicVector")}: get a \code{data.frame} describing the strata.}
    \item{\code{getStrataSplit}}{\code{signature(x = "data.frame", 
      design = "BasicVector")}: get a list in which each element contains the 
      indices of the observations belonging to the corresponding stratum.}
    \item{\code{getStrataTable}}{\code{signature(x = "data.frame", 
      design = "BasicVector")}: get a \code{data.frame} describing the strata 
      and containing the stratum sizes.}
    \item{\code{getStratumSizes}}{\code{signature(x = "data.frame", 
      design = "BasicVector")}: get the stratum sizes.}
    \item{\code{getStratumValues}}{\code{signature(x = "data.frame", 
      design = "BasicVector", split = "missing")}: get the stratum number for 
      each observation.}
    \item{\code{getStratumValues}}{\code{signature(x = "data.frame", 
      design = "BasicVector", split = "list")}: get the stratum number for 
      each observation.}
    \item{\code{simApply}}{\code{signature(x = "data.frame", 
      design = "BasicVector", fun = "function")}: apply a function to subsets.}
    \item{\code{simSapply}}{\code{signature(x = "data.frame", 
      design = "BasicVector", fun = "function")}: apply a function to subsets.}
    \item{\code{stratify}}{\code{signature(x = "data.frame", 
      design = "BasicVector")}: stratify data.}
  }
}
\section{UML class diagram}{
  A slightly simplified UML class diagram of the framework can be found in 
  Figure 1 of the package vignette \emph{An Object-Oriented Framework for 
  Statistical Simulation: The \R Package \code{simFrame}}.  Use 
  \code{vignette("simFrame-intro")} to view this vignette.
}
\author{Andreas Alfons}
\examples{
showClass("BasicVector")
}
\keyword{classes}
