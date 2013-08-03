\name{SummarySampleSetup-class}
\Rdversion{1.1}
\docType{class}
\alias{SummarySampleSetup-class}
\alias{SummarySampleSetup}
\alias{getSize,SummarySampleSetup-method}
\alias{show,SummarySampleSetup-method}

\title{Class \code{"SummarySampleSetup"}}
\description{
  Class containing a summary of set up samples.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{SummarySampleSetup(\dots)}.  
However, the constructor function is not supposed to be called directly by the 
user.

Instead, objects are expected to be created by the \code{summary} method for 
class \code{"\linkS4class{SampleSetup}"}. 
}
\section{Slots}{
  \describe{
    \item{\code{size}:}{A numeric vector giving the size of each of the set up 
      samples.}
  }
}
\section{Accessor methods}{
  \describe{
    \item{\code{getSize}}{\code{signature(x = "SummarySampleSetup")}: get slot 
      \code{size}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{show}}{\code{signature(object = "SummarySampleSetup")}: print a 
      summary of set up samples on the \R console.}
  }
}
\section{UML class diagram}{
  A slightly simplified UML class diagram of the framework can be found in 
  Figure 1 of the package vignette \emph{An Object-Oriented Framework for 
  Statistical Simulation: The \R Package \code{simFrame}}.  Use 
  \code{vignette("simFrame-intro")} to view this vignette.
}
\author{Andreas Alfons}
\seealso{
  \code{"\linkS4class{SampleSetup}"}, 
  \code{\link[simFrame:summary-methods]{summary}}
}
\keyword{classes}
