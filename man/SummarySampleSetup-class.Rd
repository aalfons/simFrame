\name{SummarySampleSetup-class}
\Rdversion{1.1}
\docType{class}
\alias{SummarySampleSetup-class}
\alias{SummarySampleSetup}
\alias{getSize,SummarySampleSetup-method}
\alias{show,SummarySampleSetup-method}
%% aliases to avoid confusion due to capitalization
\alias{summarySampleSetup-class}
\alias{summarysampleSetup-class}
\alias{summarysamplesetup-class}
\alias{summarySamplesetup-class}
\alias{SummarysampleSetup-class}
\alias{Summarysamplesetup-class}
\alias{SummarySamplesetup-class}
\alias{summarySampleSetup}
\alias{summarysampleSetup}
\alias{summarysamplesetup}
\alias{summarySamplesetup}
\alias{SummarysampleSetup}
\alias{Summarysamplesetup}
\alias{SummarySamplesetup}

\title{Class "SummarySampleSetup"}
\description{
  Class containing a summary of set up samples.
}
\section{Objects from the Class}{
Objects can be created by calls of the form 
\code{new("SummarySampleSetup", \dots)} or \code{SummarySampleSetup(\dots)}.  

However, objects are expected to be created by the \code{summary} method for 
class \code{"\linkS4class{SampleSetup}"}, these constructor functions are not 
supposed to be called by the user. 
}
\section{Slots}{
  \describe{
    \item{\code{size}:}{Object of class \code{"numeric"} giving the size of 
      each of the set up samples.}
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
\note{
  There are no mutator methods available since the slots are not supposed to 
  be changed by the user.
}
\author{Andreas Alfons}
\seealso{
  \code{"\linkS4class{SampleSetup}"}, 
  \code{\link[simFrame:summary-methods]{summary}}
}
\examples{
showClass("SummarySampleSetup")
}
\keyword{classes}
