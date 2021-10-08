\name{Strata-class}
\Rdversion{1.1}
\docType{class}
\alias{Strata-class}
\alias{Strata}
\alias{getValues,Strata-method}
\alias{getSplit,Strata-method}
\alias{getDesign,Strata-method}
\alias{getNr,Strata-method}
\alias{getLegend,Strata-method}
\alias{getSize,Strata-method}
\alias{getCall,Strata-method}
\alias{show,Strata-method}
%% aliases to avoid confusion due to capitalization
\alias{strata-class}
\alias{strata}

\title{Class "Strata"}
\description{Class containing strata information for a data set.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Strata", \dots)} or 
\code{Strata(\dots)}.

However, objects are expected to be created by the function 
\code{\link{stratify}}, these constructor functions are not supposed to be 
called by the user. 
}
\section{Slots}{
	 \describe{
    \item{\code{values}:}{Object of class \code{"integer"} giving the stratum 
        number for each observation.}
    \item{\code{split}:}{Object of class \code{"list"}; each list element 
        contains the indices of the observations belonging to the corresponding 
        stratum.}
    \item{\code{design}:}{Object of class \code{"character"} giving the 
        variables (columns) defining the strata.}
    \item{\code{nr}:}{Object of class \code{"integer"} giving the stratum 
        numbers.}
    \item{\code{legend}:}{Object of class \code{"data.frame"} describing the 
        strata.}
    \item{\code{size}:}{Object of class \code{"numeric"} giving the stratum 
        sizes.}
    \item{\code{call}:}{Object of class \code{"OptCall"}; the function call 
        used to stratify the data, or \code{NULL}.}
  }
}
\section{Accessor methods}{
  \describe{
    \item{\code{getValues}}{\code{signature(x = "Strata")}: get slot 
      \code{values}.}
    \item{\code{getSplit}}{\code{signature(x = "Strata")}:  get slot 
      \code{split}.}
    \item{\code{getDesign}}{\code{signature(x = "Strata")}: get slot 
      \code{design}.}
    \item{\code{getNr}}{\code{signature(x = "Strata")}: get slot \code{nr}.}
    \item{\code{getLegend}}{\code{signature(x = "Strata")}: get slot 
      \code{legend}.}
    \item{\code{getSize}}{\code{signature(x = "Strata")}: get slot \code{size}.}
    \item{\code{getCall}}{\code{signature(x = "Strata")}: get slot \code{call}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{head}}{\code{signature(x = "Strata")}: returns the first parts 
      of strata information.}
    \item{\code{show}}{\code{signature(object = "Strata")}: print strata 
      information on the \R console.}
    \item{\code{simApply}}{\code{signature(x = "data.frame", design = "Strata", 
      fun = "function")}: apply a function to subsets.}
    \item{\code{simSapply}}{\code{signature(x = "data.frame", design = "Strata", 
      fun = "function")}: apply a function to subsets.}
    \item{\code{summary}}{\code{signature(object = "Strata")}: produce a 
      summary of strata information.}
    \item{\code{tail}}{\code{signature(x = "Strata")}: returns the last parts 
      of strata information.}
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
  \code{\link{stratify}}
}
\examples{
showClass("Strata")
}
\keyword{classes}
