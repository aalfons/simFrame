\name{RandomContControl-class}
\Rdversion{1.1}
\docType{class}
\alias{RandomContControl-class}
\alias{RandomContControl}
\alias{getAux,RandomContControl-method}
\alias{getGrouping,RandomContControl-method}
\alias{show,RandomContControl-method}

\title{Class \code{"RandomContControl"}}
\description{
  Class for controlling contamination in a simulation experiment.  The values 
  of randomly selected observations will either be \emph{contaminated 
  completely at random} (CCAR; i.e., not depending on the original values) or 
  \emph{contaminated at random} (CAR; i.e., depending on the original values).
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{RandomContControl(\dots)} or 
\code{ContControl(\dots, sub = "Random")}.
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
      items to be created (if \code{type} is \code{"CCAR"}) or the 
      original values of the observations to be contaminated (if \code{type} 
      is \code{"CAR"}).  Furthermore, it should return an object that can be 
      coerced to a data frame, containing the contaminated data.}
    \item{\code{dots}:}{A list containing additional arguments to be passed to 
      \code{fun}.}
    \item{\code{type}:}{A character string specifying whether items should be 
      contaminated completely at random (\code{"CCAR"}) or contaminated at 
      random (\code{"CAR"}).}
    \item{\code{grouping}:}{An optional character string specifying a grouping 
      variable (column) to be used for contaminating whole groups rather than 
      individual observations.}
    \item{\code{aux}:}{An optional character string an auxiliary variable 
      (column) whose values are used as probability weights for selecting the 
      items to be contaminated.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{ContControl}"}, directly.
Class \code{"\linkS4class{VirtualContControl}"}, distance 2.
Class \code{"\linkS4class{OptContControl}"}, distance 3.
}
\section{Details}{
  With this control class, contamination is modeled as a two-step process.  The 
  first step is to randomly select observations to be contaminated, the second 
  is to model the distribution of the outliers.
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
  \code{\link{contaminate}}
}
\examples{
data("eusilcP")
sam <- draw(eusilcP[, c("id", "eqIncome")], size = 20)
cc <- ContControl(target = "eqIncome", epsilon = 0.1,
    dots = list(mean = 5e+05, sd = 10000),
    type = "CCAR", sub = "Random")
contaminate(sam, cc)
}
\keyword{classes}
