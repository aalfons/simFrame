\name{NAControl-class}
\Rdversion{1.1}
\docType{class}
\alias{NAControl-class}
\alias{NAControl}
\alias{getGrouping,NAControl-method}
\alias{setGrouping,NAControl-method}
\alias{getAux,NAControl-method}
\alias{setAux,NAControl-method}
\alias{getIntoContamination,NAControl-method}
\alias{setIntoContamination,NAControl-method}
\alias{show,NAControl-method}
%% aliases to avoid confusion due to capitalization
\alias{NAcontrol-class}
\alias{naControl-class}
\alias{nacontrol-class}
\alias{NAcontrol}
\alias{naControl}
\alias{nacontrol}

\title{Class "NAControl"}
\description{
Class for controlling the insertion of missing values in a simulation 
experiment.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("NAControl", \dots)} or 
\code{NAControl(\dots)}.
}
\section{Slots}{
  \describe{
    \item{\code{target}:}{Object of class \code{"OptCharacter"}; a character 
      vector specifying the variables (columns) in which missing values should 
      be inserted, or \code{NULL} to insert missing values in all variables 
      (except the additional ones generated internally).}
    \item{\code{NArate}:}{Object of class \code{"NumericMatrix"} giving the 
      missing value rates, which may be selected individually for the target 
      variables. In case of a vector, the same missing value rates are used for 
      all target variables. In case of a matrix, on the other hand, the missing 
      value rates to be used for each target variable are given by the 
      respective column.}
    \item{\code{grouping}:}{Object of class \code{"character"} specifying a 
      grouping variable (column) to be used for setting whole groups to 
      \code{NA} rather than individual values.}
    \item{\code{aux}:}{Object of class \code{"character"} specifying auxiliary 
      variables (columns) whose values are used as probability weights for 
      selecting the values to be set to \code{NA} in the respective target 
      variables.  If only one variable (column) is specified, it is used for 
      all target variables.}
    \item{\code{intoContamination}:}{Object of class \code{"logical"} 
      indicating whether missing values should also be inserted into 
      contaminated observations.  The default is to insert missing values only 
      into non-contaminated observations.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{VirtualNAControl}"}, directly.
Class \code{"\linkS4class{OptNAControl}"}, by class "VirtualNAControl", 
distance 2.
}
\section{Accessor and mutator methods}{
  In addition to the accessor and mutator methods for the slots inherited from 
  \code{"\linkS4class{VirtualNAControl}"}, the following are available:
  
  \describe{
    \item{\code{getGrouping}}{\code{signature(x = "NAControl")}: get slot 
      \code{grouping}.}
    \item{\code{setGrouping}}{\code{signature(x = "NAControl")}: set slot 
      \code{grouping}.}
    \item{\code{getAux}}{\code{signature(x = "NAControl")}: get slot 
      \code{aux}.}
    \item{\code{setAux}}{\code{signature(x = "NAControl")}: set slot 
      \code{aux}.}
    \item{\code{getIntoContamination}}{\code{signature(x = "NAControl")}: get 
      slot \code{intoContamination}.}
    \item{\code{setIntoContamination}}{\code{signature(x = "NAControl")}: set 
      slot \code{intoContamination}.}
  }
}
\section{Methods}{
  In addition to the methods inherited from 
  \code{"\linkS4class{VirtualNAControl}"}, the following are available:
  
  \describe{
    \item{\code{setNA}}{\code{signature(x = "data.frame", 
      control = "NAControl")}: set missing values.}
    \item{\code{show}}{\code{signature(object = "NAControl")}: print the object 
      on the \R console.}
  }
}
\section{UML class diagram}{
  A slightly simplified UML class diagram of the framework can be found in 
  Figure 1 of the package vignette \emph{An Object-Oriented Framework for 
  Statistical Simulation: The \R Package \code{simFrame}}.  Use 
  \code{vignette("simFrame-intro")} to view this vignette.
}
\note{
  Since version 0.3, this control class now allows to specify an auxiliary 
  variable with probability weights for each target variable.
  
  The slot \code{grouping} was named \code{group} prior to version 0.2.  
  Renaming the slot was necessary since accessor and mutator functions were 
  introduced in this version and a function named \code{getGroup} already 
  exists.
}
\author{Andreas Alfons}
\references{
Alfons, A., Templ, M. and Filzmoser, P. (2010) An Object-Oriented Framework for 
Statistical Simulation: The \R Package \pkg{simFrame}. \emph{Journal of 
Statistical Software}, \bold{37}(3), 1--36. URL 
\url{http://www.jstatsoft.org/v37/i03/}.
}
\seealso{
  \code{"\linkS4class{VirtualNAControl}"}, \code{\link{setNA}}
}
\examples{
data(eusilcP)
eusilcP$age[eusilcP$age < 0] <- 0  # this actually occurs
sam <- draw(eusilcP[, c("id", "age", "eqIncome")], size = 20)

## missing completely at random
mcarc <- NAControl(target = "eqIncome", NArate = 0.2)
setNA(sam, mcarc)

## missing at random
marc <- NAControl(target = "eqIncome", NArate = 0.2, aux = "age")
setNA(sam, marc)

## missing not at random
mnarc <- NAControl(target = "eqIncome", 
    NArate = 0.2, aux = "eqIncome")
setNA(sam, mnarc)
}
\keyword{classes}
