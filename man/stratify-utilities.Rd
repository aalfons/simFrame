\name{stratify-utilities}
\Rdversion{1.1}
\docType{methods}
\alias{getStrataLegend}
\alias{getStrataLegend-methods}
\alias{getStrataLegend,data.frame,BasicVector-method}
\alias{getStrataSplit}
\alias{getStrataSplit-methods}
\alias{getStrataSplit,data.frame,BasicVector-method}
\alias{getStrataTable}
\alias{getStrataTable-methods}
\alias{getStrataTable,data.frame,BasicVector-method}
\alias{getStratumSizes}
\alias{getStratumSizes-methods}
\alias{getStratumSizes,list,missing-method}
\alias{getStratumSizes,data.frame,BasicVector-method}
\alias{getStratumValues}
\alias{getStratumValues-methods}
\alias{getStratumValues,data.frame,BasicVector,list-method}
\alias{getStratumValues,data.frame,BasicVector,missing-method}

\title{Utility functions for stratifying data}
\description{
Generic utility functions for stratifying data. These are useful if not all the 
information of class \code{"Strata"} is necessary.
}
\usage{
getStrataLegend(x, design)

getStrataSplit(x, design, USE.NAMES = TRUE)

getStrataTable(x, design)

getStratumSizes(x, design, USE.NAMES = TRUE)

getStratumValues(x, design, split)
}
\arguments{
  \item{x}{the \code{data.frame} to be stratified. For \code{getStratumSizes}, 
    it is also possible to supply a list in which each list element contains 
    the indices of the observations belonging to the corresponding stratum (as 
    returned by \code{getStrataSplit}).}
  \item{design}{a character, logical or numeric vector specifying the variables 
    (columns) to be used for stratification.}
  \item{USE.NAMES}{a logical indicating whether information about the strata 
    should be used as \code{names} for the result.}
  \item{split}{an optional list in which each list element contains the indices 
    of the observations belonging to the corresponding stratum (as returned by 
    \code{getStrataSplit}).}
}
\value{
  For \code{getStrataLegend}, a \code{data.frame} describing the strata.
  
  For \code{getStrataSplit}, a list in which each element contains the 
  indices of the observations belonging to the corresponding stratum.
  
  For \code{getStrataTable}, a \code{data.frame} describing the strata 
  and containing the stratum sizes.
  
  For \code{getStratumSizes}, a numeric vector of the stratum sizes.
  
  For \code{getStratumValues}, a numeric vector giving the stratum number for 
  each observation.
}
\section{Methods for function getStrataLegend}{
  \describe{
  \item{x = "data.frame", design = "BasicVector"}{get a \code{data.frame} 
    describing the strata, according to the variables specified by 
    \code{design}.}
  }
}
\section{Methods for function getStrataSplit}{
  \describe{
  \item{x = "data.frame", design = "BasicVector"}{get a list in which each 
    element contains the indices of the observations belonging to the 
    corresponding stratum, according to the variables specified by 
    \code{design}.}
  }
}
\section{Methods for function getStrataTable}{
  \describe{
  \item{x = "data.frame", design = "BasicVector"}{get a \code{data.frame} 
    describing the strata and containing the stratum sizes, according to the 
    variables specified by \code{design}.}
  }
}
\section{Methods for function getStratumSizes}{
  \describe{
  \item{x = "list", design = "missing"}{get the stratum sizes for a list in 
    which each list element contains the indices of the observations belonging 
    to the corresponding stratum (as returned by \code{getStrataSplit}).}
  \item{x = "data.frame", design = "BasicVector"}{get the stratum sizes of a 
    data set, according to the variables specified by \code{design}.}
  }
}
\section{Methods for function getStratumValues}{
  \describe{
  \item{x = "data.frame", design = "BasicVector", split = "list"}{get the 
    stratum number for each observation, according to the variables specified 
    by \code{design}.  A previously computed list in which each list element 
    contains the indices of the observations belonging to the corresponding 
    stratum (as returned by \code{getStrataSplit}) speeds things up a bit.}
  \item{x = "data.frame", design = "BasicVector", split = "missing"}{get the 
    stratum number for each observation, according to the variables specified 
    by \code{design}.}
  }
}
\author{Andreas Alfons}
\seealso{
  \code{\link{stratify}}, \code{\linkS4class{Strata}}
}
\examples{
data(eusilcP)

## all data
getStrataLegend(eusilcP, c("region", "gender"))
getStrataTable(eusilcP, c("region", "gender"))
getStratumSizes(eusilcP, c("region", "gender"))

## small sample
sam <- draw(eusilcP, size = 25)
getStrataSplit(sam, "gender")
getStratumValues(sam, "gender")
}
\keyword{manip}
\keyword{methods}
