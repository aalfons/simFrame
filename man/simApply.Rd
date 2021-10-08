\name{simApply}
\Rdversion{1.1}
\docType{methods}
\alias{simApply}
\alias{simApply-methods}
\alias{simApply,data.frame,BasicVector,function-method}
\alias{simApply,data.frame,Strata,function-method}
\alias{simSapply}
\alias{simSapply-methods}
\alias{simSapply,data.frame,BasicVector,function-method}
\alias{simSapply,data.frame,Strata,function-method}
%% aliases to avoid confusion due to capitalization
\alias{simapply}
\alias{Simapply}
\alias{SimApply}
\alias{simapply-methods}
\alias{Simapply-methods}
\alias{SimApply-methods}
\alias{simsapply}
\alias{Simsapply}
\alias{SimSapply}
\alias{simsapply-methods}
\alias{Simsapply-methods}
\alias{SimSapply-methods}

\title{Apply a function to subsets}
\description{
Generic functions for applying a function to subsets of a data set.
}
\usage{
simApply(x, design, fun, \dots)

simSapply(x, design, fun, \dots, simplify = TRUE)
}
\arguments{
  \item{x}{the \code{data.frame} to be subsetted.}
  \item{design}{a character, logical or numeric vector specifying the variables 
    (columns) used for subsetting.}
  \item{fun}{a function to be applied to the subsets.}
  \item{simplify}{a logical indicating whether the results should be 
    simplified to a vector or matrix (if possible).}
  \item{\dots}{additional arguments to be passed to \code{fun}.}
}
\value{
  For \code{simApply} a \code{data.frame}.
  
  For \code{simSapply}, a list, vector or matrix (see \code{\link{sapply}}).
}
\section{Methods for function simApply}{
  \describe{
  \item{\code{x = "data.frame", design = "BasicVector", fun = "function"}}{apply 
    a function to subsets given by the variables (columns) in \code{design}.}
  \item{\code{x = "data.frame", design = "Strata", fun = "function"}}{apply a 
    function to subsets given by \code{design}.}
  }
}
\section{Methods for function simSapply}{
  \describe{
  \item{\code{x = "data.frame", design = "BasicVector", fun = "function"}}{apply 
    a function to subsets given by the variables (columns) in \code{design}.}
  \item{\code{x = "data.frame", design = "Strata", fun = "function"}}{apply a 
    function to subsets given by \code{design}.}
  }
}
\author{Andreas Alfons}
\seealso{
    \code{\link{sapply}}
}
\examples{
data(eusilcP)
eusilcP <- eusilcP[, c("region", "gender", "eqIncome")]

## returns data.frame
simApply(eusilcP, c("region", "gender"), 
    function(x) median(x$eqIncome))

## returns vector
simSapply(eusilcP, c("region", "gender"), 
    function(x) median(x$eqIncome))
}
\keyword{iteration}
\keyword{methods}
