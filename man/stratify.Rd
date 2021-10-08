\name{stratify}
\Rdversion{1.1}
\docType{methods}
\alias{stratify}
\alias{stratify-methods}
\alias{stratify,data.frame,BasicVector-method}

\title{Stratify data}
\description{Generic function for stratifying data.}
\usage{
stratify(x, design)
}
\arguments{
  \item{x}{the \code{data.frame} to be stratified.}
  \item{design}{a character, logical or numeric vector specifying the variables 
    (columns) to be used for stratification.}
}
\value{
  An object of class \code{"Strata"}.
}
\section{Methods}{
  \describe{
  \item{\code{x = "data.frame", design = "BasicVector"}}{stratify data 
    according to the variables (columns) given by \code{design}.}
  }
}
\author{Andreas Alfons}
\seealso{
  \code{"\linkS4class{Strata}"}
}
\examples{
data(eusilcP)
strata <- stratify(eusilcP, c("region", "gender"))
summary(strata)
}
\keyword{manip}
\keyword{methods}
