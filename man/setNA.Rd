\name{setNA}
\Rdversion{1.1}
\docType{methods}
\alias{setNA}
\alias{setNA-methods}
\alias{setNA,data.frame,character-method}
\alias{setNA,data.frame,missing-method}
\alias{setNA,data.frame,NAControl-method}
%% aliases to avoid confusion due to capitalization
\alias{setna}
\alias{SetNA}
\alias{Setna}
\alias{setna-methods}
\alias{SetNA-methods}
\alias{Setna-methods}

\title{Set missing values}
\description{Generic function for inserting missing values into data.}
\usage{
setNA(x, control, \dots)

\S4method{setNA}{data.frame,NAControl}(x, control, i)
}
\arguments{
  \item{x}{the data in which missing values should be inserted.}
  \item{control}{a control object inheriting from the virtual class
    \code{"VirtualNAControl"} or a character string specifying such a control
    class (the default being \code{"NAControl"}).}
  \item{i}{an integer giving the element or row of the slot \code{NArate} of
    \code{control} to be used as missing value rate(s).}
  \item{\dots}{if \code{control} is a character string or missing, the slots of
    the control object may be supplied as additional arguments.  See
    \code{"\linkS4class{NAControl}"} for details on the slots.}
}
\details{
  In order to extend the framework by a user-defined control class
  \code{"MyNAControl"} (which must extend
  \code{"\linkS4class{VirtualNAControl}"}), a method
  \code{setNA(x, control, i)} with signature \code{'data.frame, MyNAControl'}
  needs to be implemented.
}
\value{
  A \code{data.frame} containing the data with missing values.
}
\section{Methods}{
  \describe{
  \item{\code{x = "data.frame", control = "character"}}{set missing values
    using a control class specified by the character string \code{control}.
    The slots of the control object may be supplied as additional arguments.}
  \item{\code{x = "data.frame", control = "missing"}}{set missing values using
    a control object of class \code{"NAControl"}.  Its slots may be supplied as
    additional arguments.}
  \item{\code{x = "data.frame", control = "NAControl"}}{set missing values as
    defined by the control object \code{control}.}
  }
}
\note{
Since version 0.3, \code{setNA} no longer checks if auxiliary variable(s)
with probability weights are numeric and contain only finite positive values
(\code{\link{sample}} still throws an error in these cases).  This has been
removed to improve computational performance in simulation studies.
}
\author{Andreas Alfons}
\references{
Alfons, A., Templ, M. and Filzmoser, P. (2010) An Object-Oriented Framework for
Statistical Simulation: The \R Package \pkg{simFrame}. \emph{Journal of
Statistical Software}, \bold{37}(3), 1--36. \doi{10.18637/jss.v037.i03}.
}
\seealso{
  \code{"\linkS4class{NAControl}"}, \code{"\linkS4class{VirtualNAControl}"}
}
\examples{
data(eusilcP)
eusilcP$age[eusilcP$age < 0] <- 0  # this actually occurs
sam <- draw(eusilcP[, c("id", "age", "eqIncome")], size = 20)


## using control objects
# missing completely at random
mcarc <- NAControl(target = "eqIncome", NArate = 0.2)
setNA(sam, mcarc)

# missing at random
marc <- NAControl(target = "eqIncome", NArate = 0.2, aux = "age")
setNA(sam, marc)

# missing not at random
mnarc <- NAControl(target = "eqIncome",
    NArate = 0.2, aux = "eqIncome")
setNA(sam, mnarc)


## supply slots of control object as arguments
# missing completely at random
setNA(sam, target = "eqIncome", NArate = 0.2)

# missing at random
setNA(sam, target = "eqIncome", NArate = 0.2, aux = "age")

# missing not at random
setNA(sam, target = "eqIncome", NArate = 0.2, aux = "eqIncome")
}
\keyword{manip}
\keyword{methods}
