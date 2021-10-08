\encoding{utf8}
\name{contaminate}
\Rdversion{1.1}
\docType{methods}
\alias{contaminate}
\alias{contaminate-methods}
\alias{contaminate,data.frame,character-method}
\alias{contaminate,data.frame,ContControl-method}
\alias{contaminate,data.frame,missing-method}

\title{Contaminate data}
\description{Generic function for contaminating data.}
\usage{
contaminate(x, control, \dots)

\S4method{contaminate}{data.frame,ContControl}(x, control, i)
}
\arguments{
  \item{x}{the data to be contaminated.}
  \item{control}{a control object of a class inheriting from the virtual class
    \code{"VirtualContControl"} or a character string specifying such a control
    class (the default being \code{"DCARContControl"}).}
  \item{i}{an integer giving the element of the slot \code{epsilon} of
    \code{control} to be used as contamination level.}
  \item{\dots}{if \code{control} is a character string or missing, the slots of
    the control object may be supplied as additional arguments.  See
    \code{"\linkS4class{DCARContControl}"} and
    \code{"\linkS4class{DARContControl}"} for details on the slots.}
}
\details{
  With the control classes implemented in \pkg{simFrame}, contamination is
  modeled as a two-step process.  The first step is to select observations to
  be contaminated, the second is to model the distribution of the outliers.

  In order to extend the framework by a user-defined control class
  \code{"MyContControl"} (which must extend
  \code{"\linkS4class{VirtualContControl}"}), a method
  \code{contaminate(x, control, i)} with signature
  \code{'data.frame, MyContControl'} needs to be implemented.  In case the
  contaminated observations need to be identified at a later stage of the
  simulation, e.g., if conflicts with inserting missing values should be
  avoided, a logical indicator variable \code{".contaminated"} should be added
  to the returned data set.
}
\value{
  A \code{data.frame} containing the contaminated data.  In addition, the
  column \code{".contaminated"}, which consists of logicals indicating the
  contaminated observations, is added to the \code{data.frame}.
}
\section{Methods}{
  \describe{
  \item{\code{x = "data.frame", control = "character"}}{contaminate data using
    a control class specified by the character string \code{control}.  The
    slots of the control object may be supplied as additional arguments.}
  \item{\code{x = "data.frame", control = "ContControl"}}{contaminate data as
    defined by the control object \code{control}.}
  \item{\code{x = "data.frame", control = "missing"}}{contaminate data using a
    control object of class \code{"ContControl"}.  Its slots may be supplied as
    additional arguments.}
  }
}
\note{
Since version 0.3, \code{contaminate} no longer checks if the auxiliary
variable with probability weights are numeric and contain only finite positive
values (\code{\link{sample}} still throws an error in these cases).  This has
been removed to improve computational performance in simulation studies.
}
\author{Andreas Alfons}
\references{
Alfons, A., Templ, M. and Filzmoser, P. (2010) An Object-Oriented Framework for
Statistical Simulation: The \R Package \pkg{simFrame}. \emph{Journal of
Statistical Software}, \bold{37}(3), 1--36. \doi{10.18637/jss.v037.i03}.

Alfons, A., Templ, M. and Filzmoser, P. (2010) Contamination Models in the \R
Package \pkg{simFrame} for Statistical Simulation. In Aivazian, S., Filzmoser,
P. and Kharin, Y. (editors) \emph{Computer Data Analysis and Modeling: Complex
Stochastic Data and Systems}, volume 2, 178--181. Minsk. ISBN 978-985-476-848-9.

\enc{Béguin}{Beguin}, C. and Hulliger, B. (2008) The BACON-EEM Algorithm for
Multivariate Outlier Detection in Incomplete Survey Data. \emph{Survey
Methodology}, \bold{34}(1), 91--103.

Hulliger, B. and Schoch, T. (2009) Robust Multivariate Imputation with Survey
Data. \emph{57th Session of the International Statistical Institute}, Durban.
}
\seealso{
  \code{"\linkS4class{DCARContControl}"}, \code{"\linkS4class{DARContControl}"},
  \code{"\linkS4class{ContControl}"}, \code{"\linkS4class{VirtualContControl}"}
}
\examples{
## distributed completely at random
data(eusilcP)
sam <- draw(eusilcP[, c("id", "eqIncome")], size = 20)

# using a control object
dcarc <- ContControl(target = "eqIncome", epsilon = 0.05,
    dots = list(mean = 5e+05, sd = 10000), type = "DCAR")
contaminate(sam, dcarc)

# supply slots of control object as arguments
contaminate(sam, target = "eqIncome", epsilon = 0.05,
    dots = list(mean = 5e+05, sd = 10000))


## distributed at random
foo <- generate(size = 10, distribution = rnorm,
    dots = list(mean = 0, sd = 2))

# using a control object
darc <- DARContControl(target = "V1",
    epsilon = 0.2, fun = function(x) x * 100)
contaminate(foo, darc)

# supply slots of control object as arguments
contaminate(foo, "DARContControl", target = "V1",
    epsilon = 0.2, fun = function(x) x * 100)
}
\keyword{manip}
\keyword{methods}
