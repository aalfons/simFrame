\encoding{utf8}
\name{contaminate}
\docType{methods}
\alias{contaminate}
\alias{contaminate-methods}
\alias{contaminate,data.frame,BasicContControl-method}
\alias{contaminate,data.frame,character-method}
\alias{contaminate,data.frame,missing-method}
\alias{contaminate,data.frame,RandomContControl-method}

\title{Contaminate data}
\description{Generic function for contaminating data.}
\usage{
contaminate(x, control, \dots)

\S4method{contaminate}{data.frame,BasicContControl}(x, control, i)

\S4method{contaminate}{data.frame,RandomContControl}(x, control, i)
}
\arguments{
  \item{x}{the data to be contaminated.}
  \item{control}{a control object of a class inheriting from the virtual class 
    \code{"VirtualContControl"} or a character string specifying such a control 
    class.  If missing, the constructor \code{\link{ContControl}} is called to 
    generate the control object.}
  \item{i}{an integer giving the index of the contamination setting to be used.}
  \item{\dots}{if \code{control} is a character string or missing, the slots of 
    the control object may be supplied as additional arguments.  See 
    \code{"\linkS4class{BasicContControl}"}, 
    \code{"\linkS4class{CCARContControl}"} and 
    \code{"\linkS4class{CARContControl}"} for details on the slots.}
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
  \item{\code{x = "data.frame", control = "missing"}}{contaminate data using a 
    control object constructed by \code{\link{ContControl}}.  Its slots may be 
    supplied as additional arguments.}
  \item{\code{x = "data.frame", control = "BasicContControl"}}{contaminate data 
    as defined by the control object \code{control}.}
  \item{\code{x = "data.frame", control = "RandomContControl"}}{contaminate 
    data as defined by the control object \code{control}.}
  }
}
\author{Andreas Alfons}
\references{
Alfons, A., Templ, M. and Filzmoser, P. (2010) An Object-Oriented Framework for 
Statistical Simulation: The \R Package \pkg{simFrame}. \emph{Journal of 
Statistical Software}, \bold{37}(3), 1--36. URL 
\url{http://www.jstatsoft.org/v37/i03/}.

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
  \code{"\linkS4class{BasicContControl}"}, 
  \code{"\linkS4class{CCARContControl}"}, 
  \code{"\linkS4class{CARContControl}"}
}
\examples{
## contaminated at random
library("mvtnorm")
mean <- rep(0, 2)
sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
foo <- generate(size = 10, distribution = rmvnorm,
    dots = list(mean = mean, sigma = sigma))
# contaminating the first observations
bcc <- ContControl(target = "V2", epsilon = 0.2,
    fun = function(x) x * 10, type = "Basic")
contaminate(foo, bcc)
# randomly selecting observations to be contaminated
carc <- ContControl(target = "V2", epsilon = 0.2,
    fun = function(x) x * 10, type = "CAR")
contaminate(foo, carc)

## contaminated completely at random
data("eusilcP")
sam <- draw(eusilcP[, c("id", "eqIncome")], size = 20)
# using a control object
ccarc <- ContControl(target = "eqIncome", epsilon = 0.05,
    dots = list(mean = 5e+05, sd = 10000), type = "CCAR")
contaminate(sam, ccarc)
# supply slots of control object as arguments
contaminate(sam, target = "eqIncome", epsilon = 0.05, 
    dots = list(mean = 5e+05, sd = 10000), type = "CCAR")
}
\keyword{manip}
\keyword{methods}
