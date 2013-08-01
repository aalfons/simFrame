\name{draw}
\docType{methods}
\alias{draw}
\alias{draw-methods}
\alias{draw,data.frame,character-method}
\alias{draw,data.frame,missing-method}
\alias{draw,data.frame,SampleSetup-method}
\alias{draw,data.frame,VirtualSampleControl-method}

\title{Draw a sample}
\description{
Generic function for drawing a sample.
}
\usage{
draw(x, setup, \dots)

\S4method{draw}{data.frame,SampleSetup}(x, setup, i = 1)
}
\arguments{
  \item{x}{the data to sample from.}
  \item{setup}{an object of class \code{"SampleSetup"} containing previously 
    set up samples, a control object inheriting from the virtual class 
    \code{"VirtualSampleControl"} or a character string specifying such a 
    control class.  If missing, the constructor \code{\link{SampleControl}} is 
    called to generate the control object.}
  \item{i}{an integer specifying which one of the previously set up samples 
    should be drawn.}
  \item{\dots}{if \code{setup} is a character string or missing, the slots of 
    the control object may be supplied as additional arguments.  See 
    \code{"\linkS4class{BasicSampleControl}"} and 
    \code{"\linkS4class{TwoStageSampleControl}"} for details on the slots.}
}
\value{
  A \code{data.frame} containing the sampled observations.  In addition, the 
  column \code{".weight"}, which consists of the sample weights, is added to 
  the \code{data.frame}.
}
\section{Methods}{
  \describe{
  \item{\code{x = "data.frame", setup = "character"}}{draw a sample using a 
    control class specified by the character string \code{setup}.  The slots of 
    the control object may be supplied as additional arguments.}
  \item{\code{x = "data.frame", setup = "missing"}}{draw a sample using a 
    control object constructed by \code{\link{SampleControl}}.  Its slots may 
    be supplied as additional arguments.}
  \item{\code{x = "data.frame", setup = "SampleSetup"}}{draw a previously set 
    up sample.}
  \item{\code{x = "data.frame", setup = "VirtualSampleControl"}}{draw a sample 
    using a control object inheriting from the virtual class 
    \code{"VirtualSampleControl"}.}
  }
}
\author{Andreas Alfons}
\references{
Alfons, A., Templ, M. and Filzmoser, P. (2010) An Object-Oriented Framework for 
Statistical Simulation: The \R Package \pkg{simFrame}. \emph{Journal of 
Statistical Software}, \bold{37}(3), 1--36. URL 
\url{http://www.jstatsoft.org/v37/i03/}.
}
\seealso{
  \code{\link{setup}}, 
  \code{"\linkS4class{SampleSetup}"}, 
  \code{"\linkS4class{BasicSampleControl}"}, 
  \code{"\linkS4class{TwoStageSampleControl}"}
}
\examples{
## load data
data("eusilcP")

## simple random sampling
draw(eusilcP[, c("id", "eqIncome")], size = 20, seed = 12345)

## group sampling
draw(eusilcP[, c("hid", "id", "eqIncome")], grouping = "hid", 
    size = 10, seed = 12345)

## stratified simple random sampling
draw(eusilcP[, c("id", "region", "eqIncome")], design = "region", 
    size = c(2, 5, 5, 3, 4, 5, 3, 5, 2), seed = 12345)

## stratified group sampling
draw(eusilcP[, c("hid", "id", "region", "eqIncome")], 
    design = "region", grouping = "hid", 
    size = c(2, 5, 5, 3, 4, 5, 3, 5, 2), 
    seed = 12345)
}
\keyword{distribution}
\keyword{methods}
