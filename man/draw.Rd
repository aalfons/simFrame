\name{draw}
\Rdversion{1.1}
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

\S4method{draw}{data.frame,VirtualSampleControl}(x, setup)
}
\arguments{
  \item{x}{the data to sample from.}
  \item{setup}{an object of class \code{"SampleSetup"} containing previously
    set up samples, a control object inheriting from the virtual class
    \code{"VirtualSampleControl"} or a character string specifying such a
    control class (the default being \code{"SampleControl"}).}
  \item{i}{an integer specifying which one of the previously set up samples
    should be drawn.}
  \item{\dots}{if \code{setup} is a character string or missing, the slots of
    the control object may be supplied as additional arguments.  See
    \code{"\linkS4class{SampleControl}"} for details on the slots.}
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
    control object of class \code{"SampleControl"}.  Its slots may be supplied
    as additional arguments.}
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
Statistical Software}, \bold{37}(3), 1--36. \doi{10.18637/jss.v037.i03}.
}
\seealso{
  \code{\link{setup}}, \code{"\linkS4class{SampleSetup}"},
  \code{"\linkS4class{SampleControl}"}, \code{"\linkS4class{TwoStageControl}"},
  \code{"\linkS4class{VirtualSampleControl}"}
}
\examples{
## load data
data(eusilcP)

## simple random sampling
draw(eusilcP[, c("id", "eqIncome")], size = 20)

## group sampling
draw(eusilcP[, c("hid", "id", "eqIncome")],
    grouping = "hid", size = 10)

## stratified simple random sampling
draw(eusilcP[, c("id", "region", "eqIncome")],
    design = "region", size = c(2, 5, 5, 3, 4, 5, 3, 5, 2))

## stratified group sampling
draw(eusilcP[, c("hid", "id", "region", "eqIncome")],
    design = "region", grouping = "hid",
    size = c(2, 5, 5, 3, 4, 5, 3, 5, 2))
}
\keyword{distribution}
\keyword{methods}
