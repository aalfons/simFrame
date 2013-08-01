\name{SampleControl}
\alias{SampleControl}
\title{Create sample control objects}
\usage{
  SampleControl(..., type = c("Basic", "TwoStage"))
}
\arguments{
  \item{\dots}{arguments passed to the constructor of the
  class determined by \code{type}.}

  \item{type}{a character string specifying whether a
  control object of class \code{"BasicSampleControl"} or
  \code{"TwoStageSampleControl"} should be created.}
}
\value{
  An object of class determined by \code{type}.
}
\description{
  Create objects of a class inheriting from
  \code{"VirtualSampleControl"}.
}
\examples{
data("eusilcP")
bsc <- SampleControl(size = 20, type = "Basic")
draw(eusilcP[, c("id", "eqIncome")], bsc)
}
\author{
  Andreas Alfons
}
\seealso{
  \code{"\linkS4class{BasicSampleControl}"},
  \code{"\linkS4class{TwoStageSampleControl}"}
}
\keyword{classes}

