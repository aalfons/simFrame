\name{SampleControl}
\alias{SampleControl}
\title{Create sample control objects}
\usage{
  SampleControl(..., sub = c("Basic", "TwoStage"))
}
\arguments{
  \item{\dots}{arguments passed to the constructor of the
  class determined by \code{sub}.}

  \item{sub}{a character string specifying whether to
  create a control object of the subclass
  \code{"\linkS4class{BasicSampleControl}"} or
  \code{"\linkS4class{TwoStageSampleControl}"}.}
}
\value{
  An object of class determined by \code{sub}.
}
\description{
  Create objects of a class inheriting from
  \code{"\linkS4class{SampleControl}"}.
}
\examples{
data("eusilcP")
bsc <- SampleControl(size = 20, sub = "Basic")
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

