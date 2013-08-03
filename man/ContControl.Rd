\name{ContControl}
\alias{ContControl}
\title{Create contamination control objects}
\usage{
  ContControl(..., sub = c("Basic", "Random"))
}
\arguments{
  \item{\dots}{arguments passed to the constructor of the
  class determined by \code{sub}.}

  \item{sub}{a character string specifying whether to
  create a control object of the subclass
  \code{"\linkS4class{BasicContControl}"} or
  \code{"\linkS4class{RandomContControl}"}.}
}
\value{
  An object of class determined by \code{sub}.
}
\description{
  Create objects of a class inheriting from
  \code{"\linkS4class{ContControl}"}.
}
\examples{
# contaminate the first observations
x <- generate(size = 10)
bcc <- ContControl(epsilon = 0.2, fun = function(x) x * 10,
    type = "CAR", sub = "Basic")
contaminate(x, bcc)

# randomly select observations to be contaminated
data("eusilcP")
sam <- draw(eusilcP[, c("id", "eqIncome")], size = 20)
rcc <- ContControl(target = "eqIncome", epsilon = 0.1,
    dots = list(mean = 5e+05, sd = 10000),
    type = "CCAR", sub = "Random")
contaminate(sam, rcc)
}
\author{
  Andreas Alfons
}
\seealso{
  \code{"\linkS4class{BasicContControl}"},
  \code{"\linkS4class{RandomContControl}"}
}
\keyword{classes}

