\name{simSample}
\Rdversion{1.1}
\alias{simSample}
%% aliases to avoid confusion due to capitalization
\alias{simsample}
\alias{SimSample}
\alias{Simsample}

\title{Set up multiple samples}
\description{
A convenience wrapper for setting up multiple samples using \code{\link{setup}} 
with control class \code{\linkS4class{SampleControl}}.
}
\usage{
simSample(x, design = character(), grouping = character(), 
          collect = FALSE, fun = srs, size = NULL, 
          prob = NULL, \dots, k = 1)
}
\arguments{
  \item{x}{the \code{data.frame} to sample from.}
  \item{design}{a character, logical or numeric vector specifying variables 
    (columns) to be used for stratified sampling.}
  \item{grouping}{a character string, single integer or logical vector 
    specifying a grouping variable (column) to be used for sampling whole 
    groups rather than individual observations.}
  \item{collect}{logical; if  a grouping variable is specified and this is 
    \code{FALSE} (which is the default value), groups are sampled directly.  
    If grouping variable is specified and this is \code{TRUE}, individuals 
    are sampled in a first step.  In a second step, all individuals that 
    belong to the same group as any of the sampled individuals are collected 
    and added to the sample.  If no grouping variable is specified, this is 
    ignored.}
  \item{fun}{a function to be used for sampling (defaults to 
    \code{\link{srs}}).  It should return a vector containing the indices 
    of the sampled items (observations or groups).}
  \item{size}{an optional non-negative integer giving the number of items 
    (observations or groups) to sample.  For stratified sampling, 
    a vector of non-negative integers, each giving the number of items 
    to sample from the corresponding stratum.}
  \item{prob}{an optional numeric vector giving the probability weights, or a 
    character string or logical vector specifying a variable (column) that 
    contains the probability weights.}
  \item{\dots}{additional arguments to be passed to \code{fun}.}
  \item{k}{a single positive integer giving the number of samples to be set 
    up.}
}
\details{
  There are some restrictions on the argument names of the function 
  supplied to \code{fun}.  If it needs population data as input, 
  the corresponding argument should be called \code{x} and should expect 
  a \code{data.frame}.  If the sampling method only needs the population size 
  as input, the argument should be called \code{N}.  Note that \code{fun} is 
  not expected to have both \code{x} and \code{N} as arguments, and that the 
  latter is much faster for stratified sampling or group sampling.  
  Furthermore, if the function has arguments for sample size and probability 
  weights, they should be called \code{size} and \code{prob}, respectively.  
  Note that a function with \code{prob} as its only argument is perfectly valid 
  (for probability proportional to size sampling).  Further arguments of 
  \code{fun} may be passed directly via the \dots argument.
}
\value{
  An object of class \code{"SampleSetup"}.
}
\author{Andreas Alfons}
\seealso{
    \code{\link{setup}}, \code{"\linkS4class{SampleControl}"}, 
    \code{"\linkS4class{SampleSetup}"}
}
\examples{
data(eusilcP)

## simple random sampling
srss <- simSample(eusilcP, size = 20, k = 4)
summary(srss)
draw(eusilcP[, c("id", "eqIncome")], srss, i = 1)

## group sampling
gss <- simSample(eusilcP, grouping = "hid", size = 10, k = 4)
summary(gss)
draw(eusilcP[, c("hid", "id", "eqIncome")], gss, i = 2)

## stratified simple random sampling
ssrss <- simSample(eusilcP, design = "region", 
    size = c(2, 5, 5, 3, 4, 5, 3, 5, 2), k = 4)
summary(ssrss)
draw(eusilcP[, c("id", "region", "eqIncome")], ssrss, i = 3)

## stratified group sampling
sgss <- simSample(eusilcP, design = "region", 
    grouping = "hid", size = c(2, 5, 5, 3, 4, 5, 3, 5, 2), k = 4)
summary(sgss)
draw(eusilcP[, c("hid", "id", "region", "eqIncome")], sgss, i = 4)
}
\keyword{distribution}
