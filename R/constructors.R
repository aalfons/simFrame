# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

#' Create sample control objects
#' 
#' Create objects of a class inheriting from \code{"VirtualSampleControl"}.
#' 
#' @param \dots  arguments passed to the constructor of the class determined by 
#' \code{type}.
#' @param type  a character string specifying whether a control object of class
#' \code{"BasicSampleControl"} or \code{"TwoStageSampleControl"} should be 
#' created.
#' 
#' @return An object of class determined by \code{type}.
#' 
#' @author Andreas Alfons
#' 
#' @seealso \code{"\linkS4class{BasicSampleControl}"}, 
#' \code{"\linkS4class{TwoStageSampleControl}"}
#' 
#' @examples
#' data("eusilcP")
#' bsc <- SampleControl(size = 20, type = "Basic")
#' draw(eusilcP[, c("id", "eqIncome")], bsc)
#' 
#' @keywords classes
#' 
#' @export

SampleControl <- function(..., type = c("Basic", "TwoStage")) {
  type <- match.arg(type)
  if(type == "TwoStage") TwoStageSampleControl(...)
  else {
    class <- paste(type, "SampleControl", sep="")
    new(class, ...)
  }
}


#' Create contamination control objects
#' 
#' Create objects of a class inheriting from \code{"ContControl"}.
#' 
#' @param \dots  arguments passed to the constructor of the class determined by 
#' \code{type}.
#' @param type  a character string specifying whether a control object of class
#' \code{"BasicContControl"}, \code{"CCARContControl"} or 
#' \code{"CARContControl"} should be created.
#' 
#' @return An object of class determined by \code{type}.
#' 
#' @author Andreas Alfons
#' 
#' @seealso \code{"\linkS4class{BasicContControl}"}, 
#' \code{"\linkS4class{CCARContControl}"}, 
#' \code{"\linkS4class{CARContControl}"}
#' 
#' @examples
#' ## contaminated at random
#' library("mvtnorm")
#' mean <- rep(0, 2)
#' sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
#' foo <- generate(size = 10, distribution = rmvnorm, 
#'     dots = list(mean = mean, sigma = sigma))
#' # contaminating the first observations
#' bcc <- ContControl(target = "V2", epsilon = 0.2, 
#'     fun = function(x) x * 10, type = "Basic")
#' contaminate(foo, bcc)
#' # randomly selecting observations to be contaminated
#' carc <- ContControl(target = "V2", epsilon = 0.2, 
#'     fun = function(x) x * 10, type = "CAR")
#' contaminate(foo, carc)
#' 
#' ## contaminated completely at random
#' data("eusilcP")
#' sam <- draw(eusilcP[, c("id", "eqIncome")], size = 20)
#' ccarc <- ContControl(target = "eqIncome", epsilon = 0.05, 
#'     dots = list(mean = 5e+05, sd = 10000), type = "CCAR")
#' contaminate(sam, ccarc)
#' 
#' @keywords classes
#' 
#' @export

ContControl <- function(..., type = c("Basic", "CCAR", "CAR")) {
  type <- match.arg(type)
  class <- paste(type, "ContControl", sep="")
  new(class, ...)
}
