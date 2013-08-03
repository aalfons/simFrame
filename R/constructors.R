# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

#' Create sample control objects
#' 
#' Create objects of a class inheriting from 
#' \code{"\linkS4class{SampleControl}"}.
#' 
#' @param \dots  arguments passed to the constructor of the class determined by 
#' \code{sub}.
#' @param sub  a character string specifying whether to create a control 
#' object of the subclass \code{"\linkS4class{BasicSampleControl}"} or 
#' \code{"\linkS4class{TwoStageSampleControl}"}.
#' 
#' @return An object of class determined by \code{sub}.
#' 
#' @author Andreas Alfons
#' 
#' @seealso \code{"\linkS4class{BasicSampleControl}"}, 
#' \code{"\linkS4class{TwoStageSampleControl}"}
#' 
#' @examples
#' data("eusilcP")
#' bsc <- SampleControl(size = 20, sub = "Basic")
#' draw(eusilcP[, c("id", "eqIncome")], bsc)
#' 
#' @keywords classes
#' 
#' @export

SampleControl <- function(..., sub = c("Basic", "TwoStage")) {
  sub <- match.arg(sub)
  if(sub == "TwoStage") TwoStageSampleControl(...)
  else {
    class <- paste(sub, "SampleControl", sep="")
    new(class, ...)
  }
}


#' Create contamination control objects
#' 
#' Create objects of a class inheriting from \code{"\linkS4class{ContControl}"}.
#' 
#' @param \dots  arguments passed to the constructor of the class determined by 
#' \code{sub}.
#' @param sub  a character string specifying whether to create a control 
#' object of the subclass \code{"\linkS4class{BasicContControl}"} or 
#' \code{"\linkS4class{RandomContControl}"}.
#' 
#' @return An object of class determined by \code{sub}.
#' 
#' @author Andreas Alfons
#' 
#' @seealso \code{"\linkS4class{BasicContControl}"}, 
#' \code{"\linkS4class{RandomContControl}"}
#' 
#' @examples
#' # contaminate the first observations
#' x <- generate(size = 10)
#' bcc <- ContControl(epsilon = 0.2, fun = function(x) x * 10, 
#'     type = "CAR", sub = "Basic")
#' contaminate(x, bcc)
#' 
#' # randomly select observations to be contaminated
#' data("eusilcP")
#' sam <- draw(eusilcP[, c("id", "eqIncome")], size = 20)
#' rcc <- ContControl(target = "eqIncome", epsilon = 0.1, 
#'     dots = list(mean = 5e+05, sd = 10000), 
#'     type = "CCAR", sub = "Random")
#' contaminate(sam, rcc)
#' 
#' @keywords classes
#' 
#' @export

ContControl <- function(..., sub = c("Basic", "Random")) {
  sub <- match.arg(sub)
  class <- paste(sub, "ContControl", sep="")
  new(class, ...)
}
