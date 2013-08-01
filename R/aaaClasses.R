# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

#' @import methods
#' @import stats4

NULL


## class unions of elementary classes (for convenience)

#' @exportClass BasicVector
setClassUnion("BasicVector", c("character", "logical", "numeric"))

#' @exportClass NumericMatrix
setClassUnion("NumericMatrix", c("numeric", "matrix"))

#' @exportClass OptCall
setClassUnion("OptCall", c("NULL", "call"))

#' @exportClass OptCharacter
setClassUnion("OptCharacter", c("NULL", "character"))

#' @exportClass ListOrDataFrame
setClassUnion("ListOrDataFrame", c("list", "data.frame"))

#' @exportClass OptNumeric
setClassUnion("OptNumeric", c("NULL", "numeric"))

# ---------------------------------------

## control class for generating model based data

validVirtualDataControlObject <- function(object) {
  if(length(object@size) > 0 && all(object@size > 0)) TRUE
  else "'size' must contain positive integers"
}

#' @exportClass VirtualDataControl
setClass("VirtualDataControl",
         representation(size = "numeric"),
         prototype(size=100),
         contains = "VIRTUAL",
         validity = validVirtualDataControlObject)


## basic class
#' @exportClass DataControl
setClass("DataControl",
         representation(tuning = "ListOrDataFrame", indices = "NumericMatrix", 
                        distribution = "function", dots = "list", 
                        colnames = "OptCharacter"),
         prototype(tuning = data.frame(), indices = matrix(integer(0), ncol=2), 
                   colnames = NULL),
         contains = "VirtualDataControl")

## constructor
#' @export
DataControl <- function(...) new("DataControl", ...)


## class union for optional argument in methods
#' @exportClass OptDataControl
setClassUnion("OptDataControl", c("NULL", "VirtualDataControl"))

# ---------------------------------------

## sample control

validVirtualSampleControlObject <- function(object) {
  if(length(object@k) == 1 && object@k > 0) TRUE
  else "'k' must be a single positive integer"
}

#' @exportClass VirtualSampleControl
setClass("VirtualSampleControl",
         representation(k = "numeric", seed = "numeric"),
         prototype(k = 1),
         contains = "VIRTUAL",
         validity = validVirtualSampleControlObject)


## class union for optional argument in methods
#' @exportClass OptSampleControl
setClassUnion("OptSampleControl", c("NULL", "VirtualSampleControl"))


## basic sampling designs

validBasicSampleControlObject <- function(object) {
  lengthGrouping <- getSelectionLength(object@grouping)
  lengthProb <- getSelectionLength(object@prob)
  ok <- c(is.na(lengthGrouping) || lengthGrouping <= 1, 
          is.null(object@size) || length(object@size), 
          is.na(lengthProb) || lengthProb <= 1, 
          length(object@collect) == 1)
  msg <- c("'grouping' must not specify more than one variable",
           "'size' must have positive length", 
           "'prob' must not specify more than one variable", 
           "'collect' must be a single logical")
  if(all(ok)) TRUE
  else msg[!ok]
}

#' @exportClass BasicSampleControl
setClass("BasicSampleControl",
         representation(design = "BasicVector", grouping = "BasicVector", 
                        collect = "logical", fun = "function", 
                        size = "OptNumeric", prob = "BasicVector", 
                        dots = "list"),
         prototype(design = character(), grouping = character(), 
                   collect = FALSE, size = NULL, prob = character()),
         contains = "VirtualSampleControl",
         validity = validBasicSampleControlObject)

#' @export
BasicSampleControl <- function(...) new("BasicSampleControl", ...)


## two-stage sampling designs

validTwoStageSampleControlObject <- function(object) {
  l <- getSelectionLength(object@grouping)
  fun <- object@fun
  size <- object@size
  prob <- object@prob
  dots <- object@dots
  ok <- c(is.na(l) || l %in% 1:2, 
          length(fun) == 2 && all(sapply(fun, is, "function")), 
          length(size) == 2 && all(sapply(size, is, "OptNumeric")) && 
            all(sapply(size, function(s) is.null(s) || length(s) > 0)), 
          length(prob) == 2 && all(sapply(prob, is, "BasicVector")) && 
            all(sapply(prob, function(p) {
              lengthP <- getSelectionLength(p)
              is.na(lengthP) || lengthP <= 1
            })), 
          length(dots) == 2 && 
            all(sapply(dots, function(x) is.null(x) || is(x, "list"))))
  msg <- c("'grouping' must specify either one or two variables",
           "'fun' must have length 2 and each component should be a function", 
           "'size' must have length 2 and each component should be NULL or a numeric vector of positive length",
           "'prob' must have length 2 and each component must not specify more than one variable",
           "'dots' must have length 2 and each component should again be a list")
  if(all(ok)) TRUE
  else msg[!ok]
}

#' @exportClass TwoStageSampleControl
setClass("TwoStageSampleControl",
         representation(design = "BasicVector", grouping = "BasicVector", 
                        fun = "list", size = "list", prob = "list", 
                        dots = "list"),
         prototype(design = character(), grouping = character(), 
                   size = list(NULL, NULL), 
                   prob = list(character(), character()), 
                   dots = list(list(), list())),
         contains = "VirtualSampleControl",
         validity = validTwoStageSampleControlObject)

#' @export
TwoStageSampleControl <- function(..., fun1 = srs, fun2 = srs, size1 = NULL, 
                                  size2 = NULL, prob1 = character(), 
                                  prob2 = character(), dots1 = list(), 
                                  dots2 = list()) {
  # list components for the two stages can be supplied separately
  args <- list(...)
  if(is.null(args$fun) && !(missing(fun1) && missing(fun2))) {
    args$fun <- list(fun1, fun2)
  }
  if(is.null(args$size) && !(missing(size1) && missing(size2))) {
    args$size <- list(size1, size2)
  }
  if(is.null(args$prob) && !(missing(prob1) && missing(prob2))) {
    args$prob <- list(prob1, prob2)
  }
  if(is.null(args$dots) && !(missing(dots1) && missing(dots2))) {
    args$dots <- list(dots1, dots2)
  }
  do.call(new, c("TwoStageSampleControl", args))
}

# ---------------------------------------

## sample setup

#' @exportClass SampleSetup
setClass("SampleSetup",
         representation(indices = "list", prob = "numeric", 
                        control = "VirtualSampleControl", 
                        call = "OptCall"),
         prototype(call = NULL))

#' @export
SampleSetup <- function(...) new("SampleSetup", ...)


## summary

#' @exportClass SummarySampleSetup
setClass("SummarySampleSetup",
         representation(size = "numeric"))

#' @export
SummarySampleSetup <- function(...) new("SummarySampleSetup", ...)

# ---------------------------------------

## contamination control

validVirtualContControlObject <- function(object) {
  ok <- c(length(object@target) > 0 || is.null(object@target), 
          length(object@epsilon) > 0, 
          all(0 <= object@epsilon & object@epsilon <= 0.5))
  msg <- c("'target' must be specified", 
           "'epsilon' must be specified",  
           "values in 'epsilon' must be between 0 and 0.5")
  if(all(ok)) TRUE
  else msg[!ok]
}

#' @exportClass VirtualContControl
setClass("VirtualContControl",
         representation(target = "OptCharacter", epsilon = "numeric"),
         prototype(target = NULL, epsilon = 0.05),
         contains = "VIRTUAL",
         validity = validVirtualContControlObject)


## class union for optional argument in methods
#' @exportClass OptContControl
setClassUnion("OptContControl", c("NULL", "VirtualContControl"))


## internal control class (not expected to be extended by the user)
#' @exportClass ContControl
setClass("ContControl",
         representation(tuning = "ListOrDataFrame", indices = "NumericMatrix"),
         prototype(tuning = data.frame(), indices = matrix(integer(0), ncol=2)), 
         contains = c("VIRTUAL", "VirtualContControl"))


## simple control class for contaminating the first observations

#' @exportClass BasicContControl
setClass("BasicContControl",
         representation(fun = "function", dots = "list"),
         contains = "ContControl")

#' @export
BasicContControl <- function(...) new("BasicContControl", ...)


## internal control class (not expected to be extended by the user)

validRandomContControlObject <- function(object) {
  ok <- c(length(object@grouping) <= 1, length(object@aux) <= 1)
  msg <- c("'grouping' must not specify more than one variable", 
           "'aux' must not specify more than one variable")
  if(all(ok)) TRUE
  else msg[!ok]
}

#' @exportClass RandomContControl
setClass("RandomContControl",
         representation(grouping = "character", aux = "character"),
         contains = c("VIRTUAL", "ContControl"),
         validity = validRandomContControlObject)


## contaminated completely at random (CCAR)

#' @exportClass CCARContControl
setClass("CCARContControl",
         representation(distribution = "function", dots = "list"),
         contains = "RandomContControl")

#' @export
CCARContControl <- function(...) new("CCARContControl", ...)


## contaminated at random (CAR)

#' @exportClass CARContControl
setClass("CARContControl",
         representation(fun = "function", dots = "list"),
         contains = "RandomContControl")

#' @export
CARContControl <- function(...) new("CARContControl", ...)

# ---------------------------------------

## NA control

validVirtualNAControlObject <- function(object) {
  NARate <- object@NARate
  nl <- getLength(NARate)
  ok <- c(length(object@target) > 0 || is.null(object@target), 
          nl > 0 || is.na(nl), 
          checkNumericMatrix(NARate), 
          all(0 <= NARate & NARate <= 1))
  msg <- c("'target' must be specified", 
           "'NARate' must be specified", 
           "non-numeric values in 'NARate'", 
           "values in 'NARate' must be between 0 and 1")
  if(all(ok)) TRUE
  else msg[!ok]
}

#' @exportClass VirtualNAControl
setClass("VirtualNAControl",
         representation(target = "OptCharacter", NARate = "NumericMatrix"),
         prototype(target = NULL, NARate = 0.05),
         contains = "VIRTUAL",
         validity = validVirtualNAControlObject)


## class union for optional argument in methods
#' @exportClass OptNAControl
setClassUnion("OptNAControl", c("NULL", "VirtualNAControl"))


## select values randomly for each target variable

validNAControlObject <- function(object) {
  lengthAux <- length(object@aux)
  ok <- c(length(object@grouping) <= 1, 
          length(object@intoContamination) == 1)
  msg <- c("'grouping' must not specify more than one variable", 
           "'intoContamination' must be a single logical")
  if(all(ok)) TRUE
  else msg[!ok]
}

#' @exportClass NAControl
setClass("NAControl",
         representation(grouping = "character", aux = "character", 
                        intoContamination = "logical"),
         prototype(intoContamination=FALSE),
         contains = "VirtualNAControl", 
         validity = validNAControlObject)

#' @export
NAControl <- function(...) new("NAControl", ...)

# ---------------------------------------

## simulation control

#' @exportClass SimControl
setClass("SimControl",
         representation(contControl = "OptContControl", 
                        NAControl = "OptNAControl",
                        design = "character", 
                        fun = "function", 
                        dots = "list", 
                        seed = "numeric"),
         prototype(contControl = NULL, NAControl = NULL, design = character()))

#' @export
SimControl <- function(...) new("SimControl", ...)

# ---------------------------------------

## simulation results

#' @exportClass SimResults
setClass("SimResults",
         representation(values = "data.frame", colnames = "character", 
                        info = "numeric", dataControl = "OptDataControl", 
                        sampleControl = "OptSampleControl", nrep = "numeric", 
                        control = "SimControl", call = "OptCall"),
         prototype(dataControl = NULL, sampleControl = NULL, call = NULL))

#' @export
SimResults <- function(...) new("SimResults", ...)
