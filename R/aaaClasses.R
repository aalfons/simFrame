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

#' @exportClass OptCall
setClassUnion("OptCall", c("NULL", "call"))

#' @exportClass OptCharacter
setClassUnion("OptCharacter", c("NULL", "character"))

#' @exportClass OptNumeric
setClassUnion("OptNumeric", c("NULL", "numeric"))

# ---------------------------------------

#### model based data generation


### virtual control class for extensions by users

## validity checks
validVirtualDataControlObject <- function(object) {
  if(length(object@size) > 0 && all(object@size > 0)) TRUE
  else "'size' must contain positive integers"
}

## class definition
#' @exportClass VirtualDataControl
setClass("VirtualDataControl", representation(size = "numeric"),
         contains = "VIRTUAL", validity = validVirtualDataControlObject)

## class initialization
setMethod(
  "initialize", "VirtualDataControl", 
  function(.Object, size = 100, ...) {
    # check data sizes
    size <- as.integer(size)
    size <- size[is.finite(size) & size > 0]
    # call method for superclass (or default)
    callNextMethod(.Object, size=size, ...)
  })

## class union for optional argument in methods
#' @exportClass OptDataControl
setClassUnion("OptDataControl", c("NULL", "VirtualDataControl"))


### basic class

## class definition
#' @exportClass DataControl
setClass("DataControl",
         representation(tuning = "data.frame", indices = "matrix", 
                        fun = "function", dots = "list", 
                        colnames = "OptCharacter"),
         contains = "VirtualDataControl")

## class initialization
setMethod(
  "initialize", "DataControl", 
  function(.Object, tuning = data.frame(), indices, 
           fun = rnorm, colnames = NULL, ...) {
    # call method for superclass (or default) to set data sizes
    .Object <- callNextMethod(.Object, fun=fun, colnames=colnames, ...)
    # make sure that tuning parameters are stored as data frame
    .Object@tuning <- expandTuning(tuning)
    # get indices for data sizes and tuning parameters
    .Object@indices <- convertToIndices(.Object@size, .Object@tuning, 
                                        checkZero=FALSE)
    # return the object
    .Object
  })

## constructor
#' @export
DataControl <- function(...) new("DataControl", ...)

# ---------------------------------------

#### sampling


### virtual control class for extensions by users

## class definition
setClass("VirtualSampleControl", 
         representation(k = "numeric", seed = "numeric"),
         contains = "VIRTUAL")

## class initialization
setMethod(
  "initialize", "VirtualSampleControl", 
  function(.Object, k = 1, seed = NA, ...) {
    # check number of samples to be set up
    k <- rep(as.integer(k), length.out=1)
    if(is.na(k) || k < 1) k <- 1
    # use seed based on current date and time as default
    seed <- rep(as.integer(seed), length.out=1)
    if(is.na(seed)) seed <- as.integer(Sys.time())
    # call method for superclass (or default)
    callNextMethod(.Object, k=k, seed=seed, ...)
  })

## class union for optional argument in methods
#' @exportClass OptSampleControl
setClassUnion("OptSampleControl", c("NULL", "VirtualSampleControl"))


### internal control class (not expected to be extended by the user)

## class definition
#' @exportClass SampleControl
setClass("SampleControl",
         representation(design = "BasicVector", grouping = "BasicVector"),
         contains = c("VIRTUAL", "VirtualSampleControl"))


### basic sampling designs

## validity checks
validBasicSampleControlObject <- function(object) {
  lengthGrouping <- getSelectionLength(object@grouping)
  lengthProb <- getSelectionLength(object@prob)
  ok <- c(is.na(lengthGrouping) || lengthGrouping <= 1, 
          is.null(object@size) || length(object@size), 
          is.na(lengthProb) || lengthProb <= 1)
  msg <- c("'grouping' must not specify more than one variable",
           "'size' must contain non-negative integers", 
           "'prob' must not specify more than one variable")
  if(all(ok)) TRUE
  else msg[!ok]
}

## class definition
#' @exportClass BasicSampleControl
setClass("BasicSampleControl",
         representation(collect = "logical", fun = "function", 
                        size = "OptNumeric", prob = "BasicVector", 
                        dots = "list"),
         contains = "SampleControl", validity = validBasicSampleControlObject)

## class initialization
setMethod(
  "initialize", "BasicSampleControl", 
  function(.Object, collect = FALSE, fun = srs, size = NULL, ...) {
    # make sure 'collect' is either TRUE or FALSE
    collect <- isTRUE(collect)
    # check sample sizes
    if(!is.null(size)) {
      size <- as.integer(size)
      size <- size[is.finite(size) & size >= 0]
    }
    # call method for superclass (or default)
    callNextMethod(.Object, collect=collect, fun=fun, size=size, ...)
  })

## constructor
#' @export
BasicSampleControl <- function(...) new("BasicSampleControl", ...)


### two-stage sampling designs

## validity checks
validTwoStageSampleControlObject <- function(object) {
  lengthGrouping <- getSelectionLength(object@grouping)
  fun <- object@fun
  size <- object@size
  prob <- object@prob
  dots <- object@dots
  ok <- c(is.na(lengthGrouping) || lengthGrouping %in% 1:2, 
          length(fun) == 2, 
          all(sapply(fun, is, "function")), 
          length(size) == 2, 
          all(sapply(size, is, "OptNumeric")) && 
            all(sapply(size, function(s) is.null(s) || length(s) > 0)), 
          length(prob) == 2, 
          all(sapply(prob, is, "BasicVector")) && 
            all(sapply(prob, function(p) {
              lengthP <- getSelectionLength(p)
              is.na(lengthP) || lengthP <= 1
            })), 
          length(dots) == 2, 
          all(sapply(dots, function(x) is.null(x) || is(x, "list"))))
  msg <- c("'grouping' must specify either one or two variables",
           "'fun' must contain two components",
           "components of 'fun' must be functions",
           "'size' must contain two components",
           "components of 'size' must be NULL or contain non-negative integers",
           "'prob' must contain two components",
           "components of 'prob' must not specify more than one variable",
           "'dots' must contain two components",
           "components of 'dots' should again be a list")
  if(all(ok)) TRUE
  else msg[!ok]
}

## class definition
#' @exportClass TwoStageSampleControl
setClass("TwoStageSampleControl",
         representation(fun = "list", size = "list", prob = "list", 
                        dots = "list"),
         contains = "SampleControl", 
         validity = validTwoStageSampleControlObject)

## class initialization
setMethod(
  "initialize", "TwoStageSampleControl", 
  function(.Object, fun = list(srs, srs), size = list(NULL, NULL), 
           prob = list(character(), character()), 
           dots = list(list(), list()), ...) {
    # check sample sizes
    if(is(size, "list") && length(size) == 2) {
      size <- lapply(size, function(s) {
        if(!is.null(s)) {
          s <- as.integer(s)
          s <- s[is.finite(s) & s >= 0]
        }
        s
      })
    }
    # call method for superclass (or default)
    callNextMethod(.Object, fun=fun, size=size, prob=prob, dots=dots, ...)
  })

## constructor
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


### set-up samples

## class definition
#' @exportClass SampleSetup
setClass("SampleSetup",
         representation(indices = "list", prob = "numeric", 
                        control = "VirtualSampleControl", 
                        call = "OptCall"),
         prototype(call = NULL))

## constructor
#' @export
SampleSetup <- function(...) new("SampleSetup", ...)


### summary of set-up samples

## class definition
#' @exportClass SummarySampleSetup
setClass("SummarySampleSetup", representation(size = "numeric"))

## constructor
#' @export
SummarySampleSetup <- function(...) new("SummarySampleSetup", ...)

# ---------------------------------------

#### contamination


### virtual control class for extensions by users

## validity checks
validVirtualContControlObject <- function(object) {
  ok <- c(is.null(object@target) || length(object@target) > 0, 
          length(object@epsilon) > 0)
  msg <- c("'target' must be specified", 
           "'epsilon' must contain values between 0 and 0.5")
  if(all(ok)) TRUE
  else msg[!ok]
}

## class definition
#' @exportClass VirtualContControl
setClass("VirtualContControl",
         representation(target = "OptCharacter", epsilon = "numeric"),
         contains = "VIRTUAL", validity = validVirtualContControlObject)

## class initialization
setMethod(
  "initialize", "VirtualContControl", 
  function(.Object, target = NULL, epsilon = 0.05, ...) {
    # check contamination levels
    epsilon <- as.numeric(epsilon)
    epsilon <- epsilon[!is.na(epsilon) & epsilon >= 0 & epsilon <= 0.5]
    # call method for superclass (or default)
    callNextMethod(.Object, target=target, epsilon=epsilon, ...)
  })

## class union for optional argument in methods
#' @exportClass OptContControl
setClassUnion("OptContControl", c("NULL", "VirtualContControl"))


### internal control class (not expected to be extended by the user)

## class definition
#' @exportClass ContControl
setClass("ContControl",
         representation(tuning = "data.frame", indices = "matrix", 
                        fun = "function", dots = "list", 
                        type = "character"),
         contains = c("VIRTUAL", "VirtualContControl"))

## class initialization
setMethod(
  "initialize", "ContControl", 
  function(.Object, tuning = data.frame(), indices, fun = NULL, 
           type = c("CCAR", "CAR"), ...) {
    # set default function for contamination if necessary
    type <- match.arg(type)
    if(is.null(fun)) fun <- switch(type, CCAR=rnorm, CAR=function(...) "+"(...))
    # call method for superclass (or default) to set contamination levels
    .Object <- callNextMethod(.Object, fun=fun, type=type, ...)
    # make sure that tuning parameters are stored as data frame
    .Object@tuning <- expandTuning(tuning)
    # get indices for contamination level and tuning parameters
    .Object@indices <- convertToIndices(.Object@epsilon, .Object@tuning, 
                                        checkZero=TRUE)
    # return the object
    .Object
  })


### control class for contaminating the first observations

## class definition
#' @exportClass BasicContControl
setClass("BasicContControl", contains = "ContControl")

## constructor
#' @export
BasicContControl <- function(...) new("BasicContControl", ...)


### control class for randomly selecting the observations to be contaminated

## validity checks
validRandomContControlObject <- function(object) {
  ok <- c(length(object@grouping) <= 1, length(object@aux) <= 1)
  msg <- c("'grouping' must not specify more than one variable", 
           "'aux' must not specify more than one variable")
  if(all(ok)) TRUE
  else msg[!ok]
}

## class definition
#' @exportClass RandomContControl
setClass("RandomContControl",
         representation(grouping = "character", aux = "character"),
         contains = "ContControl", validity = validRandomContControlObject)

## constructor
#' @export
RandomContControl <- function(...) new("RandomContControl", ...)

# ---------------------------------------

#### missing data generation


### virtual control class for extensions by users

## validity checks
validVirtualNAControlObject <- function(object) {
  NARate <- object@NARate
  ok <- c(is.null(object@target) || length(object@target) > 0, 
          mode(NARate) == "numeric" && all(dim(NARate) > 0) && 
            all(!is.na(NARate) & NARate >= 0 & NARate <= 1))
  msg <- c("'target' must be specified", 
           "'NARate' must contain values between 0 and 1")
  if(all(ok)) TRUE
  else msg[!ok]
}

## class definition
#' @exportClass VirtualNAControl
setClass("VirtualNAControl",
         representation(target = "OptCharacter", NARate = "matrix"),
         contains = "VIRTUAL", validity = validVirtualNAControlObject)

## class initialization
setMethod(
  "initialize", "VirtualNAControl", 
  function(.Object, target = NULL, NARate = 0.05, ...) {
    # check missing value rates
    if(is(NARate, "numeric")) NARate <- as.matrix(NARate)
    if(ncol(NARate) == 1) {
      NARate <- NARate[!is.na(NARate) & NARate >= 0 & NARate <= 1, , drop=FALSE]
    }
    # call method for superclass (or default)
    callNextMethod(.Object, target=target, NARate=NARate, ...)
  })

## class union for optional argument in methods
#' @exportClass OptNAControl
setClassUnion("OptNAControl", c("NULL", "VirtualNAControl"))


### control class for randomly selecting values for each target variable

## validity checks
validNAControlObject <- function(object) {
  if(length(object@grouping) <= 1) TRUE
  else "'grouping' must not specify more than one variable"
}

## class definition
#' @exportClass NAControl
setClass("NAControl",
         representation(grouping = "character", aux = "character", 
                        intoContamination = "logical"),
         contains = "VirtualNAControl", validity = validNAControlObject)

## class initialization
setMethod(
  "initialize", "NAControl", 
  function(.Object, intoContamination = FALSE, ...) {
    # make sure 'intoContamination' is either TRUE or FALSE
    intoContamination <- isTRUE(intoContamination)
    # call method for superclass (or default)
    callNextMethod(.Object, intoContamination=intoContamination, ...)
  })

## constructor
#' @export
NAControl <- function(...) new("NAControl", ...)

# ---------------------------------------

### simulation control

## class definition
#' @exportClass SimControl
setClass("SimControl",
         representation(contControl = "OptContControl", 
                        NAControl = "OptNAControl",
                        design = "character", fun = "function", 
                        dots = "list", seed = "numeric"))

## class initialization
setMethod(
  "initialize", "SimControl", 
  function(.Object, contControl = NULL, NAControl = NULL, seed = NA, ...) {
    # use seed based on current date and time as default
    seed <- rep(as.integer(seed), length.out=1)
    if(is.na(seed)) seed <- as.integer(Sys.time())
    # call method for superclass (or default)
    callNextMethod(.Object, contControl=contControl, NAControl=NAControl, 
                   seed=seed, ...)
  })

## constructor
#' @export
SimControl <- function(...) new("SimControl", ...)

# ---------------------------------------

### simulation results

## class definition
#' @exportClass SimResults
setClass("SimResults",
         representation(values = "data.frame", colnames = "character", 
                        info = "numeric", dataControl = "OptDataControl", 
                        sampleControl = "OptSampleControl", nrep = "numeric", 
                        control = "SimControl", call = "OptCall"),
         prototype(dataControl = NULL, sampleControl = NULL, call = NULL))

## constructor
#' @export
SimResults <- function(...) new("SimResults", ...)
