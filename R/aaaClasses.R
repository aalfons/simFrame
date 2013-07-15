# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

## class unions of elementary classes (for convenience)

setClassUnion("BasicVector", c("character", "logical", "numeric"))
setClassUnion("NumericMatrix", c("numeric", "matrix"))
setClassUnion("OptBasicVector", c("NULL", "BasicVector"))
setClassUnion("OptCall", c("NULL", "call"))
setClassUnion("OptCharacter", c("NULL", "character"))
setClassUnion("ListOrDataFrame", c("list", "data.frame"))
setClassUnion("OptNumeric", c("NULL", "numeric"))

# ---------------------------------------

## control class for generating model based data

validDataControlObject <- function(object) {
  if(length(object@size) == 1 && object@size >= 0) TRUE
  else "'size' must be a single non-negative integer"
}

setClass("DataControl",
         representation(size = "numeric", distribution = "function", 
                        tuning = "ListOrDataFrame", dots = "list", 
                        colnames = "OptCharacter"),
         prototype(size = 0, tuning = data.frame(), colnames = NULL),
         validity = validDataControlObject)

# constructor
DataControl <- function(...) new("DataControl", ...)

# class union for extending the framework
setClassUnion("VirtualDataControl", "DataControl")

# class union for optional argument in methods
setClassUnion("OptDataControl", c("NULL", "VirtualDataControl"))

# ---------------------------------------

## sample control

# virtual class
validVirtualSampleControlObject <- function(object) {
  if(length(object@k) == 1 && object@k > 0) TRUE
  else "'k' must be a single positive integer"
}

setClass("VirtualSampleControl",
         representation(k = "numeric"),
         prototype(k = 1),
         contains = "VIRTUAL",
         validity = validVirtualSampleControlObject)

setClassUnion("OptSampleControl", c("NULL", "VirtualSampleControl"))

validSampleControlObject <- function(object) {
  lengthGrouping <- getSelectionLength(object@grouping)
  prob <- object@prob
  if(is(prob, "character") || is(prob, "logical")) {
    lengthProb <- getSelectionLength(prob)
    okProb <- is.na(lengthProb) || lengthProb <= 1
    msgProb <- "'prob' must not specify more than one variable"
  } else {
    okProb <- is.null(prob) || length(prob)
    msgProb <- "'prob' must have positive length"
  }
  ok <- c(is.na(lengthGrouping) || lengthGrouping <= 1, 
          is.null(object@size) || length(object@size), okProb, 
          length(object@collect) == 1)
  msg <- c("'grouping' must not specify more than one variable",
           "'size' must have positive length", msgProb, 
           "'collect' must be a single logical")
  if(all(ok)) TRUE
  else msg[!ok]
}

setClass("SampleControl",
         representation(design = "BasicVector", grouping = "BasicVector", 
                        collect = "logical", fun = "function", 
                        size = "OptNumeric", prob = "OptBasicVector", 
                        dots = "list"),
         prototype(design = character(), grouping = character(), 
                   collect = FALSE, size = NULL, prob = NULL),
         contains = "VirtualSampleControl",
         validity = validSampleControlObject)

SampleControl <- function(...) new("SampleControl", ...)

validTwoStageControlObject <- function(object) {
  l <- getSelectionLength(object@grouping)
  fun <- object@fun
  size <- object@size
  prob <- object@prob
  dots <- object@dots
  ok <- c(is.na(l) || l %in% 1:2, 
          length(fun) == 2 && all(sapply(fun, is, "function")), 
          length(size) == 2 && all(sapply(size, is, "OptNumeric")) && 
            all(sapply(size, function(s) is.null(s) || length(s) > 0)), 
          length(prob) == 2 && all(sapply(prob, is, "OptBasicVector")) && 
            all(sapply(prob, function(p) {
              if(is(p, "character") || is(p, "logical")) {
                lengthP <- getSelectionLength(p)
                is.na(lengthP) || lengthP <= 1
              } else is.null(p) || length(p) > 0
            })), 
          length(dots) == 2 && 
            all(sapply(dots, function(x) is.null(x) || is(x, "list"))))
  msg <- c("'grouping' must specify either one or two variables",
           "'fun' must have length 2 and each component should be a function", 
           "'size' must have length 2 and each component should be NULL or a numeric vector of positive length",
           "'prob' must have length 2 and each component should be NULL, a numeric vector of positive length, or must not specify more than one variable",
           "'dots' must have length 2 and each component should again be a list")
  if(all(ok)) TRUE
  else msg[!ok]
}

setClass("TwoStageControl",
         representation(design = "BasicVector", grouping = "BasicVector", 
                        fun = "list", size = "list", prob = "list", 
                        dots = "list"),
         prototype(design = character(), grouping = character(), 
                   size = list(NULL, NULL), prob = list(NULL, NULL), 
                   dots = list(list(), list())),
         contains = "VirtualSampleControl",
         validity = validTwoStageControlObject)

TwoStageControl <- function(..., fun1 = srs, fun2 = srs, size1 = NULL, 
                            size2 = NULL, prob1 = NULL, prob2 = NULL, 
                            dots1 = list(), dots2 = list()) {
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
  do.call(new, c("TwoStageControl", args))
}

# ---------------------------------------

## sample setup

setClass("SampleSetup",
         representation(indices = "list", prob = "numeric", 
                        control = "VirtualSampleControl", 
                        seed = "list", call = "OptCall"),
         prototype(call = NULL))

SampleSetup <- function(...) new("SampleSetup", ...)

# summary

setClass("SummarySampleSetup",
         representation(size = "numeric"))

SummarySampleSetup <- function(...) new("SummarySampleSetup", ...)

# ---------------------------------------

## contamination control

# virtual class
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

setClass("VirtualContControl",
         representation(target = "OptCharacter", epsilon = "numeric"),
         prototype(target = NULL, epsilon = 0.05),
         contains = "VIRTUAL",
         validity = validVirtualContControlObject)

setClassUnion("OptContControl", c("NULL", "VirtualContControl"))

# internal control class (not expected to be extended by the user)
setClass("ContControl",
         representation(tuning = "ListOrDataFrame", indices = "NumericMatrix"),
         prototype(tuning = data.frame(), indices = matrix(integer(0), ncol=2)), 
         contains = c("VIRTUAL", "VirtualContControl"))

# simple control class for contaminating the first observations
setClass("SimpleContControl",
         representation(fun = "function", dots = "list"),
         contains = "ContControl")

CARContControl <- function(...) new("CARContControl", ...)

# internal control class (not expected to be extended by the user)
validRandomContControlObject <- function(object) {
  ok <- c(length(object@grouping) <= 1, length(object@aux) <= 1)
  msg <- c("'grouping' must not specify more than one variable", 
           "'aux' must not specify more than one variable")
  if(all(ok)) TRUE
  else msg[!ok]
}

setClass("RandomContControl",
         representation(grouping = "character", aux = "character"),
         contains = c("VIRTUAL", "ContControl"),
         validity = validRandomContControlObject)

# contaminated completely at random (CCAR)
setClass("CCARContControl",
         representation(distribution = "function", dots = "list"),
         contains = "RandomContControl")

CCARContControl <- function(...) new("CCARContControl", ...)

# contaminated at random (CAR)
setClass("CARContControl",
         representation(fun = "function", dots = "list"),
         contains = "RandomContControl")

CARContControl <- function(...) new("CARContControl", ...)

# wrapper (mostly for compatibility)
ContControl <- function(..., type = c("Simple", "CCAR", "CAR")) {
  type <- match.arg(type)
  class <- paste(type, "ContControl", sep="")
  new(class, ...)
}

# ---------------------------------------

## NA control

# virtual class
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

setClass("VirtualNAControl",
         representation(target = "OptCharacter", NARate = "NumericMatrix"),
         prototype(target = NULL, NARate = 0.05),
         contains = "VIRTUAL",
         validity = validVirtualNAControlObject)

setClassUnion("OptNAControl", c("NULL", "VirtualNAControl"))


# select values randomly for each target variable
validNAControlObject <- function(object) {
  lengthAux <- length(object@aux)
  ok <- c(length(object@grouping) <= 1, 
          length(object@intoContamination) == 1)
  msg <- c("'grouping' must not specify more than one variable", 
           "'intoContamination' must be a single logical")
  if(all(ok)) TRUE
  else msg[!ok]
}

setClass("NAControl",
         representation(grouping = "character", aux = "character", 
                        intoContamination = "logical"),
         prototype(intoContamination=FALSE),
         contains = "VirtualNAControl", 
         validity = validNAControlObject)

NAControl <- function(...) new("NAControl", ...)

# ---------------------------------------

## simulation control

setClass("SimControl",
         representation(contControl = "OptContControl", 
                        NAControl = "OptNAControl",
                        design = "character", 
                        fun = "function", 
                        dots = "list", 
                        seed = "numeric"),
         prototype(contControl = NULL, NAControl = NULL, design = character()))

SimControl <- function(...) new("SimControl", ...)

# ---------------------------------------

## simulation results

setClass("SimResults",
         representation(values = "data.frame", colnames = "character", 
                        design = "character", epsilon = "numeric", 
                        contTuning = "data.frame", NARate = "NumericMatrix", 
                        dataControl = "OptDataControl", 
                        sampleControl = "OptSampleControl", 
                        nrep = "numeric", control = "SimControl", 
                        seed = "list", call = "OptCall"),
         prototype(NARate = as.matrix(numeric()), call = NULL, 
                   dataControl = NULL, sampleControl = NULL))

SimResults <- function(...) new("SimResults", ...)
