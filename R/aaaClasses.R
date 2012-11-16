# ----------------------
# Author: Andreas Alfons
#         KU Leuven
# ----------------------

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
# DataControl <- function(size, distribution, tuning = NULL, dots = list(...), 
#                         colnames = NULL, ...) {
#   if(is.list(tuning)) tuning <- expand.grid(tuning, stringsAsFactors=FALSE)
#   new("DataControl", size=size, distribution=distribution, tuning=tuning, 
#       dots=dots, colnames=colnames, ...)
# }
DataControl <- function(...) new("DataControl", ...)

# class union for extending the framework
setClassUnion("VirtualDataControl", "DataControl")

# class union for optional argument in methods
setClassUnion("OptDataControl", c("NULL", "VirtualDataControl"))

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
validContControlObject <- function(object) {
  ok <- c(length(object@grouping) <= 1, length(object@aux) <= 1)
  msg <- c("'grouping' must not specify more than one variable", 
           "'aux' must not specify more than one variable")
  if(all(ok)) TRUE
  else msg[!ok]
}

setClass("ContControl",
         representation(tuning = "ListOrDataFrame", grouping = "character", 
                        aux = "character"),
         prototype(tuning = data.frame()), 
         contains = c("VIRTUAL", "VirtualContControl"),
         validity = validContControlObject)

# contamination distributed completely at random (DCAR)
setClass("DCARContControl",
         representation(distribution = "function", dots = "list"),
         contains = "ContControl")

DCARContControl <- function(...) new("DCARContControl", ...)

# contamination distributed at random (DAR)
setClass("DARContControl",
         representation(fun = "function", dots = "list"),
         contains = "ContControl")

DARContControl <- function(...) new("DARContControl", ...)

# wrapper (mostly for compatibility)
ContControl <- function(..., type = c("DCAR", "DAR")) {
  type <- match.arg(type)
  class <- paste(type, "ContControl", sep="")
  new(class, ...)
}
