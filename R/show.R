# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

## data control

setMethod(
  "show", "DataControl", 
  function(object) {
    cat("Number of observations to be generated:\n")
    print(getSize(object))
    tuning <- getTuning(object)
    if(length(tuning) > 0) {
      cat("\nTuning parameters:\n")
      print(tuning)
    }
    dots <- getDots(object)
    if(length(dots) > 0) {
      cat("\nAdditional parameters:\n")
      print(dots)
    }
    colnames <- getColnames(object)
    if(!is.null(colnames)) {
      cat("\nColumn names of the resulting data frame:\n")
      print(colnames)
    }
  })


## contamination control

setMethod(
  "show", "VirtualContControl", 
  function(object) {
    target <- getTarget(object)
    if(is.null(target)) cat("All variables are target variables\n")
    else {
      if(length(target) == 1) cat("Target variable:\n")
      else cat("Target variables:\n")
      print(target)
    }
    cat("\nEpsilon:\n")
    print(getEpsilon(object))
  })

setMethod(
  "show", "ContControl", 
  function(object) {
    callNextMethod()
    tuning <- getTuning(object)
    if(length(tuning) > 0) {
      cat("\nTuning parameters:\n")
      print(tuning)
    }
  })

setMethod(
  "show", "RandomContControl", 
  function(object) {
    callNextMethod()
    group <- getGrouping(object)
    if(length(group) > 0) {
      cat("\nGrouping variable for contaminating clusters:\n")
      print(group)
    }
    aux <- getAux(object)
    if(length(aux) > 0) {
      cat("\nVariable giving probability weights for selection:\n")
      print(aux)
    }
  })


## NA control

setMethod(
  "show", "VirtualNAControl", 
  function(object) {
    target <- getTarget(object)
    if(is.null(target)) cat("All variables are target variables\n")
    else {
      if(length(target) == 1) cat("Target variable:\n")
      else cat("Target variables:\n")
      print(target)
    }
    cat("\nMissing value rates:\n")
    print(getNARate(object))
  })

setMethod(
  "show", "NAControl", 
  function(object) {
    callNextMethod()
    group <- getGrouping(object)
    if(length(group) > 0) {
      cat("\nGrouping variable for setting clusters to NA:\n")
      print(group)
    }
    aux <- getAux(object)
    if(length(aux) > 0) {
      if(length(aux) == 1) {
        cat("\nVariable giving probability weights for selection:\n")
      } else {
        cat("\nVariables giving probability weights for selection:\n")
      }
      print(aux)
    }
  })

## simulation control
setMethod(
  "show", "SimControl", 
  function(object) {
    contControl <- getContControl(object)
    if(is.null(contControl)) {
      cat("Contamination will not be added\n")
    } else {
      cat("Settings for contamination:\n")
      show(contControl)
    }
    NAControl <- getNAControl(object)
    blanks <- if(is.null(contControl) && is.null(NAControl)) "\n" else "\n\n"
    cat(blanks)
    if(is.null(NAControl)) {
      cat("NAs will not be inserted\n")
      blanks <- "\n"
    } else {
      cat("Settings for inserting NAs:\n")
      show(NAControl)
      blanks <- "\n\n"
    }
    design <- getDesign(object)
    if(length(design) > 0) {
      cat(blanks)
      cat("Variable(s) giving domains for splitting simulations:\n")
      print(design)
      blanks <- "\n"
    }
  })

## simulation results
setMethod("show", "SimResults", function(object) print(getValues(object)))
