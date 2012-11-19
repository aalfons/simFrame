# ----------------------
# Author: Andreas Alfons
#         KU Leuven
# ----------------------

## data control

setMethod("show", "DataControl", 
          function(object) {
            cat("Number of observations to be generated:\n")
            print(getSize(object))
#             cat("\nGenerating (distribution) function:\n")
#             print(getDistribution(object))
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

setMethod("show", "VirtualContControl", 
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

setMethod("show", "ContControl", 
          function(object) {
            callNextMethod()
            tuning <- getTuning(object)
            if(length(tuning) > 0) {
              cat("\nTuning parameters:\n")
              print(tuning)
            }
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

setMethod("show", "VirtualNAControl", 
          function(object) {
            target <- getTarget(object)
            if(is.null(target)) cat("All variables are target variables\n")
            else {
              if(length(target) == 1) cat("Target variable:\n")
              else cat("Target variables:\n")
              print(target)
            }
            cat("\nMissing value rates:\n")
            print(getNArate(object))
          })

setMethod("show", "NAControl", 
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
