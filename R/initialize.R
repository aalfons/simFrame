# ----------------------
# Author: Andreas Alfons
#         KU Leuven
# ----------------------

# model based data
setMethod("initialize", "DataControl", 
          function(.Object, ...) {
            args <- list(...)
            # use normal distribution as default
            if(is.null(args$distribution)) setDistribution(.Object, rnorm)
            # make sure that tuning parameters are stored as data frame
            if(is.list(tuning <- args$tuning)) {
              setTuning(.Object, tuning)
              args$tuning <- NULL
            }
            # add current object to arguments and call method for superclass
            args[[".Object"]] <- .Object
            do.call(callNextMethod, args)
          })

# contamination
setMethod("initialize", "ContControl", 
          function(.Object, ...) {
            args <- list(...)
            # make sure that tuning parameters are stored as data frame
            if(is.list(tuning <- args$tuning)) {
              setTuning(.Object, tuning)
              args$tuning <- NULL
            }
            # add current object to arguments and call method for superclass
            args[[".Object"]] <- .Object
            do.call(callNextMethod, args)
          })

# contamination distributed completely at random (DCAR)
setMethod("initialize", "DCARContControl", 
          function(.Object, ...) {
            args <- list(...)
            # use normal distribution as default for contamination data
            if(is.null(args$distribution)) setDistribution(.Object, rnorm)
            callNextMethod()  # call method for superclass (or default)
          })

# # insertion of missing values
# setMethod("initialize", "NAControl", 
#           function(.Object, ...) {
#             args <- list(...)
#             # make sure logical indicator whether NAs should be insereted 
#             # into contaminated observations is either TRUE or FALSE
#             intoContamination <- isTRUE(getIntoContamination(.Object))
#             setIntoContamination(.Object, intoContamination)
#             callNextMethod()  # call method for superclass (or default)
#           })
