# ----------------------
# Author: Andreas Alfons
#         KU Leuven
# ----------------------

## internal S3 function 
# this is used in 'runSimulation' and 'clusterRunSimulation': there the 
# objects are already checked for validity and this speeds things up slightly
generateS3 <- function(control, i = 1) {
  # initializations
  size <- getSize(control)
  distribution <- getDistribution(control)
  tuning <- getTuning(control)
  dots <- getDots(control)
  nam <- getColnames(control)
  # generate data
  if(nrow(tuning) > 0) tuning <- tuning[i,]
  values <- doCallTuning(distribution, size, tuning, dots)
  # set column names
  if(is.null(dim(values)) && is.null(nam)) nam <- "V1" 
  values <- as.data.frame(values)
  if(!is.null(nam)) {
    p <- ncol(values)
    if(length(nam) != p) {
      stop(gettextf("'names' must be a vector of length %i", p))
    }
    names(values) <- nam
  }
  # return generated data frame
  values
}


## S4 methods

setMethod("generate", signature(control = "DataControl"), generateS3)

setMethod("generate", 
          signature(control = "character"), 
          function(control, ...) {
            if(length(control) != 1) {
              stop("'control' must specify exactly one ", 
                   "class extending \"VirtualDataControl\"")
            }
            if(!extends(control, "VirtualDataControl")) {
              stop(gettextf("\"%s\" does not extend \"VirtualDataControl\"", 
                            control))
            }
            generate(new(control, ...))
          })

setMethod("generate",
          signature(control = "missing"),
          function(control, ...) {
            generate(DataControl(...))
          })
