# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

setMethod(
  "generate", signature(control = "DataControl"), 
  function(control, i = 1) {
    # initializations
    indices <- getIndices(control)
    size <- getSize(control)
    tuning <- getTuning(control)
    if(nrow(indices)) {
      size <- size[indices[i, 1]]
      tuning <- tuning[indices[i, 2], , drop=FALSE]
    }
    nam <- getColnames(control)
    # generate data
    values <- doCall(getFun(control), size, tuning, getDots(control))
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
  })

setMethod(
  "generate", signature(control = "character"), 
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

setMethod(
  "generate", signature(control = "missing"),
  function(control, ...) {
    generate(DataControl(...))
  })
