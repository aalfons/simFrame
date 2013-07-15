# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

setMethod(
  "aggregate", "SimResults", 
  function(x, select = NULL, FUN = mean, ...) {
    by <- c(if(length(getEpsilon(x))) "Epsilon", 
            if(nrow(getTuning(x, which = "cont"))) "ContTuning", 
            if(length(getNARate(x))) "NARate", 
            getDesign(x))
    if(is.null(select)) select <- getColnames(x)
    else {
      if(!is.character(select)) stop("'select' must be a character vector")
      if(!all(select %in% getColnames(x))) stop("undefined columns selected")
    }
    x <- getValues(x)
    if(length(by)) {
      aggregate(x[, select, drop=FALSE], by=x[, by, drop=FALSE], FUN=FUN, ...)
    } else apply(x[, select, drop=FALSE], 2, FUN=FUN, ...)
  })
