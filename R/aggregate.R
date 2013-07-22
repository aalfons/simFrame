# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

setMethod(
  "aggregate", "SimResults", 
  function(x, select = NULL, FUN = mean, ...) {
    # initializations
    cn <- getColnames(x)
    x <- getValues(x)
    by <- setdiff(names(x), c("Run", "Rep", "Sample", cn))
    select <- if(is.null(select)) cn else getCharacter(select, cn)
    # aggregate simulation results
    if(length(by)) {
      aggregate(x[, select, drop=FALSE], by=x[, by, drop=FALSE], FUN=FUN, ...)
    } else apply(x[, select, drop=FALSE], 2, FUN=FUN, ...)
  })
