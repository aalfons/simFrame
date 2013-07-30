# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

setMethod(
  "aggregate", "SimResults", 
  function(x, select = NULL, FUN = mean, ...) {
    # initializations
    cn <- getColnames(x)
    info <- getInfo(x)
    by <- names(info)[info > 0]
    x <- getValues(x)
    select <- if(is.null(select)) cn else getCharacter(select, cn)
    # aggregate simulation results
    aggregate(x[, select, drop=FALSE], by=x[, by, drop=FALSE], FUN=FUN, ...)
  })
