# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

setMethod(
  "subset", "SimResults", 
  function(x, data = NULL, cont = NULL, miss = NULL, select = NULL, ...) {
    # initializations
    values <- getValues(x)
    cn <- getColnames(x)
    info <- setdiff(names(values), cn)
    # find subset of observations
    subset <- NULL
    if(!is.null(data) && "Data" %in% info) {
      subset <- values$Data %in% data
    }
    if(!is.null(cont) && "Cont" %in% info) {
      tmp <- values$Cont %in% cont
      subset <- if(is.null(subset)) tmp else tmp & subset
    }
    if(!is.null(miss) && "Miss" %in% info) {
      tmp <- values$Miss %in% miss
      subset <- if(is.null(subset)) tmp else tmp & subset
    }
    # take subset of the results
    if(!is.null(subset)) values <- values[subset, , drop=FALSE]
    if(is.null(select)) select <- cn
    else {
      select <- getCharacter(select, cn)
      values <- values[, c(info, select), drop=FALSE]
    }
    SimResults(values=values, colnames=select, 
               dataControl=getControl(x, which="data"), 
               sampleControl=getControl(x, which="sample"), 
               nrep=getNrep(x), control=getControl(x))
  })
