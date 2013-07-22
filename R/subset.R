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
    dataControl <- getControl(x, which="data")
    control <- getControl(x)
    # find subset of observations
    subset <- NULL
    if(!is.null(data) && "Data" %in% info) {
      if(!is.numeric(data)) data <- seq_along(dataControl)[data]
      subset <- values$Data %in% data
    }
    if(!is.null(cont) && "Cont" %in% info) {
      if(!is.numeric(cont)) {
        contControl <- getControl(control, which="cont")
        cont <- seq_along(contControl)[cont]
      }
      tmp <- values$Cont %in% cont
      subset <- if(is.null(subset)) tmp else tmp & subset
    }
    if(!is.null(miss) && "Miss" %in% info) {
      if(!is.numeric(miss)) {
        NAControl <- getControl(control, which="NA")
        miss <- seq_along(NAControl)[miss]
      }
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
    SimResults(values=values, colnames=select, dataControl=dataControl, 
               sampleControl=getControl(x, which="sample"), nrep=getNrep(x), 
               control=control)
  })
