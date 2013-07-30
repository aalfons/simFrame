# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

setMethod(
  "subset", "SimResults", 
  function(x, data = NULL, cont = NULL, NARate = NULL, select = NULL, ...) {
    # initializations
    values <- getValues(x)
    cn <- getColnames(x)
    info <- setdiff(names(values), cn)
    ninfo <- getInfo(x)
    dataControl <- getControl(x, which="data")
    control <- getControl(x)
    # find subset of observations
    subset <- NULL
    if(!is.null(data) && "Data" %in% info) {
      data <- seq_along(dataControl)[data]
      ninfo["Data"] <- length(data)
      subset <- values$Data %in% data
    }
    if(!is.null(cont) && "Cont" %in% info) {
      contControl <- getControl(control, which="cont")
      cont <- seq_along(contControl)[cont]
      ninfo["Cont"] <- length(cont)
      tmp <- values$Cont %in% cont
      subset <- if(is.null(subset)) tmp else tmp & subset
    }
    if(!is.null(NARate) && "NARate" %in% info) {
      NAControl <- getControl(control, which="NA")
      NARate <- convertNARate(getNARate(NAControl))[NARate]
      ninfo["NARate"] <- length(NARate)
      tmp <- values$NARate %in% NARate
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
               info=ninfo, dataControl=dataControl, 
               sampleControl=getControl(x, which="sample"), 
               nrep=getNrep(x), control=control)
  })
