# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

## for convenience: construct "SimControl" object and re-call function
setMethod(
  "runSimulation", 
  signature(x = "ANY", setup = "ANY", nrep = "ANY", control = "missing"),
  function(x, setup, nrep, control, contControl = NULL, 
           NAControl = NULL, design = character(), fun, ...) {
    control <- SimControl(contControl=contControl, NAControl=NAControl, 
                          design=design, fun=fun, dots=list(...))
    if(missing(setup)) {
      if(missing(nrep)) runSimulation(x, control=control)
      else runSimulation(x, nrep=nrep, control=control)
    } else {
      if(missing(nrep)) runSimulation(x, setup, control=control)
      else runSimulation(x, setup, nrep, control)
    }
  })


## model-based simulation
setMethod(
  "runSimulation", 
  signature(x = "VirtualDataControl", setup = "missing", 
            nrep = "numeric", control = "SimControl"),
  function(x, setup, nrep, control, contControl = NULL, NAControl = NULL, 
           design = character(), fun, ...) {
    # initializations
    if(length(nrep) == 0) stop("'nrep' must be a non-negative integer")
    else if(length(nrep) > 1) nrep <- nrep[1]
    design <- getDesign(control)
    if(nrep == 0) {  # nothing to do
      return(SimResults(design=design, dataControl=x, 
                        nrep=nrep, control=control))
    }
    # run the simulations
    r <- seq_len(nrep)
    tmp <- lapply(r, modelSimulation, x, control)
    # construct results
    getSimResults(tmp, dataControl=x, reps=r, control=control)
  })


# 'i' as first argument is necessary for parallel computing  with 'parLapply'
modelSimulation <- function(i, x, control) {
  md <- try(generate(x))
  if(class(md) == "try-error") return(getEmptyResults(control))
  design <- getDesign(control)
  if(length(design)) {
    spl <- getStrataSplit(md, design, USE.NAMES=FALSE)
    leg <- getStrataLegend(md, design)
    mdSpl <- lapply(spl, function(s, x) x[s, , drop=FALSE], md)
    manageSimulationByDomain(mdSpl, spl, control, leg)
  } else manageSimulation(md, control)
}


## workhorse function for simulations
manageSimulation <- function(x, control) {
  # initializations
  contControl <- getContControl(control)
  neps <- length(contControl)
  NAControl <- getNAControl(control)
  nNA <- length(NAControl)
  fun <- getFun(control)
  useOrig <- "orig" %in% argNames(fun)
  dots <- getDots(control)
  # get results
  if(neps) {
    if(nNA) {
      # contamination, missings
      tmp <- lapply(seq_len(neps), 
                    function(e) {
                      cx <- try(contaminate(x, contControl, e))
                      if(class(cx) == "try-error") return(vector("list", nNA))
                      lapply(seq_len(nNA), 
                             function(n) try({
                               nx <- setNA(cx, NAControl, n)
                               ca <- as.call(c(fun, dots))
                               ca$x <- nx
                               if(useOrig) ca$orig <- x
                               getSimResult(eval(ca))
                             }))
                    })
      do.call(c, tmp)
    } else {
      # contamination, no missings
      lapply(seq_len(neps), 
             function(e) try({
               cx <- contaminate(x, contControl, e)
               ca <- as.call(c(fun, dots))
               ca$x <- cx
               if(useOrig) ca$orig <- x
               getSimResult(eval(ca))
             }))
    }
  } else {
    if(nNA) {
      # no contamination, missings
      lapply(seq_len(nNA), 
             function(n) try({
               nx <- setNA(x, NAControl, n)
               ca <- as.call(c(fun, dots))
               ca$x <- nx
               if(useOrig) ca$orig <- x
               getSimResult(eval(ca))
             }))
    } else {
      # no contamination, no missings
      try({
        ca <- as.call(c(fun, dots))
        ca$x <- x
        if(useOrig) ca$orig <- x
        getSimResult(eval(ca))
      })
    }
  }
}


manageSimulationByDomain <- function(xs, indices, control, legend) {
  # initializations
  contControl <- getContControl(control)
  neps <- length(contControl)
  NAControl <- getNAControl(control)
  nNA <- length(NAControl)
  fun <- getFun(control)
  nam <- argNames(fun)
  useOrig <- "orig" %in% nam
  useDomain <- "domain" %in% nam
  dots <- getDots(control)
  # get results
  if(neps) {
    if(nNA) {
      # contamination, missings
      tmp <- lapply(seq_len(neps), 
                    function(e) {
                      cxs <- try(lapply(xs, contaminate, contControl, e))
                      if(class(cxs) == "try-error") return(vector("list", nNA))
                      lapply(seq_len(nNA), 
                             function(n) {
                               try({
                                 tmp <- mapply(
                                   function(cx, x, i) {
                                     nx <- setNA(cx, NAControl, n)
                                     ca <- as.call(c(fun, dots))
                                     ca$x <- nx
                                     if(useOrig) ca$orig <- x
                                     if(useDomain) ca$domain <- i
                                     getSimResult(eval(ca))
                                   }, cxs, xs, indices, 
                                   SIMPLIFY=FALSE, USE.NAMES=FALSE)
                                 getSimResultByDomain(tmp, legend)
                               })
                             })
                    })
      do.call(c, tmp)
    } else {
      # contamination, no missings
      lapply(seq_len(neps), 
             function(e) {
               try({
                 tmp <- mapply(
                   function(x, i) {
                     cx <- contaminate(x, contControl, e)
                     ca <- as.call(c(fun, dots))
                     ca$x <- cx
                     if(useOrig) ca$orig <- x
                     if(useDomain) ca$domain <- i
                     getSimResult(eval(ca))
                   }, xs, indices, 
                   SIMPLIFY=FALSE, USE.NAMES=FALSE)
                 getSimResultByDomain(tmp, legend)
               })
             })
    }
  } else {
    if(nNA) {
      # no contamination, missings
      lapply(seq_len(nNA), 
             function(n) {
               try({
                 tmp <- mapply(
                   function(x, i) {
                     nx <- setNA(x, NAControl, n)
                     ca <- as.call(c(fun, dots))
                     ca$x <- nx
                     if(useOrig) ca$orig <- x
                     if(useDomain) ca$domain <- i
                     getSimResult(eval(ca))
                   }, xs, indices, 
                   SIMPLIFY=FALSE, USE.NAMES=FALSE)
                 getSimResultByDomain(tmp, legend)
               })
             })
    } else {
      # no contamination, no missings
      try({
        tmp <- mapply(
          function(x, i) {
            ca <- as.call(c(fun, dots))
            ca$x <- x
            if(useOrig) ca$orig <- x
            if(useDomain) ca$domain <- i
            getSimResult(eval(ca))
          }, xs, indices, SIMPLIFY=FALSE, USE.NAMES=FALSE)
        getSimResultByDomain(tmp, legend)
      })
    }
  }
}
