# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

## for convenience: construct "SimControl" object and re-call function
setMethod(
  "runSimulation", 
  signature(x = "ANY", setup = "ANY", nrep = "ANY", control = "missing"),
  function(x, setup, nrep, control, contControl = NULL, NAControl = NULL, 
           design = character(), fun, ..., seed, ncores = 1, cl = NULL) {
    # construct control object
    if(missing(seed)) {
      control <- SimControl(contControl=contControl, NAControl=NAControl, 
                            design=design, fun=fun, dots=list(...))
    } else {
      control <- SimControl(contControl=contControl, NAControl=NAControl, 
                            design=design, fun=fun, dots=list(...), 
                            seed=seed)
    }
    # call other method
    if(missing(setup)) {
      if(missing(nrep)) runSimulation(x, control=control, ncores=ncores, cl=cl)
      else runSimulation(x, nrep=nrep, control=control, ncores=ncores, cl=cl)
    } else {
      if(missing(nrep)) runSimulation(x, setup, control=control, 
                                      ncores=ncores, cl=cl)
      else runSimulation(x, setup, nrep, control, ncores=ncores, cl=cl)
    }
  })


## model-based simulation

setMethod(
  "runSimulation", 
  signature(x = "VirtualDataControl", setup = "missing", 
            nrep = "numeric", control = "SimControl"),
  function(x, setup, nrep, control, contControl = NULL, NAControl = NULL, 
           design = character(), fun, ..., seed, ncores = 1, cl = NULL) {
    # initializations
    if(length(nrep) == 0) stop("'nrep' must be a non-negative integer")
    else if(length(nrep) > 1) nrep <- nrep[1]
    if(nrep == 0) {  # nothing to do
      return(SimResults(design=getDesign(control), dataControl=x, 
                        nrep=nrep, control=control))
    }
    r <- seq_len(nrep)
    seed <- getSeed(control)
    # set up cluster for parallel computing if requested
    cl <- startCluster(ncores, cl)
    # run the simulations
    if(is.null(cl)) {
      set.seed(seed)
      tmp <- lapply(r, modelSimulation, x, control)
    } else {
      if(attr(cl, "stopOnExit")) on.exit(stopCluster(cl))
      clusterSetRNGStream(cl, iseed=seed)
      tmp <- parLapply(cl, r, modelSimulation, x, control)
    }
    # construct results
    getSimResults(tmp, dataControl=x, reps=r, control=control)
  })

# 'i' as first argument is necessary for parallel computing with 'parLapply'
modelSimulation <- function(i, x, control) {
#   print(paste(Sys.time(), i, sep=": "))
  # initializations
  ndata <- length(x)
  # generate data and run simulations and run simulations
  tmp <- lapply(seq_len(ndata), function(d) {
    md <- try(generate(x, d))
    if(class(md) == "try-error") return(getEmptyResults(control))
    design <- getDesign(control)
    if(length(design)) {
      spl <- getStrataSplit(md, design, USE.NAMES=FALSE)
      leg <- getStrataLegend(md, design)
      mdSpl <- lapply(spl, function(s, x) x[s, , drop=FALSE], md)
      manageSimulationByDomain(mdSpl, spl, control, leg)
    } else manageSimulation(md, control)
  })
  do.call(c, tmp)
}


## design-based simulation

setMethod(
  "runSimulation", 
  signature(x = "data.frame", setup = "VirtualSampleControl", 
            nrep = "missing", control = "SimControl"),
  function(x, setup, nrep, control, contControl = NULL, NAControl = NULL, 
           design = character(), fun, ..., seed, ncores = 1, cl = NULL) {
    # seed of the random number is first set by generic function 'setup'
    setup <- setup(x, setup)
    # seed is reset in the next method of 'runSimulation' (unless parallel 
    # computing is used, then the seed is set for random number streams)
    runSimulation(x, setup, control=control, ncores=ncores, cl=cl)
  })

setMethod(
  "runSimulation", 
  signature(x = "data.frame", setup = "SampleSetup", 
            nrep = "missing", control = "SimControl"),
  function(x, setup, nrep, control, contControl = NULL, NAControl = NULL, 
           design = character(), fun, ..., seed, ncores = 1, cl = NULL) {
    # initializations
    nsam <- length(setup)
    if(nsam == 0) {  # nothing to do
      return(SimResults(design=getDesign(control), 
                        sampleControl=getControl(setup), 
                        control=control))
    }
    s <- seq_len(nsam)
    seed <- getSeed(control)
    # set up cluster for parallel computing if requested
    cl <- startCluster(ncores, cl)
    # run the simulations
    if(is.null(cl)) {
      set.seed(seed)
      tmp <- lapply(s, designSimulation, x, setup, control)
    } else {
      if(attr(cl, "stopOnExit")) on.exit(stopCluster(cl))
      clusterSetRNGStream(cl, iseed=seed)
      tmp <- parLapply(cl, s, designSimulation, x, setup, control)
    }
    # construct results
    getSimResults(tmp, sampleControl=getControl(setup), samples=s, 
                  control=control)
  })

# internal function also used for parallel computing with 'parLapply'
designSimulation <- function(i, x, setup, control) {
#   print(paste(Sys.time(), i, sep=": "))
  sam <- drawS3(x, setup, i)
  if(nrow(sam) == 0) return(getEmptyResults(control))
  design <- getDesign(control)
  if(length(design)) {
    spl <- getStrataSplit(sam, design, USE.NAMES=FALSE)
    leg <- getStrataLegend(sam, design)
    samSpl <- lapply(spl, function(s, x) x[s, , drop=FALSE], sam)
    manageSimulationByDomain(samSpl, spl, control, leg)
  } else manageSimulation(sam, control)
}


## mixed simulation designs

setMethod(
  "runSimulation", 
  signature(x = "VirtualDataControl", setup = "VirtualSampleControl", 
            nrep = "numeric", control = "SimControl"),
  function(x, setup, nrep, control, contControl = NULL, NAControl = NULL, 
           design = character(), fun, ..., seed, ncores = 1, cl = NULL) {
    # initializations
    if(length(x) > 1) {
      warning("multiple data configurations currently not implemented")
    }
    if(length(nrep) == 0) stop("'nrep' must be a non-negative integer")
    else if(length(nrep) > 1) nrep <- nrep[1]
    nsam <- length(setup)
    setSeed(setup, integer())  # do not reset random seed when drawing samples
    if(nrep == 0 || nsam == 0) {
      # nothing to do
      return(SimResults(design=getDesign(control), dataControl=x, 
                        sampleControl=setup, nrep=nrep, control=control))
    }
    r <- seq_len(nrep)
    seed <- getSeed(control)
    # set up cluster for parallel computing if requested
    cl <- startCluster(ncores, cl)
    # run the simulations (generate data repeatedly and draw samples)
    if(is.null(cl)) {
      set.seed(seed)
      tmp <- lapply(r, mixedSimulation, x, setup, control)
    } else {
      if(attr(cl, "stopOnExit")) on.exit(stopCluster(cl))
      clusterSetRNGStream(cl, iseed=seed)
      tmp <- parLapply(cl, r, mixedSimulation, x, setup, control)
    }
    # construct results
    getSimResults(tmp, dataControl=x, sampleControl=setup, 
                  samples=seq_len(nsam), reps=r, control=control)
  })

# internal function also used for parallel computing with 'parLapply'
mixedSimulation <- function(i, x, setup, control) {
#   print(paste(Sys.time(), i, sep=": "))
  # generate data
  md <- try(generate(x))
  nsam <- length(setup)
  if(class(md) == "try-error") {
    # return empty results for each sample
    return(replicate(nsam, getEmptyResults(control), simplify=FALSE))
  }
  # set up samples (empty vector in case of error in one iteration)
  setup <- setup(md, setup)
  # run design-based simulations on the generated data with the set up samples
  s <- seq_len(nsam)
  lapply(s, designSimulation, md, setup, control)
}


## simulation with repetitions based on (possibly) real data
setMethod(
  "runSimulation",
  signature(x = "data.frame", setup = "missing", 
            nrep = "numeric", control = "SimControl"),
  function(x, setup, nrep, control, contControl = NULL, NAControl = NULL, 
           design = character(), fun, ..., seed, ncores = 1, cl = NULL) {
    # initializations
    if(length(nrep) == 0) stop("'nrep' must be a non-negative integer")
    else if(length(nrep) > 1) nrep <- nrep[1]
    design <- getDesign(control)
    if(nrep == 0) {  # nothing to do
      return(SimResults(design=design, nrep=nrep, control=control))
    }
    seed <- getSeed(control)
    # set up cluster for parallel computing if requested
    cl <- startCluster(ncores, cl)
    # get results (adjustments are needed for parallel computing)
    if(is.null(cl)) {
      set.seed(seed)
      if(length(design)) {
        spl <- getStrataSplit(x, design, USE.NAMES=FALSE)
        leg <- getStrataLegend(x, design)
        xSpl <- lapply(spl, function(s, x) x[s, , drop=FALSE], x)
        tmp <- replicate(nrep, 
                         manageSimulationByDomain(xSpl, spl, control, leg), 
                         simplify=FALSE)
      } else tmp <- replicate(nrep, manageSimulation(x, control), 
                              simplify=FALSE)
    } else {
      if(attr(cl, "stopOnExit")) on.exit(stopCluster(cl))
      clusterSetRNGStream(cl, iseed=seed)
      # get results
      r <- 1:nrep
      if(length(design)) {
        # necessary objects need to be constructed on workers
        seqList <- clusterSplit(cl, r)
        nrList <- lapply(seqList, length)
        tmp <- clusterApply(cl, nrList, function(nr, x, control) {
          design <- getDesign(control)
          spl <- getStrataSplit(x, design, USE.NAMES=FALSE)
          xSpl <- lapply(spl, function(s, x) x[s, , drop=FALSE], x)
          leg <- getStrataLegend(x, design)
          replicate(nr, manageSimulationByDomain(xSpl, spl, control, leg), 
                    simplify=FALSE)
        }, x, control)
        tmp <- do.call(c, tmp)
      } else tmp <- parLapply(cl, r, function(i) manageSimulation(x, control))
    }
    # construct results
    getSimResults(tmp, reps=seq_len(nrep), control=control)
  })


## no samples, no repetitions (for sensitiviy analysis)
setMethod(
  "runSimulation",
  signature(x = "data.frame", setup = "missing", 
            nrep = "missing", control = "SimControl"),
  function(x, setup, nrep, control, contControl = NULL, NAControl = NULL, 
           design = character(), fun, ..., seed, ncores = 1, cl = NULL) {
    runSimulation(x, nrep=1, control=control)
  })


## workhorse function for simulations
manageSimulation <- function(x, control) {
  # initializations
  contControl <- getControl(control, which="cont")
  neps <- length(contControl)
  NAControl <- getControl(control, which="NA")
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


## workhorse function for simulations broken down by domain
manageSimulationByDomain <- function(xs, indices, control, legend) {
  # initializations
  contControl <- getControl(control, which="cont")
  neps <- length(contControl)
  NAControl <- getControl(control, which="NA")
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
