# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

## for convenience: construct "SimControl" object and re-call function
setMethod(
  "runSimulation", 
  signature(x = "ANY", setup = "ANY", nrep = "ANY", control = "missing"),
  function(x, setup = NULL, nrep = 1, control, contControl = NULL, 
           NAControl = NULL, design = character(), fun, ..., seed, 
           ncores = 1, cl = NULL) {
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
    runSimulation(x, setup, nrep, control, ncores=ncores, cl=cl)
  })


## model-based simulation

setMethod(
  "runSimulation", 
  signature(x = "VirtualDataControl", setup = "ANY", 
            nrep = "ANY", control = "SimControl"),
  function(x, setup = NULL, nrep = 1, control, contControl = NULL, 
           NAControl = NULL, design = character(), fun, ..., seed, 
           ncores = 1, cl = NULL) {
    # initializations
    ndata <- length(x)
    nrep <- rep(as.integer(nrep), length.out=1)
    if(is.na(nrep) || nrep < 0) nrep <- 1
    if(nrep == 0) {
      # nothing to do
      return(SimResults(dataControl=x, nrep=nrep, control=control))
    }
    r <- rep.int(seq_len(ndata), nrep)
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
    getSimResults(tmp, dataControl=x, nrep=nrep, control=control)
  })

# 'i' as first argument is necessary for parallel computing with 'parLapply'
modelSimulation <- function(i, x, control) {
  md <- try(generate(x, i))
  if(class(md) == "try-error") return(getEmptyResults(control))
  design <- getDesign(control)
  if(length(design)) {
    spl <- getStrataSplit(md, design, USE.NAMES=FALSE)
    leg <- getStrataLegend(md, design)
    mdSpl <- lapply(spl, function(s, x) x[s, , drop=FALSE], md)
    manageSimulationByDomain(mdSpl, spl, control, leg)
  } else manageSimulation(md, control)
}


## design-based simulation

setMethod(
  "runSimulation", 
  signature(x = "data.frame", setup = "VirtualSampleControl", 
            nrep = "ANY", control = "SimControl"),
  function(x, setup = NULL, nrep = 1, control, contControl = NULL, 
           NAControl = NULL, design = character(), fun, ..., seed, 
           ncores = 1, cl = NULL) {
    # seed of the random number is first set by generic function 'setup'
    setup <- setup(x, setup)
    # seed is reset in the next method of 'runSimulation' (unless parallel 
    # computing is used, then the seed is set for random number streams)
    runSimulation(x, setup, control=control, ncores=ncores, cl=cl)
  })

setMethod(
  "runSimulation", 
  signature(x = "data.frame", setup = "SampleSetup", 
            nrep = "ANY", control = "SimControl"),
  function(x, setup = NULL, nrep = 1, control, contControl = NULL, 
           NAControl = NULL, design = character(), fun, ..., seed, 
           ncores = 1, cl = NULL) {
    # initializations
    nsamp <- length(setup)
    if(nsamp == 0) {
      # nothing to do
      return(SimResults(sampleControl=getControl(setup), control=control))
    }
    s <- seq_len(nsamp)
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
    getSimResults(tmp, sampleControl=getControl(setup), control=control)
  })

# internal function also used for parallel computing with 'parLapply'
designSimulation <- function(i, x, setup, control) {
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
            nrep = "ANY", control = "SimControl"),
  function(x, setup = NULL, nrep = 1, control, contControl = NULL, 
           NAControl = NULL, design = character(), fun, ..., seed, 
           ncores = 1, cl = NULL) {
    # initializations
    ndata <- length(x)
    nrep <- rep(as.integer(nrep), length.out=1)
    if(is.na(nrep) || nrep < 0) nrep <- 1
    nsamp <- length(setup)
    setSeed(setup, integer())  # do not reset random seed when drawing samples
    if(nrep == 0 || nsamp == 0) {
      # nothing to do
      return(SimResults(dataControl=x, sampleControl=setup, nrep=nrep, 
                        control=control))
    }
    r <- rep.int(seq_len(ndata), nrep)
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
                  nrep=nrep, control=control)
  })

# internal function also used for parallel computing with 'parLapply'
mixedSimulation <- function(i, x, setup, control) {
  # generate data
  md <- try(generate(x, i))
  nsamp <- length(setup)
  if(class(md) == "try-error") {
    # return empty results for each sample
    return(replicate(nsamp, getEmptyResults(control), simplify=FALSE))
  }
  # set up samples (empty vector in case of error in one iteration)
  setup <- setup(md, setup)
  # run design-based simulations on the generated data with the set up samples
  s <- seq_len(nsamp)
  lapply(s, designSimulation, md, setup, control)
}


## simulation with repetitions based on (possibly) real data
setMethod(
  "runSimulation",
  signature(x = "data.frame", setup = "ANY", 
            nrep = "ANY", control = "SimControl"),
  function(x, setup = NULL, nrep = 1, control, contControl = NULL, 
           NAControl = NULL, design = character(), fun, ..., seed, 
           ncores = 1, cl = NULL) {
    # initializations
    nrep <- rep(as.integer(nrep), length.out=1)
    if(is.na(nrep) || nrep < 0) nrep <- 1
    if(nrep == 0) {  # nothing to do
      return(SimResults(nrep=nrep, control=control))
    }
    design <- getDesign(control)
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
      r <- seq_len(nrep)
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
    getSimResults(tmp, nrep=nrep, control=control)
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
