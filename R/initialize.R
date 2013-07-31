# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

# model based data
setMethod(
  "initialize", "DataControl", 
  function(.Object, ...) {
    args <- list(...)
    # use normal distribution as default
    if(is.null(args$distribution)) setDistribution(.Object, rnorm)
    # make sure that tuning parameters are stored as data frame
    tuning <- args$tuning
    if(is.list(tuning)) {
      setTuning(.Object, tuning)
      tuning <- getTuning(.Object)
      args$tuning <- NULL
    }
    # set indices for sample size and tuning parameters
    size <- args$size
    setIndices(.Object, size, tuning)
    args$indices <- NULL
    # add current object to arguments and call method for superclass
    args[[".Object"]] <- .Object
    do.call(callNextMethod, args)
  })

# sampling
setMethod(
  "initialize", "VirtualSampleControl", 
  function(.Object, ...) {
    args <- list(...)
    # use simple random sampling as default
    if(is.null(args$seed)) setSeed(.Object, as.integer(Sys.time()))
    callNextMethod()  # call method for superclass (or default)
  })

# basic sampling designs
setMethod(
  "initialize", "BasicSampleControl", 
  function(.Object, ...) {
    args <- list(...)
    # use simple random sampling as default
    if(is.null(args$fun)) setFun(.Object, srs)
    callNextMethod()  # call method for superclass (or default)
  })

# two-stage sampling designs
setMethod(
  "initialize", "TwoStageSampleControl", 
  function(.Object, ...) {
    args <- list(...)
    # use simple random sampling as default
    if(is.null(args$fun)) setFun(.Object, list(srs, srs))
    callNextMethod()  # call method for superclass (or default)
  })

# contamination
setMethod(
  "initialize", "ContControl", 
  function(.Object, ...) {
    args <- list(...)
    # make sure that tuning parameters are stored as data frame
    tuning <- args$tuning
    if(is.list(tuning)) {
      setTuning(.Object, tuning)
      tuning <- getTuning(.Object)
      args$tuning <- NULL
    }
    # set indices for contamination level and tuning parameters
    epsilon <- args$epsilon
    setIndices(.Object, epsilon, tuning)
    args$indices <- NULL
    # add current object to arguments and call method for superclass
    args[[".Object"]] <- .Object
    do.call(callNextMethod, args)
  })

# simple contamination
setMethod(
  "initialize", "BasicContControl", 
  function(.Object, ...) {
    args <- list(...)
    # use normal distribution as default for contamination data
    if(is.null(args$fun)) setFun(.Object, function(...) "+"(...))
    callNextMethod()  # call method for superclass (or default)
  })

# contamination completely at random (CCAR)
setMethod(
  "initialize", "CCARContControl", 
  function(.Object, ...) {
    args <- list(...)
    # use normal distribution as default for contamination data
    if(is.null(args$distribution)) setDistribution(.Object, rnorm)
    callNextMethod()  # call method for superclass (or default)
  })

# contamination at random (CAR)
setMethod(
  "initialize", "CCARContControl", 
  function(.Object, ...) {
    args <- list(...)
    # use normal distribution as default for contamination data
    if(is.null(args$fun)) setFun(.Object, function(...) "+"(...))
    callNextMethod()  # call method for superclass (or default)
  })

# simulation control
setMethod(
  "initialize", "SimControl", 
  function(.Object, ...) {
    args <- list(...)
    # use simple random sampling as default
    if(is.null(args$seed)) setSeed(.Object, as.integer(Sys.time()))
    callNextMethod()  # call method for superclass (or default)
  })
