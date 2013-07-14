# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

## utilities for class "BasicVector"

# get character eqivalent
getCharacter <- function(x, names) {
  if(is(x, "character")) x
  else if((is(x, "logical") || is(x, "numeric")) && length(x)) names[x]
  else character()  # other classes
}

# get length of specified selection
getSelectionLength <- function(x) {
  if(is(x, "character")) length(x)
  else if(is(x, "numeric") && all(x >= 0)) length(x[x > 0])
  else NA  # other classes
}

# ---------------------------------------

## utilities for class "NumericMatrix"

# check if data is numeric
checkNumericMatrix <- function(x) {
  if(is(x, "numeric")) TRUE 
  else if(is(x, "matrix")) mode(x) == "numeric"
  else FALSE  # other classes
}

# ----------------------------------

## other utilities

# get argument names of a function
argNames <- function(fun, removeDots = TRUE) {
  nam <- names(formals(fun))
  if(removeDots) nam <- setdiff(nam, "...")
  nam
}

# check for errors or empty vectors in a list (for simulation results)
checkOK <- function(x) {
  # x ... list
  if(length(x)) {
    sapply(x, function(x) class(x) != "try-error" && length(x))
  } else logical()
}

# get NA rate for simulation results
convertNARate <- function(x) {
  if(is(x, "numeric")) x
  else if(is(x, "matrix")) seq_len(nrow(x))
  else numeric()  # other classes
}

## get matrix of indices in a vector and a data frame with tuning parameters
convertToIndices <- function(x, tuning) {
  # obtain indices
  nTuning <- nrow(tuning)
  nX <- length(x)
  if(is.null(nTuning) || nTuning == 0) {
    indices <- cbind(seq_len(nX), rep.int(0, nX))
  } else {
    isZero <- x == 0
    i <- rep.int(seq_len(nX), ifelse(isZero, 1, nTuning))
    j <- unlist(list(seq_len(nTuning), 1)[isZero+1])
    indices <- cbind(i, j, deparse.level=0)
  }
  # return indices
  indices
}

# get tuning parameters for simulation results
convertTuning <- function(x) if(ncol(x) == 1) x[, 1] else seq_len(nrow(x)) 

## call a function by either
# 1) simply evaluating a supplied function for the first argument if there are
#    no additional arguments in list format
# 2) evaluating a supplied function with 'do.call' if there are additional 
#    arguments in list format
# 3) evaluating a supplied function with 'do.call' if there are tuning 
#    parameters in list format (if supplied, add additional arguments to 
#    the list)
doCall <- function(fun, first, tuning, dots = list()) {
  if(nrow(tuning) == 0) {
    if(length(dots) == 0) fun(first)
    else do.call(fun, c(first, dots))
  } else if(length(dots) == 0) do.call(fun, c(first, tuning))
  else do.call(fun, c(first, tuning, dots))
}

## get empty results
getEmptyResults <- function(control) {
  neps <- length(getContControl(control))
  if(neps == 0) neps <- 1
  nNA <- length(getNAControl(control))
  if(nNA == 0) nNA <- 1
  replicate(neps*nNA, list(values=numeric()))
}

# get names of real columns of a data.frame 
# (i.e., remove those used internally by simFrame)
getNames <- function(x) setdiff(names(x), c(".weight",".contaminated"))

# get number of repetitions for additional information
getRepetitions <- function(x) {
  if(is(x, "numeric")) if(length(x)) 1 else 0 
  else if(is(x, "data.frame") || is(x, "matrix")) nrow(x)
  else numeric()  # other classes
}

# get result of one simulation run in the correct format
getSimResult <- function(x) {
  if(is(x, "numeric")) x
  else if(is(x, "list")) x$values  # for backwards compatibility (deprecated)
  else stop("'fun' must return a numeric vector")
}

# get result for one stratified simulation run
getSimResultByDomain <- function(x, legend) {
  if(is(x[[1]], "list")) {
    x <- lapply(x, "[[", "values")  # for backwards compatibility (deprecated)
  }
  cbind(legend, do.call(rbind, x))
}

# contruct object to be returned
getSimResults <- function(x, dataControl = NULL, sampleControl = NULL, 
                          samples = numeric(), reps = numeric(), control) {
  # initializations
  nsam <- length(samples)
  origNrep <- if(missing(reps)) numeric() else length(reps)
  nrep <- length(reps)
  contControl <- getContControl(control)
  ncont <- length(contControl)
  if(ncont) {
    epsilon <- getEpsilon(contControl)
    contTuning <- convertTuning(getTuning(contControl))
    nTuning <- length(contTuning)
    contIndices <- getIndices(contControl)
  } else {
    epsilon <- numeric()
    nTuning <- 0
  }
  NAControl <- getNAControl(control)
  nNA <- length(NAControl)
  if(nNA) {
    origNARate <- getNARate(NAControl)
    NARate <- convertNARate(origNARate)
  } else origNARate <- numeric()
  # combine results from all runs into one list
  if(nsam && nrep) x <- do.call(c, x)
  if(ncont || nNA) x <- do.call(c, x)
  # check for errors or empty results
  nruns <- length(x)
  ok <- checkOK(x)
  x <- x[ok]
  if(length(x) == 0) stop("error or empty result in every simulation run")
  # get additional information (at least one of 'nrep' or 'nsam' is positive)
  ca <- call("expand.grid")  # initialize call
  if(nNA) ca$NARate <- NARate
  if(ncont) {
    if(nTuning) {
      ca$Index <- seq_len(ncont)
      cont <- data.frame(Epsilon=epsilon[contIndices[, 1]], 
                         ContTuning=contTuning[contIndices[, 2]])
    } else ca$Epsilon <- epsilon
  }
  if(nsam) ca$Sample <- samples
  if(nrep) ca$Rep <- reps
  info <- eval(ca)  # create data.frame with additional information
  if(nTuning) {
    if(nNA) {
      replace <- 2
      after <- 1
    } else {
      replace <- 1
      after <- 0
    }
    before <- if(nsam || nrep) ncol(info):(replace+1) else 0
    info <- cbind(info[, before, drop=FALSE], cont, info[, after, drop=FALSE])
  } else info <- info[, ncol(info):1, drop=FALSE]  # reverse column order
  info <- cbind(Run=seq_len(nruns), info)[ok, , drop=FALSE]  # add runs
  # additional information needs to be adjusted for stratified simulations
  design <- getDesign(control)
  if(length(design)) {
    reps <- sapply(x, getRepetitions)
    info <- info[rep.int(seq_len(nrow(info)), reps), , drop=FALSE]
  }
  # put it all together
  x <- cbind(info, do.call(rbind, x))
  ninfo <- ncol(info) + length(design)
  nam <- names(x)[-seq_len(ninfo)]
  rownames(x) <- NULL
  # return results
  SimResults(values=x, design=design, colnames=nam, epsilon=epsilon, 
             NARate=origNARate, dataControl=dataControl, 
             sampleControl=sampleControl, nrep=origNrep, control=control)
}

# get information about strata as data.frame
getStrataLegend <- function(x, design) {
  tab <- getStrataTable(x, design)
  tab[, -ncol(tab), drop=FALSE]
}

# get list of indices in the strata
getStrataSplit <- function(x, design, USE.NAMES = TRUE) {
  res <- split(seq_len(nrow(x)), x[, design])
  if(!USE.NAMES) names(res) <- NULL
  res
}

# get contincency table as data.frame
getStrataTable <- function(x, design) {
  tmp <- x[, design, drop=FALSE]
  ans <- as.data.frame(table(tmp))
  names(ans) <- c(names(tmp), "Size")
  ans
}
