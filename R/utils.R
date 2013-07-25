# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

# get argument names of a function
argNames <- function(fun, removeDots = TRUE) {
  nam <- names(formals(fun))
  if(removeDots) nam <- setdiff(nam, "...")
  nam
}

# check for errors in a list (for sample setup)
checkError <- function(x) {
  if(length(x) > 0) sapply(x, function(x) class(x) == "try-error")
  else logical()
}

# check if data is numeric
checkNumericMatrix <- function(x) {
  if(is(x, "numeric")) TRUE 
  else if(is(x, "matrix")) mode(x) == "numeric"
  else FALSE  # other classes
}

# check for errors or empty vectors in a list (for simulation results)
checkOK <- function(x) {
  if(length(x) > 0) {
    sapply(x, function(x) class(x) != "try-error" && length(x) > 0)
  } else logical()
}

# check the 'stage' argument of accessors for two-stage sampling designs
checkStage <- function(stage) {
  if(!isTRUE(stage == 1) && !isTRUE(stage == 2)) {
    stop("'stage' must be either 1 or 2")
  }
}

# get NA rate for simulation results
convertNARate <- function(x) {
  if(is(x, "numeric")) x
  else if(is(x, "matrix")) seq_len(nrow(x))
  else numeric()  # other classes
}

## get matrix of indices in a vector and a data frame with tuning parameters
convertToIndices <- function(x, tuning, checkZero = TRUE) {
  # obtain indices
  nTuning <- nrow(tuning)
  nX <- length(x)
  if(is.null(nTuning) || nTuning == 0) {
    indices <- cbind(seq_len(nX), rep.int(0, nX))
  } else {
    if(checkZero) {
      isZero <- x == 0
      i <- rep.int(seq_len(nX), ifelse(isZero, 1, nTuning))
      j <- unlist(list(seq_len(nTuning), 0)[isZero+1])
    } else {
      i <- rep(seq_len(nX), each=nTuning)
      j <- rep.int(seq_len(nTuning), nX)
    }
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

# function to expand a vector according to groups
# unsplit does not do the right thing here as it expects the vector to be in 
# the order of the factor levels, but we have order of first occurence
expand <- function(x, groups, unique) {
  names(x) <- as.character(unique)
  x <- x[as.character(groups)]
  unname(x)
}

# get character eqivalent of a selection vector
getCharacter <- function(x, names) {
  if(is(x, "character")) x
  else if((is(x, "logical") || is(x, "numeric")) && length(x) > 0) names[x]
  else character()  # other classes
}

## get empty results
getEmptyResults <- function(control) {
  neps <- length(getControl(control, which="cont"))
  if(neps == 0) neps <- 1
  nNA <- length(getControl(control, which="NA"))
  if(nNA == 0) nNA <- 1
  replicate(neps*nNA, numeric())
}

# get names of real columns of a data.frame 
# (i.e., remove those used internally by simFrame)
getNames <- function(x) setdiff(names(x), c(".weight",".contaminated"))

# get number of repetitions for additional information
getRepetitions <- function(x) {
  if(is(x, "numeric")) if(length(x)) 1 else 0 
  else if(is(x, "data.frame") || is(x, "matrix")) nrow(x)
  else integer()  # other classes
}

# get length of specified selection
getSelectionLength <- function(x) {
  if(is(x, "character")) length(x)
  else if(is(x, "numeric") && all(x >= 0)) length(x[x > 0])
  else NA  # other classes
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
                          nrep = integer(), control) {
  # initializations
  ndata <- length(dataControl)
  nsamp <- length(sampleControl)
  origNrep <- nrep
  if(length(nrep) == 0) nrep <- 0
  contControl <- getControl(control, which="cont")
  ncont <- length(contControl)
  NAControl <- getControl(control, which="NA")
  nNA <- length(NAControl)
  # combine results from all runs into one list
  if(nsamp && nrep) x <- do.call(c, x)
  if(ncont || nNA) x <- do.call(c, x)
  # check for errors or empty results
  nruns <- length(x)
  ok <- checkOK(x)
  x <- x[ok]
  if(length(x) == 0) stop("error or empty result in every simulation run")
  # get additional information (at least one of 'nrep' or 'nsamp' is positive)
  ca <- call("expand.grid")  # initialize call
  if(nNA > 0) ca$NARate <- convertNARate(getNARate(NAControl))
  if(ncont > 0) ca$Cont <- seq_len(ncont)
  if(nsamp > 0) ca$Sample <- seq_len(nsamp)
  if(ndata > 0) ca$Data <- seq_len(ndata)
  if(nrep > 0) ca$Rep <- seq_len(nrep)
  info <- eval(ca)  # create data.frame with additional information
  info <- info[, ncol(info):1, drop=FALSE]  # reverse column order
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
  SimResults(values=x, colnames=nam, dataControl=dataControl, 
             sampleControl=sampleControl, nrep=origNrep, 
             control=control)
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

# get stratum sizes
getStratumSizes <- function(x, design, USE.NAMES = TRUE) {
  if(is(x, "data.frame")) x <- getStrataSplit(x, design)
  sapply(x, length, USE.NAMES=USE.NAMES)
}

# evaluate calls to sample methods with stratification and group sampling
simEval <- function(call, split, groups, unique) {
  if(!missing(split)) {
    if(missing(groups)) {
      # 'call' returns list of within-strata indices.
      tmp <- eval(call)
    } else {
      # 'groups' and 'unique' are lists
      # 'call' returns list of within-strata indices of groups. these 
      # are used to obtain the within-strata indices of individuals.
      tmp <- mapply(function(i, g, u) which(g %in% u[i]), eval(call), 
                    groups, unique, SIMPLIFY=FALSE, USE.NAMES=FALSE)
    }
    # within-strata indices are turned into global indices 
    # and the resulting list is converted to a vector.
    unlist(mapply("[", split, tmp, SIMPLIFY=FALSE, USE.NAMES=FALSE))
  } else if(!missing(groups)) {  # only 'groups' is not missing
    # 'groups' and 'unique' are vectors
    # 'call' returns list of groups. these are
    # used to obtain the indices of individuals.
    which(groups %in% unique[eval(call)])
  } else eval(call)
}

# start cluster for parallel computing
startCluster <- function(ncores = 1, cl = NULL) {
  # set up parallel computing if requested
  haveCl <- inherits(cl, "cluster")
  if(haveCl) {
    haveNcores <- FALSE
    attr(cl, "stopOnExit") <- FALSE
  } else {
    if(is.na(ncores)) ncores <- detectCores()  # use all available cores
    if(!is.numeric(ncores) || is.infinite(ncores) || ncores < 1) {
      # use default value
      ncores <- formals()$ncores
      warning("invalid value of 'ncores'; using default value")
    }
    ncores <- as.integer(ncores)
    haveNcores <- ncores > 1
  }
  # set up multicore or snow cluster if not supplied
  if(haveNcores) {
    if(.Platform$OS.type == "windows") {
      cl <- makePSOCKcluster(rep.int("localhost", ncores))
    } else cl <- makeForkCluster(ncores)
    attr(cl, "stopOnExit") <- TRUE
  }
  # return cluster for parallel computing
  cl
}
