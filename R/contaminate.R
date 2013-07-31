# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

setMethod(
  "contaminate", signature(x = "data.frame", control = "BasicContControl"),
  function(x, control, i = 1) {
    # initializations
    indices <- getIndices(control)
    epsilon <- getEpsilon(control)
    tuning <- getTuning(control)
    if(nrow(indices)) {
      epsilon <- epsilon[indices[i, 1]]
      tuning <- tuning[indices[i, 2], , drop=FALSE]
    }
    if(epsilon == 0 || nrow(x) == 0 || ncol(x) == 0) return(x)  # nothing to do
    target <- getTarget(control)
    if(is.null(target)) target <- getNames(x)
    # get indices of observations to be contaminated
    n <- ceiling(epsilon * nrow(x))
    ind <- seq_len(n)
    # get values for contamintated observations
    dots <- c(list(x[ind, target]), tuning, getDots(control))
    values <- do.call(getFun(control), dots)
    # set contaminated values and return x
    x[ind, target] <- values
    if(".contaminated" %in% names(x)) x[ind, ".contaminated"] <- TRUE
    else {
      contaminated <- logical(nrow(x))
      contaminated[ind] <- TRUE
      x$.contaminated <- contaminated
    }
    x
  })

setMethod(
  "contaminate", signature(x = "data.frame", control = "RandomContControl"),
  function(x, control, i = 1) {
    # initializations
    indices <- getIndices(control)
    epsilon <- getEpsilon(control)
    tuning <- getTuning(control)
    if(nrow(indices)) {
      epsilon <- epsilon[indices[i, 1]]
      tuning <- tuning[indices[i, 2], , drop=FALSE]
    }
    if(epsilon == 0 || nrow(x) == 0 || ncol(x) == 0) return(x)  # nothing to do
    target <- getTarget(control)
    if(is.null(target)) target <- getNames(x)
    grouping <- getGrouping(control)
    if(length(grouping) > 1) {
      stop("'grouping' must not specify more than one variable")
    }
    aux <- getAux(control)
    if(length(aux) > 1) {
      stop("'aux' must not specify more than one variable")
    }
    useGroup <- as.logical(length(grouping))  # 'grouping' supplied
    useAux <- as.logical(length(aux))  # 'aux' supplied
    # get population size and number of 
    # observations or groups to be contaminated
    if(useGroup) {
      groups <- x[, grouping]  # group of each individual
      if(useAux) {
        Ntotal <- nrow(x)
        split <- split(seq_len(Ntotal), getFactor(groups))
        N <- length(split)
      } else {
        uniqueGroups <- unique(groups)  # unique groups
        N <- length(uniqueGroups)  # number of groups
      }
    } else N <- nrow(x)
    n <- ceiling(epsilon * N)
    if(useAux) {  # prepare auxiliary variable
      aux <- x[, aux]
      if(useGroup) {
        # use the group means (much faster than medians)
        aux <- sapply(split, function(i) mean(aux[i]))
      }
      ind <- ups(N, n, prob=aux)  # get indices (unequal prob. sampling)
    } else ind <- srs(N, n)  # get indices (simple random sampling)
    if(useGroup) {
      if(useAux) ind <- unlist(split[ind])
      else ind <- which(groups %in% uniqueGroups[ind])  # row indices
    }
    if(is(control, "CCARContControl")) {
      values <- doCall(getDistribution(control), n, tuning, getDots(control))
      if(useGroup) {
        rep <- unsplit(seq_len(n), getFactor(groups[ind]))  # replication indices
        values <- if(is.null(dim(values))) values[rep] else values[rep,]
      }
    } else if(is(control, "CARContControl")) {
      dots <- c(list(x[ind, target]), tuning, getDots(control))
      values <- do.call(getFun(control), dots)
    } else {
      stop("for user defined contamination control classes, a ", 
           "method 'contaminate(x, control, i)' needs to be defined")
    }
    values <- as.data.frame(values)
    # set contaminated values and return x
    x[ind, target] <- values
    if(".contaminated" %in% names(x)) x[ind, ".contaminated"] <- TRUE
    else {
      contaminated <- logical(nrow(x))
      contaminated[ind] <- TRUE
      x$.contaminated <- contaminated
    }
    x
  })

setMethod(
  "contaminate", signature(x = "data.frame", control = "character"), 
  function(x, control, ...) {
    if(length(control) != 1) {
      stop("'control' must specify exactly one ", 
           "class extending \"VirtualContControl\"")
    }
    if(!extends(control, "VirtualContControl")) {
      stop(gettextf("\"%s\" does not extend \"VirtualContControl\"", 
                    control))
    }
    contaminate(x, new(control, ...))
  })

setMethod(
  "contaminate", signature(x = "data.frame", control = "missing"),
  function(x, control, ...) {
    contaminate(x, ContControl(...))
  })
