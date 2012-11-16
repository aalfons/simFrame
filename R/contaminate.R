# ----------------------
# Author: Andreas Alfons
#         KU Leuven
# ----------------------

## FIXME: if epsilon is 0, the tuning parameters need to be ignored
##        otherwise simulations are unnecessarily run for each combinations of 
##        tuning parameters even though nothing is contaminated in each case

## internal S3 function 
# this is used in 'runSimulation' and 'clusterRunSimulation': there the 
# objects are already checked for validity and this speeds things up slightly
contaminateS3 <- function(x, control, i = 1) {
  # initializations
  epsilon <- getEpsilon(control)
  tuning <- getTuning(control)
  nTuning <- nrow(tuning)
  if(nTuning == 0) epsilon <- epsilon[i]
  else {
    epsilon <- epsilon[(i - 1) %/% nTuning + 1]
    tuning <- tuning[(i - 1) %% nTuning + 1,]
  }
#   if(epsilon == 0 || any(dim(x) == 0)) return(x)  # nothing to do
  if(epsilon == 0 || nrow(x) == 0) {
    attr(x, "contaminated") <- rep.int(FALSE, nrow(x))
    return(x)
  } else if(ncol(x) == 0) return(x)  # nothing to do
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
      split <- split(1:Ntotal, getFactor(groups))
      N <- length(split)
    } else {
      uniqueGroups <- unique(groups)  # unique groups
      N <- length(uniqueGroups)  # number of groups
    }
  } else N <- nrow(x)
  n <- ceiling(epsilon * N)
  if(useAux) {  # prepare auxiliary variable
    aux <- x[, aux]
#     if(!is.numeric(aux)) {
#       stop("slot 'aux' in 'control' must specify a numeric variable.")
#     }
#     if(!all(is.finite(aux))) {
#       stop("variable definted by slot 'aux' in 'control'", 
#            "must not contain missing or infinite values.")
#     }
#     if(any(aux < 0)) aux <- aux - min(aux)  # add small value?
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
  if(is(control, "DCARContControl")) {
    values <- doCallTuning(getDistribution(control), n, 
                           tuning, getDots(control))
    if(useGroup) {
      rep <- unsplit(1:n, getFactor(groups[ind]))  # replication indices
      values <- if(is.null(dim(values))) values[rep] else values[rep,]
    }
  } else if(is(control, "DARContControl")) {
    dots <- c(list(x[ind, target]), tuning, getDots(control))
    values <- do.call(getFun(control), dots)
  } else {
    stop("for user defined contamination control classes, a ", 
         "method 'contaminate(x, control, i)' needs to be defined")
  }
  values <- as.data.frame(values)
  # set contaminated values and return x
  x[ind, target] <- values
  if(is.null(contaminated <- attr(x, "contaminated"))) {
    contaminated <- logical(nrow(x))
    contaminated[ind] <- TRUE
  } else contaminated[ind] <- TRUE
  attr(x, "contaminated") <- contaminated
  x
}


## S4 methods

setMethod("contaminate",
          signature(x = "data.frame", control = "ContControl"),
          contaminateS3)

setMethod("contaminate", 
          signature(x = "data.frame", control = "character"), 
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

setMethod("contaminate",
          signature(x = "data.frame", control = "missing"),
          function(x, control, ...) {
            contaminate(x, DCARContControl(...))
          })
