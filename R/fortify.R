# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

setMethod(
  "fortify", "SimResults", 
  function(model, data = NULL, cont = NULL, NARate = NULL, select = NULL, 
           method = c("box", "density", "line"), average = c("mean", "median"), 
           se = TRUE, ...) {
    ## initializations
    # take the requested subset of the results
    if(is(data, "function")) data <- NULL  # dirty hack (generic has no default)
    model <- subset(model, data=data, cont=cont, NARate=NARate, select=select)
    # extract variable names
    cn <- getColnames(model)
    ninfo <- getInfo(model)
    info <- names(ninfo)
    keep <- ninfo > 0
    ninfo <- ninfo[keep]
    info <- info[keep]
    control <- getControl(model)
    design <- getDesign(control)
    xvars <- setdiff(info, design)
    # check method
    if(missing(method)) {
      # select suitable plot method automatically
      method <- if(any(ninfo[xvars] > 1)) "line" else "box"
    } else {
      method <- match.arg(method)
      # check x-variable for line plots
      if(method == "line" && all(ninfo[xvars] <= 1)) {
        stop("no varying values for x-axis")
      }
    }
    ## extract simulation results
    if(method == "line") {
      average <- match.arg(average)
      if(average == "mean") {
        fun <- function(x) {
          # remove missing or infinite values
          x <- x[is.finite(x)]
          # compute mean
          xn <- mean(x)
          # compute approximate confidence interval
          q <- qnorm(0.975)
          s <- sd(x)
          n <- length(x)
          ci <- xn + c(-q, q) * s/sqrt(n)
          names(ci) <- c("Lower", "Upper")
          # return values
          c(Value=xn, ci)
        }
      } else {
        fun <- function(x) {
          # remove missing or infinite values
          x <- x[is.finite(x)]
          # compute median
          med <- median(x)
          # compute approximate confidence interval
          iqr <- IQR(x)
          n <- length(x)
          ci <- med + c(-1.58, 1.58) * iqr/sqrt(n)
          names(ci) <- c("Lower", "Upper")
          # return values
          c(Value=med, ci)
        }
      }
      values <- aggregate(model, FUN=fun)
    } else values <- getValues(model)[, c(info, cn), drop=FALSE]
    ## extract additional information
    # number of observations and tuning parameters for data generation
    remove <- match("Data", info, nomatch=0)
    if(remove > 0) {
      dataControl <- getControl(model, which="data")
      size <- getSize(dataControl)
      if(is(dataControl, "DataControl")) {
        indices <- getIndices(dataControl)
        size <- size[indices[, 1]]
        tuning <- convertTuning(getTuning(dataControl))[indices[, 2]]
      } else tuning <- NULL
      data <- values[, remove]
      if(length(tuning) == 0) {
        # no tuning parameters, replace data column with number of 
        # observations
        names(values)[remove] <- "Size"
        values[, remove] <- size[data]
        names(ninfo)[remove] <- "Size"
        info[remove] <- "Size"
      } else {
        # remove data column and add columns for number of observations and 
        # tuning parameters
        before <- if(remove == 1) 0 else seq_len(remove-1)
        remove <- seq_len(remove)
        values <- cbind(values[, before, drop=FALSE], 
                        Size=size[data], DataTuning=tuning[data], 
                        values[, -remove, drop=FALSE])
        ninfo <- c(ninfo[before], 
                   sapply(values[, c("Size", "DataTuning")], 
                          function(x) length(unique(x))), 
                   ninfo[-remove])
        info <- c(info[before], "Size", "DataTuning", info[-remove])
      }
    }
    # contamination level and tuning parameters for contamination
    remove <- match("Cont", info, nomatch=0)
    if(remove > 0) {
      contControl <- getControl(control, which="cont")
      epsilon <- getEpsilon(contControl)
      if(is(contControl, "ContControl")) {
        tuning <- getTuning(contControl)
        indices <- getIndices(contControl)
        # for contamination level 0, replicate results for all combinations of  
        # tuning parameters (those are of course only computed once)
        if(nrow(tuning) > 0 && 0 %in% epsilon) {
          isZero <- (epsilon == 0)[indices[, 1]]
          ntune <- nrow(tuning)
          by <- rep.int(seq_along(contControl), ifelse(isZero, ntune, 1))
          indices <- convertToIndices(epsilon, tuning, checkZero=FALSE)
          contList <- split(seq_len(nrow(indices)), by)
          valueList <- split(values, values[, remove])
          valueList <- mapply(function(values, cont, isZero) {
            n <- nrow(values)
            if(isZero) values <- values[rep(seq_len(n), ntune),]
            values$Cont <- rep.int(cont, n)
            values
          }, valueList, contList, isZero, SIMPLIFY=FALSE, USE.NAMES=FALSE)
          values <- do.call(rbind, valueList)
        }
        epsilon <- epsilon[indices[, 1]]
      } else tuning <- data.frame()
      # replace contamination column
      cont <- values[, remove]
      if(nrow(tuning) == 0) {
        # no tuning parameters, replace contamination column with 
        # contamination level
        names(values)[remove] <- "Epsilon"
        values[, remove] <- epsilon[cont]
        names(ninfo)[remove] <- "Epsilon"
        info[remove] <- "Epsilon"
      } else {
        # remove contamination column and add columns for contamination level 
        # and tuning parameters
        tuning <- convertTuning(tuning)[indices[,2]]
        before <- if(remove == 1) 0 else seq_len(remove-1)
        remove <- seq_len(remove)
        values <- cbind(values[, before, drop=FALSE], 
                        Epsilon=epsilon[cont], ContTuning=tuning[cont], 
                        values[, -remove, drop=FALSE])
        ninfo <- c(ninfo[before], 
                   sapply(values[, c("Epsilon", "ContTuning")], 
                          function(x) length(unique(x))), 
                   ninfo[-remove])
        info <- c(info[before], "Epsilon", "ContTuning", info[-remove])
      }
    }
    ## reshape simulation results
    values <- lapply(cn, function(j, info) {
      val <- cbind(Value=values[[j]])
      cbind(info, Method=rep.int(j, nrow(info)), val)
    }, info=values[, info, drop=FALSE])
    values <- do.call(rbind, values)
    rownames(values) <- NULL
    ## add additional information as attributes
    # add aesthetic mapping and plot geometry
    cond <- info[ninfo > 1]
    if(method == "box") {
      attr(values, "mapping") <- aes_string(x="Method", y="Value")
      attr(values, "geom") <- geom_boxplot
    } else if(method == "density") {
      attr(values, "mapping") <- aes_string(x="Value", color="Method")
      attr(values, "geom") <- geom_density
    } else {
      xvars <- setdiff(cond, design)
      whichMax <- which.max(ninfo[xvars])
      xvar <- cond[whichMax]
      cond <- cond[-whichMax]
      if(isTRUE(se)) {
        attr(values, "mapping") <- aes_string(x=xvar, y="Value", ymin="Lower", 
                                              ymax="Upper", color="Method", 
                                              fill="Method")
        attr(values, "geom") <- function(..., stat) geom_smooth(..., 
                                                                stat="identity")
      } else {
        attr(values, "mapping") <- aes_string(x=xvar, y="Value", color="Method")
        attr(values, "geom") <- geom_line
      }
    }
    # add facetting formula
    if(length(cond) > 0) {
      if(length(cond) == 2) f <- paste(cond, collapse="~")
      else f <- paste("~", paste(cond, collapse="+"))
      attr(values, "facets") <- as.formula(f)
    }
    ## return data
    attr(values, "method") <- method
    values
  })
