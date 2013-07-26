# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

setMethod(
  "fortify", "SimResults", 
  function(model, data = NULL, cont = NULL, NARate = NULL, select = NULL, 
           method = c("box", "density", "line"), average = c("mean", "median"), 
           level = 0.95, ...) {
    ## initializations
    method <- match.arg(method)
    ## take the requested subset of the results
    model <- subset(model, data=data, cont=cont, NARate=NARate, select=select)
    cn <- getColnames(model)
    ## extract simulation results
    if(method == "line") {
      average <- match.arg(average)
      alpha <- 1 - level
      if(average == "mean") {
        fun <- function(x, probs) {
          q <- quantile(x, probs=probs, na.rm=TRUE, names=FALSE)
          names(q) <- c("Lower", "Upper")
          c(Value=mean(x, na.rm=TRUE), q)
        }
        probs <- c(alpha/2, 1-alpha/2)
      } else {
        fun <- function(x, probs) {
          q <- quantile(x, probs=probs, na.rm=TRUE, names=FALSE)
          names(q) <- c("Value", "Lower", "Upper")
          q
        }
        probs <- c(0.5, alpha/2, 1-alpha/2)
      }
      values <- aggregate(model, FUN=fun, probs=probs)
      info <- setdiff(names(values), cn)
    } else {
      values <- getValues(model)
      info <- setdiff(names(values), c("Run", "Rep", "Sample", cn))
      values <- values[, c(info, cn), drop=FALSE]
    }
    ## extract additional information
    # number of observations and tuning parameters for data generation
    if(length(info) > 0) {
      if("Data" %in% info) {
        dataControl <- getControl(model, which="data")
        indices <- getIndices(dataControl)
        size <- getSize(dataControl)[indices[, 1]]
        tuning <- convertTuning(getTuning(dataControl))[indices[, 2]]
        remove <- match("Data", info)
        data <- values[, remove]
        if(length(tuning) == 0) {
          # no tuning parameters, replace data column with number of 
          # observations
          info[remove] <- "Size"
          names(values)[remove] <- "Size"
          values[, remove] <- size[data]
        } else {
          # remove data column and add columns for number of observations and 
          # tuning parameters
          before <- if(remove == 1) 0 else seq_len(remove-1)
          remove <- seq_len(remove)
          info <- c(info[before], "Size", "DataTuning", info[-remove])
          values <- cbind(values[, before, drop=FALSE], 
                          Size=size[data], DataTuning=tuning[data], 
                          values[, -remove, drop=FALSE])
        }
      }
      # contamination level and tuning parameters for contamination
      if("Cont" %in% info) {
        control <- getControl(model)
        contControl <- getControl(control, which="cont")
        epsilon <- getEpsilon(contControl)
        tuning <- getTuning(contControl)
        indices <- getIndices(contControl)
        remove <- match("Cont", info)
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
        # replace contamination column
        epsilon <- epsilon[indices[, 1]]
        cont <- values[, remove]
        if(nrow(tuning) == 0) {
          # no tuning parameters, replace contamination column with 
          # contamination level
          info[remove] <- "Epsilon"
          names(values)[remove] <- "Epsilon"
          values[, remove] <- epsilon[cont]
        } else {
          # remove contamination column and add columns for contamination level 
          # and tuning parameters
          tuning <- convertTuning(tuning)[indices[,2]]
          before <- if(remove == 1) 0 else seq_len(remove-1)
          remove <- seq_len(remove)
          info <- c(info[before], "Epsilon", "ContTuning", info[-remove])
          values <- cbind(values[, before, drop=FALSE], 
                          Epsilon=epsilon[cont], ContTuning=tuning[cont], 
                          values[, -remove, drop=FALSE])
        }
      }
      ninfo <- sapply(values[, info, drop=FALSE], function(x) length(unique(x)))
      cond <- info[ninfo > 1]
    } else cond <- character()
    ## reshape simulation results
    values <- lapply(cn, function(j, info) {
      val <- cbind(Value=values[[j]])
      cbind(info, Method=rep.int(j, nrow(info)), val)
    }, info=values[, info, drop=FALSE])
    values <- do.call(rbind, values)
    rownames(values) <- NULL
    ## add additional information as attributes
    # add aesthetic mapping and plot geometry
    if(method == "box") {
      attr(values, "mapping") <- aes_string(x="Method", y="Value")
      attr(values, "geom") <- geom_boxplot
    } else if(method == "density") {
      attr(values, "mapping") <- aes_string(x="Value", color="Method")
      attr(values, "geom") <- geom_density
    } else {
      if(length(cond) == 0) {
        # TODO: should error be thrown instead?
        attr(values, "mapping") <- aes_string(x="Method", y="Value", 
                                              ymin="Lower", ymax="Upper")
        attr(values, "geom") <- geom_pointrange
      } else {
        whichMax <- which.max(ninfo[cond])
        xvar <- cond[whichMax]
        cond <- cond[-whichMax]
        attr(values, "mapping") <- aes_string(x=xvar, y="Value", ymin="Lower", 
                                              ymax="Upper", color="Method", 
                                              fill="Method")
        attr(values, "geom") <- function(..., stat) geom_smooth(..., stat="identity")
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
