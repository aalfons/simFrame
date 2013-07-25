# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

setMethod(
  "fortify", "SimResults", 
  function(model, data = NULL, cont = NULL, NARate = NULL, select = NULL, 
           method = c("box", "density", "line"), average = c("mean", "median"), 
           ...) {
    ## initializations
    method <- match.arg(method)
    ## take the requested subset of the results
    model <- subset(model, data=data, cont=cont, NARate=NARate, select=select)
    cn <- getColnames(model)
    # extract simulation results
    if(method == "line") {
      average <- match.arg(average)
      fun <- switch(average, mean=mean, median=median)
      values <- aggregate(model, FUN=fun, na.rm=TRUE)
      info <- setdiff(names(values), cn)
    } else {
      values <- getValues(model)
      info <- setdiff(names(values), c("Run", "Rep", "Sample", cn))
      values <- values[, c(info, cn), drop=FALSE]
    }
    # extract additional information
    if(length(info) > 0) {
      if("Data" %in% info) {
        dataControl <- getControl(model, which="data")
        indices <- getIndices(dataControl)
        size <- getSize(dataControl)[indices[, 1]]
        tuning <- convertTuning(getTuning(dataControl))[indices[, 2]]
        data <- values[, 1]
        if(length(tuning) == 0) {
          info[1] <- "Size"
          values <- cbind(Size=size[data], values[, -1, drop=FALSE])
        } else {
          info <- c("Size", "DataTuning", info[-1])
          values <- cbind(Size=size[data], DataTuning=tuning[data], 
                          values[, -1, drop=FALSE])
        }
      }
      if("Cont" %in% info) {
        control <- getControl(model)
        contControl <- getControl(control, which="cont")
        epsilon <- getEpsilon(contControl)
        tuning <- getTuning(contControl)
        indices <- getIndices(contControl)
        remove <- match("Cont", info)
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
        cont <- values[, remove]
        if(nrow(tuning) == 0) {
          info[remove] <- "Epsilon"
          names(values)[remove] <- "Epsilon"
          values[, remove] <- epsilon[cont]
        } else {
          tuning <- convertTuning(tuning)[indices[,2]]
          before <- if(remove == 1) 0 else seq_len(remove-1)
          info <- c(info[before], "Epsilon", "ContTuning", 
                    info[-seq_len(remove)])
          values <- cbind(values[, before, drop=FALSE], 
                          Epsilon=epsilon[cont], ContTuning=tuning[cont], 
                          values[, -seq_len(remove), drop=FALSE])
        }
      }
      ninfo <- sapply(values[, info, drop=FALSE], function(x) length(unique(x)))
      cond <- info[ninfo > 1]
    } else cond <- character()
    # reshape simulation results
    values <- melt(values, id.vars=info, measure.vars=cn, 
                   variable.name="Method", value.name="Value")
    # add additional information as attributes
    if(method == "box") {
      attr(values, "mapping") <- aes_string(x="Method", y="Value")
      attr(values, "geom") <- geom_boxplot
    } else if(method == "density") {
      attr(values, "mapping") <- aes_string(x="Value", color="Method")
      attr(values, "geom") <- geom_density
    } else {
      if(length(cond) == 0) {
        values <- cbind(Simulation=rep.int("", nrow(values)), values)
        xvar <- "Simulation"
      } else {
        whichMax <- which.max(ninfo[cond])
        xvar <- cond[whichMax]
        cond <- cond[-whichMax]
      }
      attr(values, "mapping") <- aes_string(x=xvar, y="Value", color="Method")
      attr(values, "geom") <- geom_line
    }
    if(length(cond) > 0) {
      if(length(cond) == 2) f <- paste(cond, collapse="~")
      else f <- paste("~", paste(cond, collapse="+"))
      attr(values, "facets") <- as.formula(f)
    }
    # return data
    attr(values, "method") <- method
    values
  })
