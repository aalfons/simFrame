# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

setMethod(
  "simPlot", "SimResults", 
  function(object, data = NULL, cont = NULL, miss = NULL, select = NULL, 
           method = c("box", "density", "line"), average = c("mean", "median"), 
           se = TRUE, ...) {
    if(missing(method)) {
      data <- fortify(object, data=data, cont=cont, miss=miss, select=select, 
                      average=average, se=se)
    } else {
      data <- fortify(object, data=data, cont=cont, miss=miss, select=select, 
                      method=method, average=average, se=se)
    }
    simPlot(data, ...)
  })

setMethod(
  "simPlot", "data.frame", 
  function(object, mapping = attr(object, "mapping"), 
           facets = attr(object, "facets"), 
           labels = NULL, ...) {
    # initializations
    method <- attr(object, "method")
    # change factor levels of method column to change labels in the plot
    if(!is.null(labels)) levels(object$Method) <- labels
    # create selected plot
    if(method == "box") boxPlot(object, mapping, facets, ...)
    else if(method == "density") densityPlot(object, mapping, facets, ...)
    else linePlot(object, mapping, facets, ...)
  })


setMethod(
  "autoplot", "SimResults", 
  function(object, ...) simPlot(object, ...))


setMethod(
  "plot", signature(x = "SimResults", y = "missing"), 
  function(x, y, ...) simPlot(x, ...))


## internal functions

# box plot
boxPlot <- function(data, mapping, facets = NULL, main = NULL, 
                    xlab = NULL, ylab = NULL, ...) {
  # define default title and axis labels
  if(is.null(ylab)) ylab <- "Simulation results"
  # generate plot
  geom <- attr(data, "geom")
  p <- ggplot(data, mapping) + geom(...) + labs(title=main, x=xlab, y=ylab)
  if(!is.null(facets)) {
    # split plot into different panels
    if(length(facets) == 2) p <- p + facet_wrap(facets) 
    else p <- p + facet_grid(facets)
  }
  p
}

# density plot
densityPlot <- function(data, mapping, facets = NULL, main = NULL, 
                        xlab = NULL, ylab = NULL, ...) {
  # define default title and axis labels
  if(is.null(xlab)) xlab <- "Simulation results"
  if(is.null(ylab)) ylab <- "Density"
  # generate plot
  geom <- attr(data, "geom")
  p <- ggplot(data, mapping) + geom(...) + labs(title=main, x=xlab, y=ylab)
  if(!is.null(facets)) {
    # split plot into different panels
    if(length(facets) == 2) p <- p + facet_wrap(facets) 
    else p <- p + facet_grid(facets)
  }
  p
}

# line plot
linePlot <- function(data, mapping, facets = NULL, main = NULL, 
                     xlab = NULL, ylab = NULL, ...) {
  # define default title and axis labels
  if(is.null(ylab)) ylab <- "Simulation results"
  # generate plot
  geom <- attr(data, "geom")
  p <- ggplot(data, mapping) + geom(...) + labs(title=main, x=xlab, y=ylab)
  if(!is.null(facets)) {
    # split plot into different panels
    if(length(facets) == 2) p <- p + facet_wrap(facets) 
    else p <- p + facet_grid(facets)
  }
  p
}
