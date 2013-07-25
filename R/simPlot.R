# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

setMethod(
  "simPlot", "SimResults", 
  function(object, data = NULL, cont = NULL, NARate = NULL, select = NULL, 
           method = c("box", "density", "line"), average = c("mean", "median"), 
           ...) {
    data <- fortify(object, data=data, cont=cont, NARate=NARate, select=select, 
                    method=method)
    simPlot(data, ...)
  })

setMethod(
  "simPlot", "data.frame", 
  function(object, mapping = attr(object, "mapping"), 
           facets = attr(object, "facets"), ...) {
    # create selected plot
    method <- attr(object, "method")
    if(method == "box") boxPlot(object, mapping, facets, ...)
    else if(method == "density") densityPlot(object, mapping, facets, ...)
    else linePlot(object, mapping, facets, ...)
  })


## internal function for box plot
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

## internal function for density plot
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

## internal function for line plot
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
