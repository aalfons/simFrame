# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

setMethod(
  "simPlot", "SimResults", 
  function(object, data = NULL, cont = NULL, miss = NULL, select = NULL, 
           method = c("box", "density", "line"), average = c("mean", "median"), 
           ...) {
    if(missing(method)) {
      data <- fortify(object, data=data, cont=cont, miss=miss, select=select, 
                      average=average)
    } else {
      data <- fortify(object, data=data, cont=cont, miss=miss, select=select, 
                      method=method, average=average)
    }
    simPlot(data, ...)
  })

setMethod(
  "simPlot", "data.frame", 
  function(object, mapping = attr(object, "mapping"), 
           facets = attr(object, "facets"), 
           labels = NULL, ...) {
    # change factor levels of method column to change labels in the plot
    if(!is.null(labels)) levels(object$Method) <- labels
    # create selected plot
    method <- attr(object, "method")
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
  p <- ggplot(data, mapping) + geom_boxplot(...) + 
    labs(title=main, x=xlab, y=ylab)
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
  p <- ggplot(data, mapping) + geom_density(...) + 
    labs(title=main, x=xlab, y=ylab)
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
  # define the function to draw the visual representation depending on whether 
  # a confidence band is specified in the aesthetic mapping
  if(is.null(mapping$ymin) || is.null(mapping$ymax)) geom <- geom_line
  else if(is.null(mapping$colour)) {
    geom <- function(..., col = "black", stat) {
      geom_smooth(..., col=col, stat="identity")
    }
  } else geom <- function(..., stat) geom_smooth(..., stat="identity")
  # drop x-variable from facetting formula
  facets <- removeFacets(facets, mapping$x)
  # generate plot
  p <- ggplot(data, mapping) + geom(...) + labs(title=main, x=xlab, y=ylab)
  if(!is.null(facets)) {
    # split plot into different panels
    if(length(facets) == 2) p <- p + facet_wrap(facets) 
    else p <- p + facet_grid(facets)
  }
  p
}
