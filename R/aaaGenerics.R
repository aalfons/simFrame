# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

#' @import methods

NULL


#' @export
setGeneric(
  "contaminate",
  function(x, control, ...) standardGeneric("contaminate"),
  valueClass = "data.frame")

#' @export
setGeneric(
  "draw",
  function(x, setup, ...) standardGeneric("draw"), 
  valueClass = "data.frame")

#' @export
setGeneric(
  "generate",
  function(control, ...) standardGeneric("generate"), 
  valueClass = "data.frame")

#' @export
setGeneric(
  "runSimulation",
  function(x, setup, nrep, control, contControl = NULL, NAControl = NULL, 
           design = character(), fun, ..., seed, ncores = 1, cl = NULL) {
    # initializations
    call <- match.call()
    # call method and store call
    res <- standardGeneric("runSimulation")
    setCall(res, call)
    res
  },
  valueClass = "SimResults"
)

#' @export
setGeneric(
  "setNA",
  function(x, control, ...) standardGeneric("setNA"),
  valueClass = "data.frame")

#' @export
setGeneric(
  "setup",
  function(x, control, ...) {
    # initializations
    call <- match.call()
    if(!missing(control) && is(control, "VirtualSampleControl")) {
      seed <- getSeed(control)
      if(length(seed) > 0) set.seed(seed)
    }
    # call method and store call
    res <- standardGeneric("setup")
    setCall(res, call)
    res
  },
  valueClass = "SampleSetup")

#' @import ggplot2
#' @export
setGeneric("simPlot", function(object, ...) standardGeneric("simPlot"))


## public accessor functions (to be exported)

#' @export
setGeneric("getAux", function(x) standardGeneric("getAux"))

#' @export
setGeneric("getCollect", function(x) standardGeneric("getCollect"))

#' @export
setGeneric("getColnames", function(x) standardGeneric("getColnames"))

#' @export
setGeneric("getControl", function(x, ...) standardGeneric("getControl"))

#' @export
setGeneric("getDesign", function(x) standardGeneric("getDesign"))

#' @export
setGeneric("getDistribution", function(x) standardGeneric("getDistribution"))

#' @export
setGeneric("getDots", function(x, ...) standardGeneric("getDots"))

#' @export
setGeneric("getEpsilon", function(x) standardGeneric("getEpsilon"))

#' @export
setGeneric("getFun", function(x, ...) standardGeneric("getFun"))

#' @export
setGeneric("getGrouping", function(x) standardGeneric("getGrouping"))

#' @export
setGeneric("getIndices", function(x) standardGeneric("getIndices"))

#' @export
setGeneric("getInfo", function(x) standardGeneric("getInfo"))

#' @export
setGeneric("getIntoContamination", 
           function(x) standardGeneric("getIntoContamination"))

#' @export
setGeneric("getK", function(x) standardGeneric("getK"))

#' @export
setGeneric("getNARate", function(x) standardGeneric("getNARate"))

#' @export
setGeneric("getNrep", function(x) standardGeneric("getNrep"))

#' @export
setGeneric("getProb", function(x, ...) standardGeneric("getProb"))

#' @export
setGeneric("getSeed", function(x) standardGeneric("getSeed"))

#' @export
setGeneric("getSize", function(x, ...) standardGeneric("getSize"))

#' @export
setGeneric("getTarget", function(x) standardGeneric("getTarget"))

#' @export
setGeneric("getTuning", function(x) standardGeneric("getTuning"))

#' @export
setGeneric("getValues", function(x) standardGeneric("getValues"))


## private mutator functions

setGeneric("setCall", function(x, call) standardGeneric("setCall"))

setGeneric("setDistribution", 
           function(x, distribution) standardGeneric("setDistribution"))

setGeneric("setFun", function(x, fun, ...) standardGeneric("setFun"))

setGeneric("setIndices", function(x, ...) standardGeneric("setIndices"))

setGeneric("setK", function(x, k) standardGeneric("setK"))

setGeneric("setSeed", function(x, seed) standardGeneric("setSeed"))

setGeneric("setTuning", function(x, tuning) standardGeneric("setTuning"))

setGeneric("setValues", function(x, values) standardGeneric("setValues"))


## existing S3 or S4 generics (just to be safe)

#' @export
setGeneric("aggregate")

#' @import ggplot2
#' @export
setGeneric("autoplot")

#' @import ggplot2
#' @export
setGeneric("fortify", 
           function(model, data = NULL, ...) standardGeneric("fortify"))

#' @export
setGeneric("getCall")

#' @export
setGeneric("head")

#' @export
setGeneric("length")

#' @export
setGeneric("plot")

#' @export
setGeneric("show")

#' @export
setGeneric("subset")

#' @export
setGeneric("summary")

#' @export
setGeneric("tail")
