# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

setGeneric(
  "contaminate",
  function(x, control, ...) standardGeneric("contaminate"),
  valueClass = "data.frame")

setGeneric(
  "draw",
  function(x, setup, ...) standardGeneric("draw"), 
  valueClass = "data.frame")

setGeneric(
  "generate",
  function(control, ...) standardGeneric("generate"), 
  valueClass = "data.frame")

setGeneric(
  "runSimulation",
  function(x, setup, nrep, control, contControl = NULL, NAControl = NULL, 
           design = character(), fun, ...) {
    # make sure that .Random.seed exists
    if(!exists(".Random.seed", envir=.GlobalEnv, inherits = FALSE)) runif(1)
    # call method and store seed before and after
    firstSeed <- .Random.seed
    res <- standardGeneric("runSimulation")
    lastSeed <- .Random.seed
    setSeed(res, list(firstSeed, lastSeed))
    call <- match.call()
    setCall(res, call)
    res
  },
  valueClass = "SimResults"
)

setGeneric(
  "setNA",
  function(x, control, ...) standardGeneric("setNA"),
  valueClass = "data.frame")

setGeneric(
  "setup",
  function(x, control, ...) {
    # make sure that .Random.seed exists
    if(!exists(".Random.seed", envir=.GlobalEnv, inherits = FALSE)) runif(1)
    # call method and store seed before and after
    firstSeed <- .Random.seed
    res <- standardGeneric("setup")
    lastSeed <- .Random.seed
    setSeed(res, list(firstSeed, lastSeed))
    call <- match.call()
    setCall(res, call)
    res
  },
  valueClass = "SampleSetup")


## public accessor and mutator functions (to be exported)

setGeneric("getAux", function(x) standardGeneric("getAux"))
# setGeneric("setAux", function(x, aux) standardGeneric("setAux"))

setGeneric("getCall", function(x, ...) standardGeneric("getCall"))

setGeneric("getCollect", function(x) standardGeneric("getCollect"))
# setGeneric("setCollect", function(x, collect) standardGeneric("setCollect"))

setGeneric("getColnames", function(x) standardGeneric("getColnames"))
# setGeneric("setColnames", function(x, colnames) standardGeneric("setColnames"))

setGeneric("getControl", function(x, ...) standardGeneric("getControl"))

setGeneric("getDesign", function(x) standardGeneric("getDesign"))
# setGeneric("setDesign", function(x, design) standardGeneric("setDesign"))

setGeneric("getDistribution", function(x) standardGeneric("getDistribution"))

setGeneric("getDots", function(x, ...) standardGeneric("getDots"))
# setGeneric("setDots", function(x, dots, ...) standardGeneric("setDots"))

setGeneric("getEpsilon", function(x) standardGeneric("getEpsilon"))
# setGeneric("setEpsilon", function(x, epsilon) standardGeneric("setEpsilon"))

setGeneric("getFun", function(x, ...) standardGeneric("getFun"))
# setGeneric("setFun", function(x, fun, ...) standardGeneric("setFun"))

setGeneric("getGrouping", function(x) standardGeneric("getGrouping"))
# setGeneric("setGrouping", function(x, grouping) standardGeneric("setGrouping"))

setGeneric("getIndices", function(x) standardGeneric("getIndices"))

setGeneric("getIntoContamination", 
           function(x) standardGeneric("getIntoContamination"))
# setGeneric("setIntoContamination", 
#            function(x, intoContamination) {
#              standardGeneric("setIntoContamination")
#            })

setGeneric("getK", function(x) standardGeneric("getK"))
setGeneric("setK", function(x, k) standardGeneric("setK"))

setGeneric("getNARate", function(x) standardGeneric("getNARate"))
# setGeneric("setNARate", function(x, NARate) standardGeneric("setNARate"))

setGeneric("getNrep", function(x) standardGeneric("getNrep"))

setGeneric("getProb", function(x, ...) standardGeneric("getProb"))
# setGeneric("setProb", function(x, prob, ...) standardGeneric("setProb"))

setGeneric("getSeed", function(x) standardGeneric("getSeed"))

setGeneric("getSize", function(x, ...) standardGeneric("getSize"))
# setGeneric("setSize", function(x, size, ...) standardGeneric("setSize"))

setGeneric("getTarget", function(x) standardGeneric("getTarget"))
# setGeneric("setTarget", function(x, target) standardGeneric("setTarget"))

setGeneric("getTuning", function(x, ...) standardGeneric("getTuning"))

setGeneric("getValues", function(x) standardGeneric("getValues"))


## private accessor and mutator functions

setGeneric("setCall", function(x, call) standardGeneric("setCall"))

setGeneric("setDistribution", 
           function(x, distribution) standardGeneric("setDistribution"))

setGeneric("setFun", function(x, fun, ...) standardGeneric("setFun"))

setGeneric("setIndices", function(x, ...) standardGeneric("setIndices"))

setGeneric("setSeed", function(x, seed) standardGeneric("setSeed"))

setGeneric("setTuning", function(x, tuning) standardGeneric("setTuning"))

setGeneric("setValues", function(x, values) standardGeneric("setValues"))


## existing S3 or S4 generics (just to be safe)

setGeneric("aggregate")
setGeneric("head")
setGeneric("length")
setGeneric("plot")
setGeneric("show")
setGeneric("summary")
setGeneric("tail")
