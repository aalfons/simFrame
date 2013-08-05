# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

# mutator functions, whose generic functions contain the '...' argument:
# the expression needs to be defined in the environment of the generic 
# function, but it needs to be evaluated in the environment one level 
# further up (i.e., second environment above the current one)


## data generation

setMethod("getSize", "VirtualDataControl", function(x) slot(x, "size"))
setMethod("getFun", "DataControl", function(x) slot(x, "fun"))
setMethod("getTuning", "DataControl", function(x) slot(x, "tuning"))
setMethod("getIndices", "DataControl", function(x) slot(x, "indices"))
setMethod("getDots", "DataControl", function(x) slot(x, "dots"))
setMethod("getColnames", "DataControl", function(x) slot(x, "colnames"))


## sampling

# public accessors (getters)
setMethod("getK", "VirtualSampleControl", function(x) slot(x, "k"))
setMethod("getSeed", "VirtualSampleControl", function(x) slot(x, "seed"))
setMethod("getDesign", "SampleControl", function(x) slot(x, "design"))
setMethod("getGrouping", "SampleControl", function(x) slot(x, "grouping"))

# private mutators (setters)
setMethod(
  "setK", "VirtualSampleControl", 
  function(x, k) eval.parent(substitute(slot(x, "k") <- k)))
setMethod(
  "setSeed", "VirtualSampleControl", 
  function(x, seed) eval.parent(substitute(slot(x, "seed") <- seed)))


# basic sampling designs

setMethod("getCollect", "BasicSampleControl", function(x) slot(x, "collect"))
setMethod("getFun", "BasicSampleControl", function(x) slot(x, "fun"))
setMethod("getSize", "BasicSampleControl", function(x) slot(x, "size"))
setMethod("getProb", "BasicSampleControl", function(x) slot(x, "prob"))
setMethod("getDots", "BasicSampleControl", function(x) slot(x, "dots"))


# two-stage sampling designs

setMethod(
  "getFun", "TwoStageSampleControl", 
  function(x, stage = NULL) {
    fun <- slot(x, "fun")
    if(is.null(stage)) fun
    else {
      checkStage(stage)
      fun[[stage]]
    }
  })
setMethod(
  "getSize", "TwoStageSampleControl", 
  function(x, stage = NULL) {
    size <- slot(x, "size")
    if(is.null(stage)) size
    else {
      checkStage(stage)
      size[[stage]]
    }
  })
setMethod(
  "getProb", "TwoStageSampleControl", 
  function(x, stage = NULL) {
    prob <- slot(x, "prob")
    if(is.null(stage)) prob
    else {
      checkStage(stage)
      prob[[stage]]
    }
  })
setMethod(
  "getDots", "TwoStageSampleControl", 
  function(x, stage = NULL) {
    dots <- slot(x, "dots")
    if(is.null(stage)) dots
    else {
      checkStage(stage)
      dots[[stage]]
    }
  })


## set-up samples

# public accessors (getters)
setMethod("getIndices", "SampleSetup", function(x) slot(x, "indices"))
setMethod("getProb", "SampleSetup", function(x) slot(x, "prob"))
setMethod("getControl", "SampleSetup", function(x) slot(x, "control"))
setMethod("getCall", "SampleSetup", function(x) slot(x, "call"))

# private mutators (setters)
setMethod(
  "setIndices", "SampleSetup", 
  function(x, indices) eval.parent(substitute(slot(x, "indices") <- indices)))
setMethod(
  "setCall", "SampleSetup", 
  function(x, call) eval.parent(substitute(slot(x, "call") <- call)))

# summary
setMethod("getSize", "SummarySampleSetup", function(x) slot(x, "size"))


## contamination

setMethod("getTarget", "VirtualContControl", function(x) slot(x, "target"))
setMethod("getEpsilon", "VirtualContControl", function(x) slot(x, "epsilon"))
setMethod("getTuning", "ContControl", function(x) slot(x, "tuning"))
setMethod("getIndices", "ContControl", function(x) slot(x, "indices"))
setMethod("getFun", "ContControl", function(x) slot(x, "fun"))
setMethod("getDots", "ContControl", function(x) slot(x, "dots"))
setMethod("getType", "ContControl", function(x) slot(x, "type"))
setMethod("getGrouping", "RandomContControl", function(x) slot(x, "grouping"))
setMethod("getAux", "RandomContControl", function(x) slot(x, "aux"))


## missing values

setMethod("getTarget", "VirtualNAControl", function(x) slot(x, "target"))
setMethod("getNARate", "VirtualNAControl", function(x) slot(x, "NARate"))
setMethod("getGrouping", "NAControl", function(x) slot(x, "grouping"))
setMethod("getAux", "NAControl", function(x) slot(x, "aux"))
setMethod(
  "getIntoContamination", "NAControl", 
  function(x) slot(x, "intoContamination"))


## class "SimControl"

setMethod(
  "getControl", "SimControl", 
  function(x, which = c("cont", "NA")) {
    which <- match.arg(which)
    slot(x, paste(which, "Control", sep=""))
  })
setMethod("getDesign", "SimControl", function(x) slot(x, "design"))
setMethod("getFun", "SimControl", function(x) slot(x, "fun"))
setMethod("getDots", "SimControl", function(x) slot(x, "dots"))
setMethod("getSeed", "SimControl", function(x) slot(x, "seed"))


## class "SimResults"

# public accessors (getters)
setMethod("getValues", "SimResults", function(x) slot(x, "values"))
setMethod("getColnames", "SimResults", function(x) slot(x, "colnames"))
setMethod("getInfo", "SimResults", function(x) slot(x, "info"))
setMethod("getNrep", "SimResults", function(x) slot(x, "nrep"))
setMethod(
  "getControl", "SimResults", 
  function(x, which = c("sim", "data", "sample")) {
    which <- match.arg(which)
    which <- if(which == "sim") "control" else paste(which, "Control", sep="")
    slot(x, which)
  })
setMethod("getCall", "SimResults", function(x) slot(x, "call"))

# private mutators (setters)
setMethod(
  "setValues", "SimResults", 
  function(x, values) eval.parent(substitute(slot(x, "values") <- values)))
# setMethod(
#   "setColnames", "SimResults", 
#   function(x, colnames) {
#     eval.parent(substitute(slot(x, "values") <- renameValues(x, colnames)))
#     eval.parent(substitute(slot(x, "colnames") <- colnames))
#   })
setMethod(
  "setCall", "SimResults", 
  function(x, call) eval.parent(substitute(slot(x, "call") <- call)))
