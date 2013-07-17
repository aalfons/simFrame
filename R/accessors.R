# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

# mutator functions, whose generic functions contain the '...' argument:
# the expression needs to be defined in the environment of the generic 
# function, but it needs to be evaluated in the environment one level 
# further up (i.e., second environment above the current one)


## data generation

setMethod(
  "getSize", "VirtualDataControl", 
  function(x) slot(x, "size"))
# setMethod(
#   "setSize", "VirtualDataControl", 
#   function(x, size) {
#     eval.parent(substitute(slot(x, "size") <- size, env=parent.frame()), n=2)
#   })

setMethod(
  "getDistribution", "DataControl", 
  function(x) slot(x, "distribution"))
setMethod(
  "setDistribution", "DataControl", 
  function(x, distribution) {
    eval.parent(substitute(slot(x, "distribution") <- distribution))
  })

setMethod(
  "getTuning", "DataControl", 
  function(x) slot(x, "tuning"))
setMethod(
  "setTuning", "DataControl", 
  function(x, tuning) {
    if(is.list(tuning)) {
      tuning$stringsAsFactors <- FALSE
      tuning <- do.call(expand.grid, tuning)
    }
    eval.parent(substitute(slot(x, "tuning") <- tuning))
  })

setMethod(
  "getIndices", "DataControl", 
  function(x) slot(x, "indices"))
setMethod(
  "setIndices", "DataControl", 
  function(x, size, tuning) {
    eval.parent(substitute(slot(x, "indices") <- convertToIndices(size, tuning), 
                           env=parent.frame()), n=2)
  })

setMethod(
  "getDots", "DataControl", 
  function(x) slot(x, "dots"))
# setMethod(
#   "setDots", "DataControl", 
#   function(x, dots) {
#     eval.parent(substitute(slot(x, "dots") <- dots, env=parent.frame()), n=2)
#   })

setMethod(
  "getColnames", "DataControl", 
  function(x) slot(x, "colnames"))
# setMethod(
#   "setColnames", "DataControl", 
#   function(x, colnames) {
#     eval.parent(substitute(slot(x, "colnames") <- colnames))
#   })


## sampling

setMethod(
  "getK", "VirtualSampleControl", function(x) slot(x, "k"))
setMethod(
  "setK", "VirtualSampleControl", 
  function(x, k) eval.parent(substitute(slot(x, "k") <- k)))

setMethod("getSeed", "VirtualSampleControl", function(x) slot(x, "seed"))
setMethod(
  "setSeed", "VirtualSampleControl", 
  function(x, seed) eval.parent(substitute(slot(x, "seed") <- seed)))


# basic sampling designs

setMethod(
  "getDesign", "SampleControl", function(x) slot(x, "design"))
# setMethod(
#   "setDesign", "SampleControl", 
#   function(x, design) eval.parent(substitute(slot(x, "design") <- design)))

setMethod(
  "getGrouping", "SampleControl", function(x) slot(x, "grouping"))
# setMethod(
#   "setGrouping", "SampleControl", 
#   function(x, grouping) {
#     eval.parent(substitute(slot(x, "grouping") <- grouping))
#   })

setMethod(
  "getCollect", "SampleControl", function(x) slot(x, "collect"))
# setMethod(
#   "setCollect", "SampleControl", 
#   function(x, collect) eval.parent(substitute(slot(x, "collect") <- collect)))

setMethod(
  "getFun", "SampleControl", function(x) slot(x, "fun"))
setMethod(
  "setFun", "SampleControl", 
  function(x, fun) {
    eval.parent(substitute(slot(x, "fun") <- fun, env=parent.frame()), n=2)
  })

setMethod(
  "getSize", "SampleControl", function(x) slot(x, "size"))
# setMethod(
#   "setSize", "SampleControl", 
#   function(x, size) {
#     eval.parent(substitute(slot(x, "size") <- size, env=parent.frame()), n=2)
#   })

setMethod(
  "getProb", "SampleControl", function(x) slot(x, "prob"))
# setMethod(
#   "setProb", "SampleControl", 
#   function(x, prob) {
#     eval.parent(substitute(slot(x, "prob") <- prob, env=parent.frame()), n=2)
#   })

setMethod(
  "getDots", "SampleControl", function(x) slot(x, "dots"))
# setMethod(
#   "setDots", "SampleControl", 
#   function(x, dots) {
#     eval.parent(substitute(slot(x, "dots") <- dots, env=parent.frame()), n=2)
#   })


# two-stage sampling designs

setMethod(
  "getDesign", "TwoStageControl", function(x) slot(x, "design"))
# setMethod(
#   "setDesign", "TwoStageControl", 
#   function(x, design) eval.parent(substitute(slot(x, "design") <- design)))

setMethod(
  "getGrouping", "TwoStageControl", function(x) slot(x, "grouping"))
# setMethod(
#   "setGrouping", "TwoStageControl", 
#   function(x, grouping) {
#     eval.parent(substitute(slot(x, "grouping") <- grouping))
#   })

# in the following mutators: 'stage' is not available in the environment of 
# the generic function and needs to be extracted from the additional arguments

setMethod(
  "getFun", "TwoStageControl", 
  function(x, stage = NULL) {
    fun <- slot(x, "fun")
    if(is.null(stage)) fun
    else {
      checkStage(stage)
      fun[[stage]]
    }
  })
setMethod(
  "setFun", "TwoStageControl", 
  function(x, fun, stage = NULL) {
    pf <- parent.frame()  # environment of generic function
    if(is.null(stage)) expr <- substitute(slot(x, "fun") <- fun, pf)
    else {
      checkStage(stage)
      expr <- substitute(slot(x, "fun")[[list(...)$stage]] <- fun, pf)
    }
    eval.parent(expr, n=2)  # evaluate expression
  })

setMethod(
  "getSize", "TwoStageControl", 
  function(x, stage = NULL) {
    size <- slot(x, "size")
    if(is.null(stage)) size
    else {
      checkStage(stage)
      size[[stage]]
    }
  })
# setMethod(
#   "setSize", "TwoStageControl", 
#   function(x, size, stage = NULL) {
#     pf <- parent.frame()  # environment of generic function
#     if(is.null(stage)) expr <- substitute(slot(x, "size") <- size, pf)
#     else {
#       checkStage(stage)
#       expr <- substitute(slot(x, "size")[[list(...)$stage]] <- size, pf)
#     }
#     eval.parent(expr, n=2)  # evaluate expression
#   })

setMethod(
  "getProb", "TwoStageControl", 
  function(x, stage = NULL) {
    prob <- slot(x, "prob")
    if(is.null(stage)) prob
    else {
      checkStage(stage)
      prob[[stage]]
    }
  })
# setMethod(
#   "setProb", "TwoStageControl", 
#   function(x, prob, stage = NULL) {
#     pf <- parent.frame()  # environment of generic function
#     if(is.null(stage)) expr <- substitute(slot(x, "prob") <- prob, pf)
#     else {
#       checkStage(stage)
#       expr <- substitute(slot(x, "prob")[[list(...)$stage]] <- prob, pf)
#     }
#     eval.parent(expr, n=2)  # evaluate expression
#   })

setMethod(
  "getDots", "TwoStageControl", 
  function(x, stage = NULL) {
    dots <- slot(x, "dots")
    if(is.null(stage)) dots
    else {
      checkStage(stage)
      dots[[stage]]
    }
  })
# setMethod(
#   "setDots", "TwoStageControl", 
#   function(x, dots, stage = NULL) {
#     pf <- parent.frame()  # environment of generic function
#     if(is.null(stage)) expr <- substitute(slot(x, "dots") <- dots, pf)
#     else {
#       checkStage(stage)
#       expr <- substitute(slot(x, "dots")[[list(...)$stage]] <- dots, pf)
#     }
#     eval.parent(expr, n=2)  # evaluate expression
#   })


# set-up samples

# public accessors (getters)
setMethod("getIndices", "SampleSetup", function(x) slot(x, "indices"))
setMethod("getProb", "SampleSetup", function(x) slot(x, "prob"))
setMethod("getControl", "SampleSetup", function(x) slot(x, "control"))
setMethod("getCall", "SampleSetup", function(x) slot(x, "call"))

# private mutators (setters)
setMethod(
  "setIndices", "SampleSetup", 
  function(x, indices) {
    eval.parent(substitute(slot(x, "indices") <- indices, env=parent.frame()), 
                n=2)
  })
setMethod(
  "setCall", "SampleSetup", 
  function(x, call) eval.parent(substitute(slot(x, "call") <- call)))

# summary
setMethod("getSize", "SummarySampleSetup", function(x) slot(x, "size"))


## contamination

setMethod(
  "getTarget", "VirtualContControl", 
  function(x) slot(x, "target"))
# setMethod(
#   "setTarget", "VirtualContControl", 
#   function(x, target) eval.parent(substitute(slot(x, "target") <- target)))

setMethod(
  "getEpsilon", "VirtualContControl", 
  function(x) slot(x, "epsilon"))
# setMethod(
#   "setEpsilon", "VirtualContControl", 
#   function(x, epsilon) eval.parent(substitute(slot(x, "epsilon") <- epsilon)))

setMethod(
  "getTuning", "ContControl", 
  function(x) slot(x, "tuning"))
setMethod(
  "setTuning", "ContControl", 
  function(x, tuning) {
    if(is.list(tuning)) {
      tuning$stringsAsFactors <- FALSE
      tuning <- do.call(expand.grid, tuning)
    }
    eval.parent(substitute(slot(x, "tuning") <- tuning))
  })

setMethod(
  "getIndices", "ContControl", 
  function(x) slot(x, "indices"))
setMethod(
  "setIndices", "ContControl", 
  function(x, epsilon, tuning) {
    eval.parent(substitute(slot(x, "indices") <- convertToIndices(epsilon, tuning), 
                           env=parent.frame()), n=2)
  })

setMethod(
  "getFun", "SimpleContControl", 
  function(x) slot(x, "fun"))
setMethod(
  "setFun", "SimpleContControl", 
  function(x, fun) {
    eval.parent(substitute(slot(x, "fun") <- fun, env=parent.frame()), n=2)
  })

setMethod(
  "getDots", "SimpleContControl", 
  function(x) slot(x, "dots"))

setMethod(
  "getGrouping", "RandomContControl", 
  function(x) slot(x, "grouping"))
# setMethod(
#   "setGrouping", "RandomContControl", 
#   function(x, grouping) {
#     eval.parent(substitute(slot(x, "grouping") <- grouping))
#   })

setMethod(
  "getAux", "RandomContControl", 
  function(x) slot(x, "aux"))
# setMethod(
#   "setAux", "RandomContControl", 
#   function(x, aux) eval.parent(substitute(slot(x, "aux") <- aux)))

setMethod(
  "getDistribution", "CCARContControl", 
  function(x) slot(x, "distribution"))
setMethod(
  "setDistribution", "CCARContControl", 
  function(x, distribution) {
    eval.parent(substitute(slot(x, "distribution") <- distribution))
  })

setMethod(
  "getDots", "CCARContControl", 
  function(x) slot(x, "dots"))
# setMethod(
#   "setDots", "CCARContControl", 
#   function(x, dots) {
#     eval.parent(substitute(slot(x, "dots") <- dots, env=parent.frame()), n=2)
#   })

setMethod(
  "getFun", "CARContControl", 
  function(x) slot(x, "fun"))
setMethod(
  "setFun", "CARContControl", 
  function(x, fun) {
    eval.parent(substitute(slot(x, "fun") <- fun, env=parent.frame()), n=2)
  })

setMethod(
  "getDots", "CARContControl", 
  function(x) slot(x, "dots"))
# setMethod(
#   "setDots", "CARContControl", 
#   function(x, dots) {
#     eval.parent(substitute(slot(x, "dots") <- dots, env=parent.frame()), n=2)
#   })


## missing values

setMethod(
  "getTarget", "VirtualNAControl", 
  function(x) slot(x, "target"))
# setMethod(
#   "setTarget", "VirtualNAControl", 
#   function(x, target) eval.parent(substitute(slot(x, "target") <- target)))

setMethod(
  "getNARate", "VirtualNAControl", 
  function(x) slot(x, "NARate"))
# setMethod(
#   "setNARate", "VirtualNAControl", 
#   function(x, NARate) eval.parent(substitute(slot(x, "NARate") <- NARate)))

setMethod(
  "getGrouping", "NAControl", 
  function(x) slot(x, "grouping"))
# setMethod(
#   "setGrouping", "NAControl", 
#   function(x, grouping) {
#     eval.parent(substitute(slot(x, "grouping") <- grouping))
#   })

setMethod(
  "getAux", "NAControl", 
  function(x) slot(x, "aux"))
# setMethod(
#   "setAux", "NAControl", 
#   function(x, aux) eval.parent(substitute(slot(x, "aux") <- aux)))

setMethod(
  "getIntoContamination", "NAControl", 
  function(x) slot(x, "intoContamination"))
# setMethod(
#   "setIntoContamination", "NAControl", 
#   function(x, intoContamination) {
#     eval.parent(substitute(slot(x, "intoContamination") <- intoContamination))
#   })


## class "SimControl"

setMethod(
  "getControl", "SimControl", 
  function(x, which = c("cont", "NA")) {
    which <- match.arg(which)
    slot(x, paste(which, "Control", sep=""))
  })

setMethod("getDesign", "SimControl", function(x) slot(x, "design"))
# setMethod("setDesign", "SimControl", 
#           function(x, design) eval.parent(substitute(slot(x, "design") <- design)))

setMethod("getFun", "SimControl", function(x) slot(x, "fun"))
# setMethod("setFun", "SimControl", 
#           function(x, fun) {
#             eval.parent(substitute(slot(x, "fun") <- fun, env=parent.frame()), n=2)
#           })

setMethod("getDots", "SimControl", function(x) slot(x, "dots"))
# setMethod("setDots", "SimControl", 
#           function(x, dots) {
#             eval.parent(substitute(slot(x, "dots") <- dots, env=parent.frame()), n=2)
#           })

setMethod("getSeed", "SimControl", function(x) slot(x, "seed"))
setMethod(
  "setSeed", "SimControl", 
  function(x, seed) eval.parent(substitute(slot(x, "seed") <- seed)))


## class "SimResults"

# public accessors (getters)
setMethod("getValues", "SimResults", function(x) slot(x, "values"))
setMethod("getDesign", "SimResults", function(x) slot(x, "design"))
setMethod("getColnames", "SimResults", function(x) slot(x, "colnames"))
setMethod("getEpsilon", "SimResults", function(x) slot(x, "epsilon"))
setMethod(
  "getTuning", "SimResults", 
  function(x, which = c("data", "cont")) {
    which <- match.arg(which)
    slot(x, paste(which, "Tuning", sep=""))
  })
setMethod("getNARate", "SimResults", function(x) slot(x, "NARate"))
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
setMethod("setValues", "SimResults", 
          function(x, values) eval.parent(substitute(slot(x, "values") <- values)))
setMethod("setCall", "SimResults", 
          function(x, call) eval.parent(substitute(slot(x, "call") <- call)))
