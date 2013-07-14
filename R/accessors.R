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
  "getSize", "DataControl", 
  function(x) slot(x, "size"))
# setMethod(
#   "setSize", "DataControl", 
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

setMethod("getContControl", "SimControl", function(x) slot(x, "contControl"))
# setMethod("setContControl", "SimControl", 
#           function(x, contControl) {
#             eval.parent(substitute(slot(x, "contControl") <- contControl))
#           })

setMethod("getNAControl", "SimControl", function(x) slot(x, "NAControl"))
# setMethod("setNAControl", "SimControl", 
#           function(x, NAControl) {
#             eval.parent(substitute(slot(x, "NAControl") <- NAControl))
#           })

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


## class "SimResults"

# public accessors (getters)
setMethod("getValues", "SimResults", function(x) slot(x, "values"))
setMethod("getAdd", "SimResults", function(x) slot(x, "add"))
setMethod("getDesign", "SimResults", function(x) slot(x, "design"))
setMethod("getColnames", "SimResults", function(x) slot(x, "colnames"))
setMethod("getEpsilon", "SimResults", function(x) slot(x, "epsilon"))
setMethod("getNARate", "SimResults", function(x) slot(x, "NARate"))
setMethod("getDataControl", "SimResults", function(x) slot(x, "dataControl"))
setMethod("getSampleControl", "SimResults", function(x) slot(x, "sampleControl"))
setMethod("getNrep", "SimResults", function(x) slot(x, "nrep"))
setMethod("getControl", "SimResults", function(x) slot(x, "control"))
setMethod("getSeed", "SimResults", function(x) slot(x, "seed"))
setMethod("getCall", "SimResults", function(x) slot(x, "call"))

# private mutators (setters)
setMethod("setValues", "SimResults", 
          function(x, values) eval.parent(substitute(slot(x, "values") <- values)))
setMethod("setSeed", "SimResults", 
          function(x, seed) eval.parent(substitute(slot(x, "seed") <- seed)))
setMethod("setCall", "SimResults", 
          function(x, call) eval.parent(substitute(slot(x, "call") <- call)))
