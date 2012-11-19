# ----------------------
# Author: Andreas Alfons
#         KU Leuven
# ----------------------

# mutator functions, whose generic functions contain the '...' argument:
# the expression needs to be defined in the environment of the generic 
# function, but it needs to be evaluated in the environment one level 
# further up (i.e., second environment above the current one)


## class "DataControl"

setMethod("getSize", "DataControl", function(x) slot(x, "size"))
# setMethod("setSize", "DataControl", 
#           function(x, size) eval.parent(substitute(slot(x, "size") <- size)))
setMethod("setSize", "DataControl", 
          function(x, size) {
            eval.parent(substitute(slot(x, "size") <- size, env=parent.frame()), n=2)
          })

setMethod("getDistribution", "DataControl", function(x) slot(x, "distribution"))
setMethod("setDistribution", "DataControl", 
          function(x, distribution) {
            eval.parent(substitute(slot(x, "distribution") <- distribution))
          })

setMethod("getTuning", "DataControl", function(x) slot(x, "tuning"))
setMethod("setTuning", "DataControl", 
          function(x, tuning) {
            if(is.list(tuning)) {
              tuning$stringsAsFactors <- FALSE
              tuning <- do.call(expand.grid, tuning)
            }
            eval.parent(substitute(slot(x, "tuning") <- tuning))
          })

setMethod("getDots", "DataControl", function(x) slot(x, "dots"))
# setMethod("setDots", "DataControl", 
#           function(x, dots) eval.parent(substitute(slot(x, "dots") <- dots)))
setMethod("setDots", "DataControl", 
          function(x, dots) {
            eval.parent(substitute(slot(x, "dots") <- dots, env=parent.frame()), n=2)
          })

setMethod("getColnames", "DataControl", function(x) slot(x, "colnames"))
setMethod("setColnames", "DataControl", 
          function(x, colnames) {
            eval.parent(substitute(slot(x, "colnames") <- colnames))
          })


## class "ContControl"

setMethod("getTarget", "VirtualContControl", function(x) slot(x, "target"))
setMethod("setTarget", "VirtualContControl", 
          function(x, target) eval.parent(substitute(slot(x, "target") <- target)))

setMethod("getEpsilon", "VirtualContControl", function(x) slot(x, "epsilon"))
setMethod("setEpsilon", "VirtualContControl", 
          function(x, epsilon) eval.parent(substitute(slot(x, "epsilon") <- epsilon)))

setMethod("getTuning", "ContControl", function(x) slot(x, "tuning"))
setMethod("setTuning", "ContControl", 
          function(x, tuning) {
            if(is.list(tuning)) {
              tuning$stringsAsFactors <- FALSE
              tuning <- do.call(expand.grid, tuning)
            }
            eval.parent(substitute(slot(x, "tuning") <- tuning))
          })

setMethod("getGrouping", "ContControl", function(x) slot(x, "grouping"))
setMethod("setGrouping", "ContControl", 
          function(x, grouping) {
            eval.parent(substitute(slot(x, "grouping") <- grouping))
          })

setMethod("getAux", "ContControl", function(x) slot(x, "aux"))
setMethod("setAux", "ContControl", 
          function(x, aux) eval.parent(substitute(slot(x, "aux") <- aux)))

setMethod("getDistribution", "DCARContControl", 
          function(x) slot(x, "distribution"))
setMethod("setDistribution", "DCARContControl", 
          function(x, distribution) {
            eval.parent(substitute(slot(x, "distribution") <- distribution))
          })

setMethod("getDots", "DCARContControl", function(x) slot(x, "dots"))
# setMethod("setDots", "DCARContControl", 
#           function(x, dots) eval.parent(substitute(slot(x, "dots") <- dots)))
setMethod("setDots", "DCARContControl", 
          function(x, dots) {
            eval.parent(substitute(slot(x, "dots") <- dots, env=parent.frame()), n=2)
          })

setMethod("getFun", "DARContControl", function(x) slot(x, "fun"))
# setMethod("setFun", "DARContControl", 
#           function(x, fun) eval.parent(substitute(slot(x, "fun") <- fun)))
setMethod("setFun", "DARContControl", 
          function(x, fun) {
            eval.parent(substitute(slot(x, "fun") <- fun, env=parent.frame()), n=2)
          })

setMethod("getDots", "DARContControl", function(x) slot(x, "dots"))
# setMethod("setDots", "DARContControl", 
#           function(x, dots) eval.parent(substitute(slot(x, "dots") <- dots)))
setMethod("setDots", "DARContControl", 
          function(x, dots) {
            eval.parent(substitute(slot(x, "dots") <- dots, env=parent.frame()), n=2)
          })

setGeneric("getIntoContamination", 
           function(x) standardGeneric("getIntoContamination"))
setGeneric("setIntoContamination", 
           function(x, intoContamination) standardGeneric("setIntoContamination"))

setGeneric("getNArate", function(x) standardGeneric("getNArate"))
setGeneric("setNArate", function(x, NArate) standardGeneric("setNArate"))


## class "NAControl"

setMethod("getTarget", "VirtualNAControl", function(x) slot(x, "target"))
setMethod("setTarget", "VirtualNAControl", 
          function(x, target) eval.parent(substitute(slot(x, "target") <- target)))

setMethod("getNArate", "VirtualNAControl", function(x) slot(x, "NArate"))
setMethod("setNArate", "VirtualNAControl", 
          function(x, NArate) eval.parent(substitute(slot(x, "NArate") <- NArate)))

setMethod("getGrouping", "NAControl", function(x) slot(x, "grouping"))
setMethod("setGrouping", "NAControl", 
          function(x, grouping) {
            eval.parent(substitute(slot(x, "grouping") <- grouping))
          })

setMethod("getAux", "NAControl", function(x) slot(x, "aux"))
setMethod("setAux", "NAControl", 
          function(x, aux) eval.parent(substitute(slot(x, "aux") <- aux)))

setMethod("getIntoContamination", "NAControl", 
          function(x) slot(x, "intoContamination"))
setMethod("setIntoContamination", "NAControl", 
          function(x, intoContamination) {
            eval.parent(substitute(slot(x, "intoContamination") <- intoContamination))
          })
