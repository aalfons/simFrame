# ----------------------
# Author: Andreas Alfons
#         KU Leuven
# ----------------------

setGeneric("generate",
           function(control, ...) standardGeneric("generate"), 
           valueClass = "data.frame")

setGeneric("contaminate",
           function(x, control, ...) standardGeneric("contaminate"),
           valueClass = "data.frame")


## public accessor and mutator functions (to be exported)

setGeneric("getAux", function(x) standardGeneric("getAux"))
setGeneric("setAux", function(x, aux) standardGeneric("setAux"))

setGeneric("getColnames", function(x) standardGeneric("getColnames"))
setGeneric("setColnames", 
           function(x, colnames) standardGeneric("setColnames"))

setGeneric("getDistribution", function(x) standardGeneric("getDistribution"))
setGeneric("setDistribution", 
           function(x, distribution) standardGeneric("setDistribution"))

setGeneric("getDots", function(x) standardGeneric("getDots"))
#setGeneric("setDots", function(x, dots) standardGeneric("setDots"))
setGeneric("setDots", function(x, dots, ...) standardGeneric("setDots"))

setGeneric("getEpsilon", function(x) standardGeneric("getEpsilon"))
setGeneric("setEpsilon", function(x, epsilon) standardGeneric("setEpsilon"))

setGeneric("getGrouping", function(x) standardGeneric("getGrouping"))
setGeneric("setGrouping", function(x, grouping) standardGeneric("setGrouping"))

setGeneric("getFun", function(x) standardGeneric("getFun"))
#setGeneric("setFun", function(x, fun) standardGeneric("setFun"))
setGeneric("setFun", function(x, fun, ...) standardGeneric("setFun"))

setGeneric("getSize", function(x) standardGeneric("getSize"))
#setGeneric("setSize", function(x, size) standardGeneric("setSize"))
setGeneric("setSize", function(x, size, ...) standardGeneric("setSize"))

setGeneric("getTarget", function(x) standardGeneric("getTarget"))
setGeneric("setTarget", function(x, target) standardGeneric("setTarget"))

setGeneric("getTuning", function(x) standardGeneric("getTuning"))
setGeneric("setTuning", function(x, tuning) standardGeneric("setTuning"))


## existing S3 or S4 generics (just to be safe)
setGeneric("aggregate")
setGeneric("head")
setGeneric("length")
setGeneric("plot")
setGeneric("show")
setGeneric("summary")
setGeneric("tail")
