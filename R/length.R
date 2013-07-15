# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

## sample control
setMethod("length", "VirtualSampleControl", function(x) getK(x))

## sample setup
setMethod("length", "SampleSetup", function(x) length(getIndices(x)))

## contamination control
setMethod("length", "VirtualContControl", function(x) length(getEpsilon(x)))
setMethod("length", "ContControl", function(x) nrow(getIndices(x)))

## NA control
setMethod("length", "VirtualNAControl", function(x) getLength(getNARate(x)))


## internal S3 function
getLength <- function(x) {
  if(is(x, "numeric")) length(x) 
  else if(is(x, "matrix")) nrow(x)
  else NA  # other classes
}
