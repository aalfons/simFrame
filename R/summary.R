# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

## data control
setMethod("summary", "VirtualDataControl", function(object) object)

## sample control
setMethod("summary", "VirtualSampleControl", function(object) object)

## sample setup
setMethod(
  "summary", "SampleSetup", 
  function(object) {
    size <- sapply(getIndices(object), length, USE.NAMES=FALSE)
    if(length(size) == 0) size <- numeric()  # in case of empty list
    SummarySampleSetup(size=size)
  })

## contamination control
setMethod("summary", "VirtualContControl", function(object) object)

## NA control
setMethod("summary", "VirtualNAControl", function(object) object)

## simulation control
setMethod("summary", "SimControl", function(object) object)

## simulation results
# summary of data.frame is of class "table", 
# thus no extra class "summary.SimResults" necessary
setMethod(
  "summary", "SimResults", 
  function(object, ...) summary(getValues(object), ...))
