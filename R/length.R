# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

## data control
setMethod("length", "VirtualDataControl", function(x) length(getSize(x)))
setMethod("length", "DataControl", function(x) nrow(getIndices(x)))

## sample control
setMethod("length", "VirtualSampleControl", function(x) getK(x))

## sample setup
setMethod("length", "SampleSetup", function(x) length(getIndices(x)))

## contamination control
setMethod("length", "VirtualContControl", function(x) length(getEpsilon(x)))
setMethod("length", "ContControl", function(x) nrow(getIndices(x)))

## NA control
setMethod("length", "VirtualNAControl", function(x) nrow(getNARate(x)))
