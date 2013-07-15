# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

## data control
setMethod("head", "VirtualDataControl", function(x) x)

## sample control
setMethod("head", "VirtualSampleControl", function(x) x)

## sample setup
setMethod(
  "head", "SampleSetup", 
  function(x, k = 6, n = 6, ...) {
    if(!is.numeric(k) || length(k) == 0) k <- 6
    else k <- k[1]
    indices <- head(getIndices(x), n=k, ...)  # first list components
    indices <- lapply(indices, head, n=n, ...)  # first elements of components
    setIndices(x, indices)
    call <- match.call(call=sys.call(-1))  # jump back one environment
    setCall(x, call)
    x
  })

## contamination control
setMethod("head", "VirtualContControl", function(x) x)

## NA control
setMethod("head", "VirtualNAControl", function(x) x)

## simulation control
setMethod("head", "SimControl", function(x) x)

# class "SimResults"
setMethod(
  "head", "SimResults", 
  function(x, ...) {
    values <- head(getValues(x), ...)
    setValues(x, values)
    call <- match.call()
    setCall(x, call)
    x
  })
