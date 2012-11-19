# ----------------------
# Author: Andreas Alfons
#         KU Leuven
# ----------------------

## utilities for class "BasicVector"

# get character eqivalent
getCharacter <- function(x, names) {
  if(is(x, "character")) x
  else if((is(x, "logical") || is(x, "numeric")) && length(x)) names[x]
  else character()  # other classes
}

# get length of specified selection
getSelectionLength <- function(x) {
  if(is(x, "character")) length(x)
  else if(is(x, "numeric") && all(x >= 0)) length(x[x > 0])
  else NA  # other classes
}

# ---------------------------------------

## utilities for class "NumericMatrix"

# check if data is numeric
checkNumericMatrix <- function(x) {
  if(is(x, "numeric")) TRUE 
  else if(is(x, "matrix")) mode(x) == "numeric"
  else FALSE  # other classes
}

# ---------------------------------------

## other utilities

## call a function by either
# 1) simply evaluating a supplied function for the first argument if there are
#    no additional arguments in list format
# 2) evaluating a supplied function with 'do.call' if there are additional 
#    arguments in list format
doCall <- function(fun, first, dots = list()) {
  if(length(dots) == 0) fun(first)
  else do.call(fun, c(first, dots))
}

## call a function by either
# 1) using the above 'doCall' if there are no tuning parameters in list form
# 2) evaluating a supplied function with 'do.call' if there are tuning 
#    parameters in list format (if supplied, add additional arguments to 
#    the list)
doCallTuning <- function(fun, first, tuning, dots = list()) {
  if(length(tuning) == 0) doCall(fun, first, dots)
  else if(length(dots) == 0) do.call(fun, c(first, tuning))
  else do.call(fun, c(first, tuning, dots))
}

# get names of real columns of a data.frame 
# (i.e., remove those used internally by simFrame)
getNames <- function(x) {
  #    nam <- names(x)
  #    nam[substring(nam, 1, 1) != "."]
  setdiff(names(x), c(".weight"))
}
