# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

## initializations
library("simFrame")
data("eusilcP")

## control objects for sampling and contamination
sc <- SampleControl(size = 500, k = 50)
cc <- RandomContControl(target = "eqIncome", epsilon = 0.02, 
    fun = function(x) x * 25, type = "CAR")

## define function for simulation runs
sim <- function(x) {
    c(mean = mean(x$eqIncome), trimmed = mean(x$eqIncome, trim = 0.02))
}

## run simulation
results <- runSimulation(eusilcP, sc, contControl = cc, fun = sim, seed = 12345)

## inspect results
head(results)
aggregate(results)

## compute true values
tv <- mean(eusilcP$eqIncome)
tv

## plot results
plot(results) + geom_hline(yintercept=tv)
plot(results, method = "density") + geom_vline(xintercept=tv)
