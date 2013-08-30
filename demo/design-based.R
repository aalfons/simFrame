# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

## initializations
library("simFrame")
library("laeken")
data("eusilcP")

## define samples
sc <- SampleControl(eusilcP, design = "region", grouping = "hid", 
    size = c(75, 250, 250, 125, 200, 225, 125, 150, 100), k = 100)

## define contamination
cc <- RandomContControl(target = "eqIncome", epsilon = 0.005, 
    grouping = "hid", dots = list(mean = 500000, sd = 10000))

## define function for simulation runs
sim <- function(x, k) {
    g <- gini(x$eqIncome, x$.weight)$value
    eqIncHill <- fitPareto(x$eqIncome, k = k, 
        method = "thetaHill", groups = x$hid)
    gHill <- gini(eqIncHill, x$.weight)$value
    eqIncPDC <- fitPareto(x$eqIncome, k = k, 
        method = "thetaPDC", groups = x$hid)
    gPDC <- gini(eqIncPDC, x$.weight)$value
    c(standard = g, Hill = gHill, PDC = gPDC)
}

## run simulation
results <- runSimulation(eusilcP, sc, contControl = cc, 
    design = "gender", fun = sim, k = 125, seed = 12345)

## inspect results
head(results)
aggregate(results)

## compute true values
tv <- aggregate(eusilcP[, "eqIncome"], eusilcP[, "gender", drop=FALSE], 
    function(x) gini(x)$value)
tv

## plot results
plot(results, true = tv, ylab = "Gini coefficient") + 
  geom_hline(aes(yintercept=x), data=tv)
