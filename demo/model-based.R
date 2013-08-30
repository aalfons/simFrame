# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

## initializations
library("simFrame")
library("robCompositions")
library("mvtnorm")

## define function and control class for generating data
crnorm <- function(n, mean, sigma) isomLRinv(rmvnorm(n, mean, sigma))
sigma <- matrix(c(1, -0.5, 1.4, -0.5, 1, -0.6, 1.4, -0.6, 2), 3, 3)
dc <- DataControl(size = 150, fun = crnorm, 
    dots = list(mean = c(0, 2, 3), sigma = sigma))

## define control class for the insertion of missing values 
nc <- NAControl(NARate = 0.05)

## define function for simulation runs
sim <- function(x, orig) {
    i <- apply(x, 1, function(x) any(is.na(x)))
    ni <- length(which(i))
    xKNNa <- impKNNa(x)$xImp
    xLS <- impCoda(x, method = "lm")$xImp
    c(knn = aDist(xKNNa, orig)/ni, LS = aDist(xLS, orig)/ni)
}

## run simulation
results <- runSimulation(dc, nrep = 50, NAControl = nc, fun = sim, seed = 12345)

## inspect results
head(results)
aggregate(results)

## plot results
plot(results, ylab = "Relative Aitchison distance")
plot(results, method = "density", xlab = "Relative Aitchison distance")
