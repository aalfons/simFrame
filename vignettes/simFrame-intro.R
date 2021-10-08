### R code from vignette source 'simFrame-intro.Rnw'

###################################################
### code chunk number 1: simFrame-intro.Rnw:95-98
###################################################
options(width=75, prompt="R> ")
library("simFrame")
set.seed(1234)


###################################################
### code chunk number 2: simFrame-intro.Rnw:237-238
###################################################
showMethods("setNA")


###################################################
### code chunk number 3: simFrame-intro.Rnw:400-404
###################################################
nc <- NAControl(NArate = 0.05)
getNArate(nc)
setNArate(nc, c(0.01, 0.03, 0.05, 0.07, 0.09))
getNArate(nc)


###################################################
### code chunk number 4: simFrame-intro.Rnw:506-509
###################################################
library("mvtnorm")
dc <- DataControl(size = 10, distribution = rmvnorm, dots =
    list(mean = rep(0, 2), sigma = matrix(c(1, 0.5, 0.5, 1), 2, 2)))


###################################################
### code chunk number 5: simFrame-intro.Rnw:516-518
###################################################
foo <- generate(dc)
foo


###################################################
### code chunk number 6: simFrame-intro.Rnw:721-726
###################################################
data("eusilcP")
set <- setup(eusilcP, size = 10, k = 2)
summary(set)
set
draw(eusilcP[, c("id", "eqIncome")], set, i = 1)


###################################################
### code chunk number 7: simFrame-intro.Rnw:826-828
###################################################
cc <- DARContControl(target = "V2", epsilon = 0.2,
    fun = function(x) x * 100)


###################################################
### code chunk number 8: simFrame-intro.Rnw:844-846
###################################################
bar <- contaminate(foo, cc)
bar


###################################################
### code chunk number 9: simFrame-intro.Rnw:962-963
###################################################
nc <- NAControl(NArate = 0.3)


###################################################
### code chunk number 10: simFrame-intro.Rnw:971-972
###################################################
setNA(bar, nc)


###################################################
### code chunk number 11: simFrame-intro.Rnw:1119-1128
###################################################
data("eusilcP")
sc <- SampleControl(size = 500, k = 50)
cc <- DARContControl(target = "eqIncome", epsilon = 0.02,
    fun = function(x) x * 25)
sim <- function(x) {
    c(mean = mean(x$eqIncome), trimmed = mean(x$eqIncome, trim = 0.02))
}
set.seed(12345)
results <- runSimulation(eusilcP, sc, contControl = cc, fun = sim)


###################################################
### code chunk number 12: simFrame-intro.Rnw:1148-1152
###################################################
head(results)
aggregate(results)
tv <- mean(eusilcP$eqIncome)
tv


###################################################
### code chunk number 13: simFrame-intro.Rnw:1190-1191
###################################################
print(plot(results, true = tv))


###################################################
### code chunk number 14: simFrame-intro.Rnw:1193-1194
###################################################
print(simDensityplot(results, true = tv))


###################################################
### code chunk number 15: simFrame-intro.Rnw:1311-1314
###################################################
library("laeken")
data("eusilcP")
set.seed(12345)


###################################################
### code chunk number 16: simFrame-intro.Rnw:1321-1323
###################################################
set <- setup(eusilcP, design = "region", grouping = "hid",
    size = c(75, 250, 250, 125, 200, 225, 125, 150, 100), k = 100)


###################################################
### code chunk number 17: simFrame-intro.Rnw:1336-1338
###################################################
cc <- DCARContControl(target = "eqIncome", epsilon = 0.005,
    grouping = "hid", dots = list(mean = 500000, sd = 10000))


###################################################
### code chunk number 18: simFrame-intro.Rnw:1345-1355
###################################################
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


###################################################
### code chunk number 19: simFrame-intro.Rnw:1362-1364
###################################################
results <- runSimulation(eusilcP, set, contControl = cc,
    design = "gender", fun = sim, k = 125)


###################################################
### code chunk number 20: simFrame-intro.Rnw:1371-1373
###################################################
head(results)
aggregate(results)


###################################################
### code chunk number 21: simFrame-intro.Rnw:1380-1381
###################################################
tv <- simSapply(eusilcP, "gender", function(x) gini(x$eqIncome)$value)


###################################################
### code chunk number 22: simFrame-intro.Rnw:1387-1388
###################################################
print(plot(results, true = tv, xlab = "Gini coefficient"))


###################################################
### code chunk number 23: simFrame-intro.Rnw:1453-1456
###################################################
library("robCompositions")
library("mvtnorm")
set.seed(12345)


###################################################
### code chunk number 24: simFrame-intro.Rnw:1485-1489
###################################################
crnorm <- function(n, mean, sigma) isomLRinv(rmvnorm(n, mean, sigma))
sigma <- matrix(c(1, -0.5, 1.4, -0.5, 1, -0.6, 1.4, -0.6, 2), 3, 3)
dc <- DataControl(size = 150, distribution = crnorm,
    dots = list(mean = c(0, 2, 3), sigma = sigma))


###################################################
### code chunk number 25: simFrame-intro.Rnw:1496-1497
###################################################
nc <- NAControl(NArate = 0.05)


###################################################
### code chunk number 26: simFrame-intro.Rnw:1504-1511
###################################################
sim <- function(x, orig) {
    i <- apply(x, 1, function(x) any(is.na(x)))
    ni <- length(which(i))
    xKNNa <- impKNNa(x)$xImp
    xLS <- impCoda(x, method = "lm")$xImp
    c(knn = aDist(xKNNa, orig)/ni, LS = aDist(xLS, orig)/ni)
}


###################################################
### code chunk number 27: simFrame-intro.Rnw:1516-1517
###################################################
results <- runSimulation(dc, nrep = 50, NAControl = nc, fun = sim)


###################################################
### code chunk number 28: simFrame-intro.Rnw:1523-1525
###################################################
head(results)
aggregate(results)


###################################################
### code chunk number 29: simFrame-intro.Rnw:1532-1533
###################################################
print(plot(results, xlab = "Relative Aitchison distance"))


###################################################
### code chunk number 30: simFrame-intro.Rnw:1535-1537
###################################################
alpha <- if(names(dev.cur()) == "pdf") 0.6 else 1
print(simDensityplot(results, alpha = alpha, xlab = "Relative Aitchison distance"))


###################################################
### code chunk number 31: simFrame-intro.Rnw:1577-1578
###################################################
cl <- makeCluster(2, type="PSOCK")


###################################################
### code chunk number 32: simFrame-intro.Rnw:1584-1589
###################################################
clusterEvalQ(cl, {
        library("simFrame")
        library("robCompositions")
        library("mvtnorm")
    })


###################################################
### code chunk number 33: simFrame-intro.Rnw:1594-1595
###################################################
clusterSetRNGStream(cl, iseed=12345)


###################################################
### code chunk number 34: simFrame-intro.Rnw:1603-1615
###################################################
crnorm <- function(n, mean, sigma) isomLRinv(rmvnorm(n, mean, sigma))
sigma <- matrix(c(1, -0.5, 1.4, -0.5, 1, -0.6, 1.4, -0.6, 2), 3, 3)
dc <- DataControl(size = 150, distribution = crnorm,
    dots = list(mean = c(0, 2, 3), sigma = sigma))
nc <- NAControl(NArate = c(0.01, 0.03, 0.05, 0.07, 0.09))
sim <- function(x, orig) {
    i <- apply(x, 1, function(x) any(is.na(x)))
    ni <- length(which(i))
    xKNNa <- impKNNa(x)$xImp
    xLS <- impCoda(x, method = "lm")$xImp
    c(knn = aDist(xKNNa, orig)/ni, LS = aDist(xLS, orig)/ni)
}


###################################################
### code chunk number 35: simFrame-intro.Rnw:1623-1624
###################################################
clusterExport(cl, c("crnorm", "sigma", "dc", "nc", "sim"))


###################################################
### code chunk number 36: simFrame-intro.Rnw:1629-1630
###################################################
results <- clusterRunSimulation(cl, dc, nrep = 50, NAControl = nc, fun = sim)


###################################################
### code chunk number 37: simFrame-intro.Rnw:1636-1637
###################################################
stopCluster(cl)


###################################################
### code chunk number 38: simFrame-intro.Rnw:1644-1646
###################################################
head(results)
aggregate(results)


###################################################
### code chunk number 39: simFrame-intro.Rnw:1652-1653
###################################################
print(plot(results, ylab = "Relative Aitchison distance"))


###################################################
### code chunk number 40: simFrame-intro.Rnw:1655-1658
###################################################
alpha <- if(names(dev.cur()) == "pdf") 0.6 else 1
print(simDensityplot(results, NArate=0.07,
        alpha = alpha, xlab = "Relative Aitchison distance"))


