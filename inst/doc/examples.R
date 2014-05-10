### R code from vignette source 'examples.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: sweave
###################################################
options(SweaveHooks=list(## options for all lattice figures
fig=function() {require(lattice)
  require(grid)
  ltheme <- canonical.theme(color = FALSE)     ## in-built B&W theme  
  ltheme$strip.background$col <- "transparent" ## change strip bg  
  lattice.options(default.theme = ltheme)      ## set as default  
  trellis.par.set(list(superpose.symbol = list(pch = 15:17),fontsize=list(text=14)))
}))


###################################################
### code chunk number 2: examples.Rnw:28-34 (eval = FALSE)
###################################################
## 
## # loading all for debugging
## source("~/repos/lib/rForge/power/pkg/power/R/AllGenerics.R")
## source("~/repos/lib/rForge/power/pkg/power/R/power.R")
## # for plotting
## library(lattice)


###################################################
### code chunk number 3: examples.Rnw:38-39
###################################################
library(sse)


###################################################
### code chunk number 4: pPmini
###################################################

psi <- powPar(theta = seq(from = 0.5, to = 1.5, by = 0.05),
              n = seq(from = 20, to = 60, by = 2))


###################################################
### code chunk number 5: pFmini
###################################################

powFun <- function(psi)
{
  return(power.t.test(n = n(psi)/2, 
                      delta = theta(psi),
                      sig.level = 0.05)$power)
}


###################################################
### code chunk number 6: pCmini
###################################################

calc <- powCalc(psi, statistic = powFun)


###################################################
### code chunk number 7: pEmini
###################################################

ex <- powEx(theta = 1, power = 0.9)


###################################################
### code chunk number 8: pMmini
###################################################

pow <- merge(calc, ex)


###################################################
### code chunk number 9: inspectmini
###################################################
getOption("SweaveHooks")[["fig"]]()

inspect(pow)


###################################################
### code chunk number 10: plotmini
###################################################
getOption("SweaveHooks")[["fig"]]()

plot(pow,
     xlab = "Effect size",
     ylab = "Total sample size", 
     label.pos = c(0,1))


###################################################
### code chunk number 11: texmini
###################################################

tex(pow, type = "nEval")


###################################################
### code chunk number 12: pFresample
###################################################

powFun.resample <- function(psi)
{
  x <- rnorm(n(psi)/2)
  y <- rnorm(n(psi)/2) + theta(psi)
  return(wilcox.test(x = x, y = y)$p.value < 0.05)
}


###################################################
### code chunk number 13: pCresample
###################################################

calc.resample <- powCalc(psi, statistic = powFun.resample, n.iter = 99)


###################################################
### code chunk number 14: examples.Rnw:179-181 (eval = FALSE)
###################################################
## 
## save(calc.resample, file = "dat/calc.resample.rda")


###################################################
### code chunk number 15: examples.Rnw:184-186 (eval = FALSE)
###################################################
## 
## load(file = "dat/calc.resample.rda")


###################################################
### code chunk number 16: pMresample
###################################################


ex <- powEx(theta = 1, power = 0.9)
pow.resample <- merge(calc.resample, ex)


###################################################
### code chunk number 17: inspectresample
###################################################
getOption("SweaveHooks")[["fig"]]()

inspect(pow.resample)


###################################################
### code chunk number 18: examples.Rnw:225-226 (eval = FALSE)
###################################################
## tex(pow.resample, type = "sampling")


###################################################
### code chunk number 19: examples.Rnw:231-232 (eval = FALSE)
###################################################
## tex(pow.resample, type = "n.iter")


