
# Required R packages
library(nlme)
library(data.table)
library(survey)
library(VGAM)


source("./Funcs/MonteCarlo.R")
options(digits=6)


doSomething <- function(
  v1,v2,v3
)
{
  return (v1 * (v2-v3))
}


withParams <- function (i)
{
  v1 <- rtriangle(n = 1, theta = 900, 
            lower = 8,
            upper = 1810)
  v2 <- rtriangle(n = 1, theta = 3, 
                  lower = 1,
                  upper = 4)
  v3 <- rtriangle(n = 1, theta = -100, 
                  lower = -101,
                  upper = -99)
  calcArgs <- list(v1,v2,v3)
  return(calcArgs)
}




o <- vector()

#system.time({o <- CalcStableMonteCarlo("doSomething", doSomething(900,3,-100),doSomething, withParams)})
#plotStableMC(o)

system.time({o <- CalcASCBRMonteCarlo(92699,92800,100,2,"ASCBR doSomething",doSomething(900,3,-100),doSomething, withParams,dbgLvl=3)})
#plotStableMC(o)

