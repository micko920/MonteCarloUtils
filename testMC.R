# Required R packages
library(nlme)
library(data.table)
library(survey)
library(VGAM)
options(digits=6)


doMC <- function(
  calc, calcArgs,iterations = 10, CI = 0.9
)
{
  result <- vector()
  for (i in 1:iterations){
    result[i] <- do.call(calc,calcArgs(i))
  }
  QLCI = unlist(quantile(result, prob = (1 - CI) / 2)[1])
  QUCI = unlist(quantile(result, prob = 1 - ((1 - CI) / 2))[1])
  return(list(QLCI,QUCI))
}



doSomething <- function(
  v1,v2,v3
)
{
  return (v1 * (v2-v3))
}


#mcd <- vector()


#par <- rtriangle(n = 10, theta = 90,
#                 lower = 10,
#                 upper = 800)

#par <- rep(10,10)
#for (i in 1:10){ # i <- 1
# mcd[i] <- doSomething(par[i],1,2)
#}

#mcd
#o <- doMC( rtriangle(n = 1, theta = 10,
#                lower = 7,
#                upper = 13))





withParams <- function (i)
{
  v1 <- rtriangle(n = 1, theta = 90,
                  lower = 10,
                  upper = 800)
  v2 <- rtriangle(n = 1, theta = 3,
                  lower = -32,
                  upper = 5494)
  v3 <- rtriangle(n = 1, theta = -100,
                  lower = -45324,
                  upper = 5494)
  calcArgs <- list(v1,v2,v3)
  return(calcArgs)
}

o <- doMC(doSomething, withParams,10)

o