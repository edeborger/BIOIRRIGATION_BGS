# Bioirrigation fitting function

## 1. Cost function
## This function calculates the cost of a certain set of parameter values.

URAcost              <- function(p, corepars, measured.data, verbose = FALSE){
  parms              <- c(p, corepars)
  out                <- run.Uranine(parms, hours = 24, rtol = 1e-8, atol = 1e-12)
  out[,2]            <- log(out[,2])
  measured.data$UrOW <- log(measured.data$UrOW)
  cost <- modCost(model = out, obs = measured.data)
  if(verbose) print(c(p, cost = cost$model))
  return(cost = cost)
}