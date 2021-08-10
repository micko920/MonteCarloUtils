#' CalcMonteCarloSamples
#'
#' `CalcMonteCarloSamples` is a higher level function which will do some debug
#' capturing and bundling of the output.
#'
#' @return samples The return values from the calc function





# @export
CalcMonteCarlo <- function(title, # Name to use on debug and plots
                           est, # CO2e estimate
                           calc, # Function to calculate CO2e from inputs
                           calcArgs, # function to create #MC runs of CO2e possibilities using uncertainties
                           iterations = MCRuns,
                           Confidence = CI, #  confidence interval
                           previous = vector()) {
  result <- StableMonteCarloSamples(calc, calcArgs, est, tolerance = MCTolerance, limit = iterations, debug = plot_mc_output)
  if (plot_mc_output) PlotMonteCarloSamples(result, est, title)
  return(list(value = SamplesWithBounds(est, result), MCresults = result))
}
#####

##### Value Models #####


# @export
vwuNorm <- function(v, n, ...) {
  sd <- (diff(range(v)) / (2 * qnorm(p = QUCI))) # LCI,UCI are 90% QCI (5%,95%)
  return(rnorm(n, mean = ValueWithUncertaintyValue(v), sd = sd))
}

# @export
vwuTriangle <- function(v, n, ...) {
  return(rtriangle(n = n, theta = ValueWithUncertaintyValue(v), lower = min(v), upper = max(v)))
}

# @export
create_vwuSampled <- function(value_samples) {
  return(function(v, n, ...) {
    return(sample(value_samples, size = n, replace = TRUE))
  })
}

