#' CalcMonteCarloSamples
#'
#' `CalcMonteCarloSamples` is a higher level function which will do some debug
#' capturing and bundling of the output.
#'
#' @return samples The return values from the calc function



#' @export
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

