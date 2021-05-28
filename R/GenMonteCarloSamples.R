#' GenMonteCarloSamples
#'
#' `GenMonteCarloSamples` returns a list of samples from executing the function
#' with arguments
#'
#' @param calc A function to sample
#' @param calc_args A function to create arguments for the func being sampled
#' @param iterations The number of samples to create
#' @return samples The return values from the calc function
#' @examples
#'
#' calc_estimate <- function(a, ef) {
#'   return(a * ef)
#' }
#' calc_args <- function() {
#'   area <- 10
#'   emission_factor <- rtriangle(
#'     n = 1, theta = ef_defor,
#'     lower = ef_defor_lci,
#'     upper = ef_defor_uci
#'   )
#'   return(list(area, emission_factor))
#' }
#' GenMonteCarloSamples(calc_estimate, calc_args, 1000) # returns object
#' @export

# @export
GenMonteCarloSamples <- function(calc, # Function to sample
                                 calc_args, # function to create arguments for func being sampled
                                 iterations = 100 # Number of times to sample function
) {
  return(replicate(iterations, do.call(calc, calc_args())))
}
