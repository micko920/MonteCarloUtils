#' StableMonteCarloSamples
#'
#' `StableMonteCarloSamples` returns a list of samples from executing the function
#' with arguments. This differs to the GenMonteCarloSamples by traking the
#' samples and stopping when a confidence interval from an estimate is reached.
#'
#' In hindsite after writing this by experimentation with a logistics curve
#' concept I realised that this could be done with a PID method.
#'
#'
#' @param calc A function to sample
#' @param calc_args A function to create arguments for the func being sampled
#' @param iterations The number of samples to create
#' @return samples The return values from the calc function
#' @examples
#'
#' calc_estimate <- function(v, ef) {
#'   return(v + 1)
#' }
#' calc_args <- function() {
#'   return(list(10))
#' }
#' StableMonteCarloSamples(calc_estimate, calc_args, 11) # returns samples
#' @export
StableMonteCarloSamples <- function(calc,
                                    calc_args,
                                    estimate,
                                    tolerance = 10^(-min(round(log10(abs(estimate))), 2.1)),
                                    confidence = 0.9, #  confidence interval
                                    start = 10,
                                    limit = 1e+8,
                                    rolling_window = 3,
                                    debug = FALSE,
                                    trace = FALSE) {
  runs <- start - 1 - rolling_window

  if (trace) print(c(estimate, tolerance))
  lowerCI <- (1 - confidence) / 2
  upperCI <- (1 - lowerCI)

  mcSD <- matrix(ncol = 2, nrow = 0, dimnames = list(c(), c("Runs", "v")))
  mcLCI <- matrix(ncol = 2, nrow = 0, dimnames = list(c(), c("Runs", "v")))
  mcUCI <- matrix(ncol = 2, nrow = 0, dimnames = list(c(), c("Runs", "v")))

  mcResults <- vector()
  result <- matrix(ncol = 3, nrow = 0, dimnames = list(c(), c("LCI", "UCI", "DIFF")))
  loops <- 0


  trend <- 1
  while ((length(mcSD) < rolling_window) | (trend > tolerance) & (runs < limit)) {
    if (length(mcSD) > rolling_window) {
      tt <- tail(na.omit(mcSD[, 2]), rolling_window)
      if (length(tt) >= rolling_window) {
        if (mean(tt) == 0) {
          trend <- tolerance
        } else {
          trend <- sd(tt) / mean(tt)
        }
      }
      step <- round((1 + log(trend / tolerance)) * (0.1^log10(tolerance)))
    } else {
      trend <- 1
      step <- 1
    }
    runs <- min(runs + step, limit)
    batch <- runs - length(mcResults)
    if (batch > 0) mcResults <- append(mcResults, GenMonteCarloSamples(calc, calc_args, batch))
    qq <- quantile(mcResults, probs = c(lowerCI, 0.5, upperCI))
    if (trace) print(c(runs, qq, tolerance, trend))
    result <- rbind(result, c(qq[[1]], qq[[3]], qq[[3]] - qq[[1]]))
    mcSD <- rbind(mcSD, c(runs, sd(result[, "DIFF"])))
    mcLCI <- rbind(mcLCI, c(runs, mean(result[, "LCI"])))
    mcUCI <- rbind(mcUCI, c(runs, mean(result[, "UCI"])))
    loops <- loops + 1
  }
  if (trace) print(c(loops, runs))
  if (debug) {
    attr(mcResults, "debug") <- list(summary = summary(mcResults), total_runs = runs, samples = mcSD, lci_samples = mcLCI, uci_samples = mcUCI)
  }
  return(mcResults)
}
