#' PlotMonteCarloSamples
#'
#' `PlotMonteCarloSamples` plots the stable analysis
#'
#' @param o Debug output from StableMonteCarloSamples
#' @return graph of the stable analysis
#' @export

# @export
PlotMonteCarloSamples <- function(o) {
  par(mfrow = c(3, 1))
  mcdebug <- attr(o,"debug")

  result <- list(
    hist(o,
      col = "gray40", breaks = 20, main = "",
      xlim = c(min(o), 1.2 * max(o)),
      xlab = "Stable MC Estimates"
    ),
    plot(mcdebug$samples, ylab = "SD of each set of MC estimates", xlab = "Iterations", log = "x"),
    plot(NULL,
      xlim = c(10, mcdebug$total_runs),
      ylim = c(min(mcdebug$lci_samples[, 2]), max(which(mcdebug$uci_samples[, 2] < Inf))),
      xlab = "Iterations", ylab = "Qunatiles", log = "x"
    )
  )

  abline(h = mcdebug$summary[3], untf = FALSE, col = "gray60")
  points(mcdebug$uci_samples, type = "o", col = "blue", pch = "o")
  lines(mcdebug$uci_samples, col = "blue", lty = 1)
  points(mcdebug$lci_samples, col = "red", pch = "*")
  lines(mcdebug$lci_samples, col = "red", lty = 2)
  abline(v = mcdebug$total_runs, untf = FALSE, col = "green", )
  text(mcdebug$total_runs * (7 / 8),
       max(mcdebug$lci_samples) + ((max(mcdebug$uci_samples) - max(mcdebug$lci_samples)) / 2),
       paste0("total runs = ", mcdebug$total_runs), col = "black", )

  return(result)
}
