#' PlotMonteCarloSamples
#'
#' `PlotMonteCarloSamples` plots the stable analysis
#'
#' @param o Debug output from StableMonteCarloSamples
#' @return graph of the stable analysis
#' @export

# @export
PlotMonteCarloSamples <- function(o, est, title = "") {
  result <- list()


  s <- quantile(o, probs = c(0, 0.05, 0.5, 0.95, 1))

  values <- subset(o, o > s[1] & o < s[5])
  if (!length(values) > 0) values <- o
  h <- hist(values,
    col = "gray40", breaks = 20, main = title,
    xlim = c(s[1], s[5]),
    xlab = "Stable MC Estimates"
  )
  result <- append(result, h)

  box <- boxplot(o,
    ylab = title,
    xlab = "samples",
    horizontal = TRUE
  )
  points(c(s[2], est, s[4]),
    y = c(1, 1, 1),
    col = c("orange", "red", "blue"),
    pch = c("[", "+", "]"),
    cex = 2
  )
  legend("topleft",
    legend = c(paste("5%", signif(s[2], 8)), paste("estimate: ", signif(est, 8)), paste("95%", signif(s[4], 8))),
    col = c("orange", "red", "blue"),
    lty = 1, cex = 0.8,
    pch = c("[", "+", "]"),
    horiz = TRUE
  )
  result <- append(result, box)


  if ("debug" %in% attributes(attributes(o))$names) {
    mcdebug <- attr(o, "debug")
    pSD <- plot(mcdebug$samples, ylab = "SD of CI width for each set of MC estimates", xlab = "Iterations (log)", log = "x")

    pQ <- plot(NULL,
      xlim = c(10, mcdebug$total_runs),
      ylim = c(s[1], s[5]),
      xlab = "Iterations (log)", ylab = "CI", log = "x"
    )
    result <- append(result, pSD)
    result <- append(result, pQ)

    abline(h = mcdebug$summary[3], untf = FALSE, col = "gray60")
    points(mcdebug$uci_samples, col = "blue", pch = "*")
    lines(mcdebug$uci_samples, col = "blue", lty = 1)
    points(mcdebug$lci_samples, col = "orange", pch = "*")
    lines(mcdebug$lci_samples, col = "orange", lty = 2)
    abline(v = mcdebug$total_runs, untf = FALSE, col = "green", )
    legend("topleft",
      legend = c(paste0("total runs = ", mcdebug$total_runs)),
      col = c("green"), lty = 1, cex = 0.8
    )
  }

  return(result)
}
