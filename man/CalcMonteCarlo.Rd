% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CalcMonteCarloSamles.R
\name{CalcMonteCarlo}
\alias{CalcMonteCarlo}
\title{CalcMonteCarloSamples}
\usage{
CalcMonteCarlo(
  title,
  est,
  calc,
  calcArgs,
  iterations = MCRuns,
  Confidence = CI,
  previous = vector()
)
}
\value{
samples The return values from the calc function
}
\description{
\code{CalcMonteCarloSamples} is a higher level function which will do some debug
capturing and bundling of the output.
}
