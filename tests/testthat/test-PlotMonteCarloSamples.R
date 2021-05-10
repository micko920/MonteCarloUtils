test_that("Simple constant calculation", {
  calc <- function(v) {
    return(v + 1)
  }

  calc_args <- function() {
    return(list(10))
  }
  testthat::expect_output(samples <- StableMonteCarloSamples(calc, calc_args, 11, limit = 100, debug = TRUE))
  testthat::expect_silent(plot <- PlotMonteCarloSamples(samples))
})

test_that("Functions with variables with large variance", {
  calc_args <- function() {
    return(list(runif(1, min = 0, max = 1000)))
  }

  set.seed(8121976)
  testthat::expect_output(samples <- StableMonteCarloSamples(function(v) {
    return(v + 1)
  }, calc_args, 501, tolerance = 0.00805, limit = 1.5e+04, debug = TRUE))
  # only does 64 stop condition evaluations

  testthat::expect_silent(plot <- PlotMonteCarloSamples(samples))


  # this needs b to be larger than 1 so as not to trend to infinity
  calc_args <- function() {
    return(list(runif(1, min = 0, max = 1000), runif(1, min = 1, max = 10)))
  }

  set.seed(8121976)
  testthat::expect_output(samples <- StableMonteCarloSamples(function(a, b) {
    return(a / b)
  }, calc_args, 90, tolerance = 0.013459, limit = 3.0e+03, debug = TRUE))
  # only does 15 stop condition evaluations

  testthat::expect_silent(plot <- PlotMonteCarloSamples(samples))
})

test_that("Functions with bad formulas and variance", {
  calc_args <- function() {
    return(list(runif(1, min = 1, max = 5)))
  }

  set.seed(8121976)
  testthat::expect_output(samples <- StableMonteCarloSamples(function(v) {
    return(10^v + v^3)
  }, calc_args, 10653, tolerance = 0.00805, limit = 1.5e+04, debug = TRUE))
  # only does 56 stop condition evaluations

  testthat::expect_silent(plot <- PlotMonteCarloSamples(samples))

  # this needs b to be larger than 1 so as not to trend to infinity
  calc_args <- function() {
    return(list(runif(1, min = 0, max = 1000), runif(1, min = 0.1, max = 10)))
  }

  set.seed(8121976)
  testthat::expect_output(samples <- StableMonteCarloSamples(function(a, b) {
    return(a / (3.0^b))
  }, calc_args, 1.42, tolerance = 0.019, limit = 3.0e+03, debug = TRUE))
  # only does 27 stop condition evaluations

  testthat::expect_silent(plot <- PlotMonteCarloSamples(samples))
})
