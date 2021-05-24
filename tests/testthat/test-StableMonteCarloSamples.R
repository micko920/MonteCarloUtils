compare_summary_equal <- function(samples, min, qtr1, med, u, qtr3, max, ...) {
  sample_summary <- stats::quantile(samples)
  sample_summary <- signif(c(sample_summary[1L:3L], mean(samples), sample_summary[4L:5L]), 3)
  names(sample_summary) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")

  expect_summary <- c(min, qtr1, med, u, qtr3, max)
  names(expect_summary) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")

  return(expect_equal(sample_summary, expect_summary, ...))
}


test_that("Simple constant calculation", {
  calc <- function(v) {
    return(v + 1)
  }

  calc_args <- function() {
    return(list(10))
  }
  testthat::expect_silent(StableMonteCarloSamples(calc, calc_args, 11))
  samples <- StableMonteCarloSamples(calc, calc_args, 11, limit = 100)
  # only does 5 stop condition evaluations
  expect_equal(length(samples), 84)
  expect_lt(length(samples), 100)
  compare_summary_equal(samples, 11, 11, 11, 11, 11, 11)
})

test_that("Functions with variables with variance", {
  calc_args <- function() {
    return(list(runif(1, min = 9, max = 11)))
  }

  set.seed(8121976)
  samples <- StableMonteCarloSamples(function(v) {
    return(v + 1)
  }, calc_args, 11,
  tolerance = 0.098,
  limit = 150
  )
  # only does 6 stop condition evaluations

  testthat::expect_equal(round(mean(samples)), 11)
  compare_summary_equal(samples, 10.0, 10.5, 10.9, 10.9, 11.4, 12.0)
  expect_equal(length(samples), 107)
  expect_lt(length(samples), 150)
})

test_that("Functions with variables with large variance", {
  calc_args <- function() {
    return(list(runif(1, min = 0, max = 1000)))
  }

  set.seed(8121976)
  samples <- StableMonteCarloSamples(function(v) {
    return(v + 1)
  }, calc_args, 501, tolerance = 0.00805, limit = 1.5e+04)
  # only does 64 stop condition evaluations

  testthat::expect_equal(round(mean(samples)), 499)
  compare_summary_equal(samples, 1.01, 251.0, 500.0, 499.0, 747.0, 1000)
  expect_equal(length(samples), 14836)
  expect_lt(length(samples), 1.5e+04)


  # this needs b to be larger than 1 so as not to trend to infinity
  calc_args <- function() {
    return(list(runif(1, min = 0, max = 1000), runif(1, min = 1, max = 10)))
  }

  set.seed(8121976)
  samples <- StableMonteCarloSamples(function(a, b) {
    return(a / b)
  }, calc_args, 90, tolerance = 0.013459, limit = 3.0e+03)
  # only does 15 stop condition evaluations

  testthat::expect_equal(round(median(samples)), 93)
  compare_summary_equal(samples, 0.01320, 48.10, 92.50, 128.00, 157.00, 964.00)
  expect_equal(length(samples), 2605)
  expect_lt(length(samples), 3.0e+03)
})

test_that("Functions with bad formulas and variance", {
  calc_args <- function() {
    return(list(runif(1, min = 1, max = 5)))
  }

  set.seed(8121976)
  samples <- StableMonteCarloSamples(function(v) {
    return(10^v + v^3)
  }, calc_args, 10653, tolerance = 0.00805, limit = 1.5e+04)
  # only does 56 stop condition evaluations

  testthat::expect_equal(round(mean(samples)), 10653)
  compare_summary_equal(samples, 11.00, 107.00, 1040.00, 10700.00, 9880.00, 99800)
  expect_equal(length(samples), 13398)
  expect_lt(length(samples), 1.5e+04)


  # this needs b to be larger than 1 so as not to trend to infinity
  calc_args <- function() {
    return(list(runif(1, min = 0, max = 1000), runif(1, min = 0.1, max = 10)))
  }

  set.seed(8121976)
  samples <- StableMonteCarloSamples(function(a, b) {
    return(a / (3.0^b))
  }, calc_args, 1.42, tolerance = 0.019, limit = 3.0e+03)
  # only does 27 stop condition evaluations

  testthat::expect_equal(signif(median(samples), 8), 1.4135286)
  compare_summary_equal(samples, 0.0000606, 0.10100, 1.410, 41.000, 22.900, 859.000)
  expect_equal(length(samples), 2812)
  expect_lt(length(samples), 3.0e+03)
})
