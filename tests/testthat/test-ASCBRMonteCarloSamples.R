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
  testthat::expect_silent(ASCBRMonteCarloSamples(calc, calc_args, 11, 11, 2, 2, limit = 100))
  samples <- ASCBRMonteCarloSamples(calc, calc_args, 11, 11, 2, 2, limit = 110)
  expect_equal(length(samples), 100)
  expect_lt(length(samples), 110)
  compare_summary_equal(samples, 11, 11, 11, 11, 11, 11)
})

test_that("Functions with variables with variance", {
  calc_args <- function() {
    return(list(runif(1, min = 9, max = 11)))
  }

  set.seed(8121976)
  samples <- ASCBRMonteCarloSamples(function(v) {
    return(v + 1)
  }, calc_args, 11, 11, 2, 2, limit = 150)

  testthat::expect_equal(round(mean(samples)), 11)
  compare_summary_equal(samples, 10.0, 10.5, 10.9, 10.9, 11.4, 12.0)
  expect_equal(length(samples), 100)
  expect_lt(length(samples), 110)
})

test_that("Functions with variables with large variance", {
  calc_args <- function() {
    return(list(runif(1, min = 0, max = 1000)))
  }

  set.seed(8121976)
  samples <- ASCBRMonteCarloSamples(function(v) {
    return(v + 1)
  }, calc_args, 501, 501, 0.352, 205, limit = 1.5e+04)

  testthat::expect_equal(round(mean(samples)), 501)
  compare_summary_equal(samples, 1.08, 254.0, 503.0, 501.0, 750.0, 1000)
  expect_equal(length(samples), 7909)
  expect_lt(length(samples), 1.5e+04)


  # this needs b to be larger than 1 so as not to trend to infinity
  calc_args <- function() {
    return(list(runif(1, min = 0, max = 1000), runif(1, min = 1, max = 10)))
  }

  set.seed(8121976)
  samples <- ASCBRMonteCarloSamples(function(a, b) {
    return(a / b)
  }, calc_args, 90, 90, 0.04, 10, limit = 3.0e+03)

  testthat::expect_equal(round(median(samples)), 91)
  compare_summary_equal(samples, 0.01320, 47.00, 91.20, 127.00, 156.00, 964.00)
  expect_equal(length(samples), 2260)
  expect_lt(length(samples), 3.0e+03)
})

test_that("Functions with bad formulas and variance", {
  calc_args <- function() {
    return(list(runif(1, min = 1, max = 5)))
  }

  set.seed(8121976)
  samples <- ASCBRMonteCarloSamples(function(v) {
    return(10^v + v^3)
  }, calc_args, 10607, 10607, 0.9, 9, limit = 1.5e+04)

  testthat::expect_equal(round(mean(samples)), 10607)
  compare_summary_equal(samples, 11.00, 107.00, 1040.00, 10600.00, 9810.00, 99800)
  expect_equal(length(samples), 13693)
  expect_lt(length(samples), 1.5e+04)


  calc_args <- function() {
    return(list(runif(1, min = 0, max = 1000), runif(1, min = 0.1, max = 10)))
  }

  set.seed(8121976)
  samples <- ASCBRMonteCarloSamples(function(a, b) {
    return(a / (3.0^b))
  }, calc_args, 1.4373283, 1.4373283, 0.01333503, 5, limit = 3.0e+03)

  testthat::expect_equal(signif(median(samples),8), 1.4373283)
  compare_summary_equal(samples, 0.0000606, 0.09860, 1.440, 40.900, 22.400, 859.000)
  expect_equal(length(samples), 2571)
  expect_lt(length(samples), 3.0e+03)
})
