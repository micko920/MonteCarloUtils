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
  testthat::expect_silent(GenMonteCarloSamples(calc, calc_args, 10))
  samples <- GenMonteCarloSamples(calc, calc_args, 10)
  expect_equal(length(samples), 10)
  expect_equal(samples, rep(11, 10)) # 10 function samples which return 11

  expect_equal(length(GenMonteCarloSamples(calc, calc_args)), 100)
})

test_that("Different parameter types", {
  # anonymous funcs
  testthat::expect_silent(GenMonteCarloSamples(function(v) {
    return(v + 1)
  }, function() {
    return(list(10))
  }, 10))
  samples <- GenMonteCarloSamples(function(v) {
    return(v + 1)
  }, function() {
    return(list(10))
  }, 10)
  expect_equal(length(samples), 10)
  expect_equal(samples, rep(11, 10)) # 10 function samples which return 11
})

test_that("Closure of args func", {
  calc_args <- function() {
    return(list(x))
  }

  x <- 10
  testthat::expect_equal(GenMonteCarloSamples(function(v) {
    return(v + 1)
  }, calc_args, 10), rep(11, 10))

  x <- 11
  testthat::expect_equal(GenMonteCarloSamples(function(v) {
    return(v + 1)
  }, calc_args, 10), rep(12, 10))
})

test_that("Functions with variables with variance", {
  calc_args <- function() {
    return(list(runif(1, min = 9, max = 11)))
  }

  set.seed(8121976)
  samples <- GenMonteCarloSamples(function(v) {
    return(v + 1)
  }, calc_args, 10000)

  testthat::expect_equal(round(mean(samples)), 11)
  compare_summary_equal(samples, 10.0, 10.5, 11.0, 11.0, 11.5, 12.0)
})
