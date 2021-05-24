#' ASCBRMonteCarloSamples
#'
#' `ASCBRMonteCarloSamples` returns a list of samples from executing the function
#' with arguments. This differs to the GenMonteCarloSamples by tracking the
#' samples and stopping when a confidence interval from an estimate is reached.
#' Ata, Mustafa Y.. (2007). A convergence criterion for the Monte Carlo
#' estimates. Simulation Modeling Practice and Theory. 15. 237-246.
#' 10.1016/j.simpat.2006.12.002.
#'
#' This algorithm has a bug with stable non variant functions. The 100
#' minimum run would push the value of z outside the value of zeta and so would
#' never end the internal loop. This condition was change from z!=zeta to
#' z>zeta to solve this problem.
#'
#' Using this function requires some pre existing understanding of the range
#' (CI) to allow the values for zeta, upper and lower values. If set incorrectly
#' these will mean the algorithm will not end. The limit was added to stop
#' these issues
#'
#' This algorithm evaluates the end condition for every run of the calculation.
#' This does not optimise well for unstable functions. It does not do testing
#' of the end condition after batches or do estimates of the next batch size
#' before testing again.S
#'
#' This algorithm has type range issues for the end condition. If the sum of
#' the results is larger than the type can hold it will overflow.
#'
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
#' ASCBRMonteCarloSamples(calc_estimate, calc_args, 11) # returns samples
#'
#'
#' # @export
ASCBRMonteCarloSamples <- function(calc,
                                   calc_args,
                                   estimate_upper,
                                   estimate_lower,
                                   err, zeta,
                                   limit = 1e+8,
                                   debug = FALSE) {

  # Initialize:
  u <- estimate_upper
  l <- estimate_lower
  total <- 0
  j <- 0
  z <- 0
  mu <- 0
  x <- 0

  results <- vector()

  # Do Steps 1 to until z_j = zeta
  while ((j < 100) | ((z < zeta) & (j < limit))) {

    # step 1
    j <- j + 1

    # step 1, generate x_j
    results[length(results) + 1] <- do.call(calc, calc_args())

    x <- tail(results, 1)

    # step 2
    total <- total + x
    mu <- total / j

    # step 3
    if ((l <= mu) & (mu <= u)) { # sig = 0

      # step 4 z != 0
      u <- u
      l <- l

      # step 5
      z <- z + 1
    } else { # sig != 0

      # step 4
      u <- mu + err
      l <- mu - err

      # step 5
      z <- 0
    }
    if (debug && (((j %% 10000) == 0) | (z > 1))) {
      print(debugASCBRMC(u, l, total, mu, x, j, z, zeta))
    }
  }

  # step 6 - changed for this application
  return(results)
}

debugASCBRMC <- function(u, l, total, mu, x, j, z, zeta) {
  o <- c(u, l, total, mu, x, j, z - zeta)
  names(o) <- c("u", "l", "total", "mu", "x", "j", "z-zeta")
  return(o)
}
