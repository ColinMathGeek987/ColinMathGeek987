#' ntickets function
#'
#' @param N the number of seats in the flight
#' @param gamma the probability a plane will be truly overbooked (more people show than there are seats)
#' @param p the probability a passenger will show up to the flight
#'
#' @importFrom stats qbinom pbinom uniroot
#' @importFrom graphics abline
#'
#' @return A list containing:
#' \describe{
#'   \item{nd}{The number of tickets to sell based on the discrete binomial method.}
#'   \item{nc}{The number of tickets to sell based on the normal approximation method.}
#'   \item{N}{The number of seats on the flight.}
#'   \item{p}{The probability a passenger shows up.}
#'   \item{gamma}{The probability of overbooking.}
#' }
#'
#' @details
#' The function also generates two plots:
#' \itemize{
#'   \item A plot of the objective function vs. `n` for the **discrete binomial case**.
#'   \item A plot of the objective function vs. `n` for the **continuous normal approximation**.
#' }
#'
#' @examples
#' ntickets(N = 400,gamma = 0.02, p = 0.95)
#'
#' @export
ntickets <- function(N, gamma, p) {
  find_nd <- function(N, gamma, p) {
    val = 0
    n = N
    while(val < N) {
      n = n + 1
      val = qbinom(1 - gamma, n, p)
    }
    n
  }

  # 2. Normal Approximation Method
  find_nc <- function(N, gamma, p) {
    f <- function(n) {
      pnorm(N + 0.5, mean = n * p, sd = sqrt(n * p * (1 - p))) - (1 - gamma)
    }
    result <- uniroot(f, lower = N, upper = N * 2)$root
    return(ceiling(result))
  }

  # Compute nd and nc
  nd <- find_nd(N, gamma, p)
  nc <- find_nc(N, gamma, p)

  # Create a named list
  result <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)

  # Plot Objective Function for Discrete Case
  n_vals <- seq(N, nd + 20, by = 1)
  obj_discrete <- sapply(n_vals, (function(n) - pbinom(N, n, p) + (1 - gamma)))

  plot(n_vals, obj_discrete, type = "o", col = "blue", pch = 16, lwd = 2,
       ylab = "Objective Function", xlab = "n",
       main = paste("Objective vs n (Discrete) - gamma =", gamma, "N =", N))
  abline(h = 0, col = "red", lwd = 2)
  abline(v = nd, col = "red", lwd = 2)

  # Plot Objective Function for Normal Approximation
  obj_continuous <- sapply(n_vals, function(n) -(pnorm(N + 0.5, mean = n * p, sd = sqrt(n * p * (1 - p))) - (1 - gamma)))

  plot(n_vals, obj_continuous, type = "l", col = "black", lwd = 2,
       ylab = "Objective Function", xlab = "n",
       main = paste("Objective vs n (Continuous) - gamma =", gamma, "N =", N))
  abline(h = 0, col = "blue", lwd = 2)
  abline(v = nc, col = "blue", lwd = 2)

  print(result)
  invisible(result)
}
