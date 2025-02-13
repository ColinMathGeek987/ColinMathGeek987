#' distri Function - Binomial Simulation
#'
#' @description Performs a binomial distribution simulation and returns the relative frequencies of successes along with a bar plot.
#'
#' @param iter Integer. The number of iterations (simulations) to perform.
#' @param n Integer. The number of trials in each simulation.
#' @param p Numeric (0 ≤ p ≤ 1). The probability of success in a single trial.
#'
#' @return A named vector of relative frequencies of successes. The function also generates a bar plot displaying the distribution of successes.
#'
#' @details This function creates a matrix to hold samples, calculates the number of successes in each trial, and generates a bar plot displaying the distribution of successes.
#'
#' @export
#'
#' @examples
#' # Example usage:
#' distri(iter = 1000, n = 10, p = 0.5)
#'
#'
distri = function(iter = 100, n = 10, p = 0.5) {  # make a matrix to hold the samples
  sam.mat = matrix(NA, nr = n, nc = iter, byrow = TRUE) #initially filled with NA's
  succ = c() #Make a vector to hold the number of successes in each trial
  for(i in 1:iter) {
    sam.mat[,i] = sample(c(1,0), n, replace = TRUE, prob = c(p, 1 - p)) #Fill each column with a new sample
    succ[i] = sum(sam.mat[,i]) #Calculate a statistic from the sample (this case it is the sum)
  }
  succ.tab = table(factor(succ,levels = 0:n)) #Make a table of successes
  barplot(succ.tab/(iter), col = rainbow(n + 1), main = "Binomial simulation", xlab = "Number of successes") #Make a barplot of the proportions
  succ.tab/iter
}
