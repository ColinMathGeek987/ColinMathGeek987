#' myplotty function
#'
#' @param value A numeric vector of data values (e.g., a column from a dataset).
#' This represents the values for which Z-scores are to be computed.
#'
#' @param dataset A numeric vector containing the dataset for calculating the Z-scores.
#' The mean and standard deviation of this dataset are used to compute the Z-scores.
#'
#' @return A numeric vector of Z-scores corresponding to each value in the `value` vector.
#' The Z-score is calculated as: (value - mean(dataset)) / sd(dataset).
#'
#' @importFrom stats sd
#' @importFrom utils head
#'
#' @export
#'
#' @examples
#' value <- 2
#' spruce <- c(1, 2, 3, 4, 5, 6)
#' myplotty(value, spruce)
#'
myplotty <- function(value, dataset) {
  mean_value <- mean(dataset)
  sd_value <- sd(dataset)
  z <- (value - mean_value) / sd_value
  return(z)
}
