#' @title Another Greedy Solution for the Knapsack Problem
#' @param x a \code{data.frame} with two columns, "w" and "v", indicating the weight and value of items respectively
#' @param W the maximum weight that the knapsack can hold
#' @return a \code{list} containing two elements: \code{value} and \code{elements}, representing the maximum value that can be realised with the given knapsack and the indices of the items to be packed in the knapsack for realising maximum value
#' @export

another_greedy_knapsack <- function(x, W) {
  
  # check input integrity
  stopifnot(inherits(x, "data.frame"))
  stopifnot(W > 0)
  
  # initialise
  value_per_unit_weight = x$v/x$w
  item_seq <- order(value_per_unit_weight, decreasing = TRUE)
  value <- 0
  elements <- vector()
  n <- nrow(x)
  i <- 1
  item_id <- item_seq[1]
  item_weight <- x$w[item_seq[1]]
  weight_remaining <- W
  
  # run algo
  while(i <= n && item_weight <= weight_remaining) {
    
    value <- value + x$v[item_id]
    elements <- c(elements, item_id)
    weight_remaining <- weight_remaining - item_weight

    i <- i + 1
    item_id <- item_seq[i]
    item_weight <- x$w[item_id]
    
  }
  
  # return
  return(list(value = value, elements = elements))
}