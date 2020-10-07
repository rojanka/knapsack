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