knapsack_another_dynamic <- function(x, W) {
  
  # check input integrity
  stopifnot(inherits(x, "data.frame"))
  stopifnot(W > 0)
  stopifnot(W %% 1 == 0) # W needs to be an integer
  
  # set up dp matrices and boolean vector for pick/no-pick decisions
  n <- nrow(x)
  prev_value <- vector(mode = "numeric", length = W + 1)
  curr_value <- vector(mode = "numeric", length = W + 1)
  prev_combo_matrix <- matrix(data = FALSE, nrow = n, ncol = W + 1)
  curr_combo_matrix <- matrix(data = FALSE, nrow = n, ncol = W + 1)
  
  # run algo
  
  for (i in 1:n) {
    
    for (j in 1:W) {
      
      if (x$w[i] <= j) { # i-th item can only be added to knapsack if it's weight <= j
        
        base <- prev_value[j + 1] # knapsack value if i-th item is not added
        alternative <- prev_value[j + 1 - x$w[i]] + x$v[i] # knapsack value if i-th item is added
        
        if (alternative > base) { # knapsack has higher value if i-th item is added
          
          curr_value[j + 1] <- alternative # update knapsack value
          curr_combo_matrix[, j + 1] <- prev_combo_matrix[, j + 1 - x$w[i]]
          curr_combo_matrix[i, j + 1] <- TRUE # indicate that i-th item has been added
          
        }
        
      }
      
    }
    
    prev_value <- curr_value
    prev_combo_matrix <- curr_combo_matrix
    
  }
  
  return(list(value = curr_value[W + 1],
              elements = which(curr_combo_matrix[, W+ 1])))
  
}