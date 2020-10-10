#' @import foreach
#' @import doParallel
#' @import parallel
#' @title Brute-Force Solution for the Knapsack Problem
#' @param x a \code{data.frame} with two columns, "w" and "v", indicating the weight and value of items respectively
#' @param W the maximum weight that the knapsack can hold
#' @param parallel TRUE if a parallelised version of the solution is to be called; FALSE by default
#' @return a \code{list} containing two elements: \code{value} and \code{elements}, representing the maximum value that can be realised with the given knapsack and the indices of the items to be packed in the knapsack for realising maximum value
#' @export

another_knapsack_brute_force <- function(x, W, parallel=FALSE){
  stopifnot(inherits(x,"data.frame"))
  stopifnot(W>0)
  
  if (nrow(x) > 31) stop("This implementation cannot handle knapsack
      problem involving more than 31 items")
  
  if (parallel==TRUE){
    
    get_combo_value <- function(i) {
      
      combination = which(as.logical(intToBits(i)))
      
      if (sum(x$w[combination])<=W) {
        
        return(sum(x$v[combination]))
        
      } else {
        
        return(0)
        
      }
      
    }
    
    cl <- parallel::makeCluster(parallel::detectCores())
    parallel::clusterExport(cl, c("x", "W", "get_combo_value"), envir = environment())
    all_combos_values <- parallel::parSapply(cl, 0:(2^(length(x$v))-1), get_combo_value)
    parallel::stopCluster(cl)
    
    #closeAllConnections()
    #gc()
    
    highestValue <- max(all_combos_values)
    highestValueComb <- which(as.logical(intToBits(which(all_combos_values == highestValue)[1] - 1)))

  }else{
    highestValue = 0
    highestValueComb = c()
    
    for(i in 0:(2^(length(x$v))-1)){
      combination = which(as.logical(intToBits(i)))
      
      if ((sum(x$w[combination])<=W) & ((sum(x$v[combination])>highestValue))){
        highestValue=sum(x$v[combination])
        highestValueComb = combination
      }
      
    }
    
  }
  
  return(list(value=highestValue,elements=highestValueComb))
  
}
