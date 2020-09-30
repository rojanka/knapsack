knapsack_brute_force <- function(x, W){
  stopifnot((length(x$v)==length(x$w)))

  highestValue = 0

  for(i in 0:(2^(length(x$v))-1)){
    combination = which(as.logical(intToBits(i)))

    if (sum(x$w[combination])<=W){
      highestValue=max(c(sum(x$v[combination]), highestValue))
    }

  }

  return(highestValue)

}
