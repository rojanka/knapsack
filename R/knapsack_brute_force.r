knapsack_brute_force <- function(x, W){
  stopifnot(inherits(x,"data.frame"))
  stopifnot(W>0)

  highestValue = 0
  highestValueComb = c()

  for(i in 0:(2^(length(x$v))-1)){
    combination = which(as.logical(intToBits(i)))

    if ((sum(x$w[combination])<=W) & ((sum(x$v[combination])>highestValue))){
      highestValue=sum(x$v[combination])
      highestValueComb = combination
    }

  }
  returnList=list(value=highestValue,elements=highestValueComb)

  return(returnList)

}
