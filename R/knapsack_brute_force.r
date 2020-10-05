#library(foreach)
#library(doParallel)

knapsack_brute_force <- function(x, W, parallel=FALSE){
  stopifnot(inherits(x,"data.frame"))
  stopifnot(W>0)



  if (parallel==TRUE){
    registerDoParallel(detectCores())

    combListsMax=function(a,b){
      if (a[[1]]>b[[1]]){
        return(a)
      }else{
        return(b)
      }
    }

    returnList=foreach (i=0:(2^(length(x$v)) - 1), .combine=combListsMax) %dopar% {
      if (sum(x$w[which(as.logical(intToBits(i)))])<=W){
        return(list(value=sum(x$v[which(as.logical(intToBits(i)))]), elements=which(as.logical(intToBits(i)))))
      }else{
        return(list(value=0,elements=c()))
      }
    }

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
    returnList=list(value=highestValue,elements=highestValueComb)

  }






  return(returnList)

}
