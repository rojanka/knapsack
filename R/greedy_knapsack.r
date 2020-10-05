greedy_knapsack <- function(x, W){
  stopifnot(inherits(x,"data.frame"))
  stopifnot(W>0)

  valueRatio = x$v/x$w
  x$origIndex = 1:nrow(x)

  x=x[order(valueRatio, decreasing=TRUE),]

  combination1=c()
  combination=c()

  vikt1 = 0
  vikt = 0

  varde1 = 0
  varde = 0

  i=1
  while(vikt1<=W){
    vikt=vikt1
    varde=varde1
    combination=combination1

    vikt1=vikt1+x$w[i]
    varde1=varde1+x$v[i]
    combination1=c(combination1,i)
    i=i+1
  }

  returnList=list(value=varde,elements=x$origIndex[combination])


  return(returnList)

}
