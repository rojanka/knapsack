greedy_knapsack <-
function(x, W){
  stopifnot((length(x$v)==length(x$w)))

  valueRatio = x$v/x$w

  x=x[order(valueRatio, decreasing=TRUE),]

  vikt1 = 0
  vikt = 0

  varde1 = 0
  varde = 0

  i=1
  while(vikt1<=W){
    vikt=vikt1
    varde=varde1

    vikt1=vikt1+x$w[i]
    varde1=varde1+x$v[i]
    i=i+1
  }


  return(varde)

}
