knapsack_brute_force <-
function(x, W){
  stopifnot((length(x$v)==length(x$w)))

  combinations=matrix(data=FALSE,nrow=length(x$v),ncol=2)
  combinations[1,2]=TRUE

  for(i in 1:(length(x$v)-1)){
    combinations2 = combinations

    combinations2[i,] = TRUE

    combinations = cbind(combinations,combinations2)
  }

  values=apply(combinations,2,function(y){ sum(x$v[y]) })
  weights=apply(combinations,2,function(y){ sum(x$w[y]) })

  return(max(values[weights<=W]))

}
