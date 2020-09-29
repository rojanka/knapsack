knapsack_dynamic <- function(x, W){
  stopifnot((length(x$v)==length(x$w)))



  combinations=matrix(data=FALSE,nrow=length(x$v),ncol=W)

  x=x[order(x$w),]

  for(i in 1:length(x$v)){
    for(j in 1:W){
      if (i==1){ #first row (special case treated separately)
        if (j<x$w[1]){
          combinations[1,j]=0
        }else{
          combinations[1,j]=x$v[1]
        }
      }else{ #not the first row
        if (j<x$w[i]){
          combinations[i,j] = combinations[i-1,j]
        }else{
          diff = j - x$w[i]
          if (diff==0){
            combinations[i,j] = max(combinations[i-1,j],x$v[i])
          }else{
            combinations[i,j] = max(combinations[i-1,j],x$v[i]+combinations[i-1,diff])
          }
        }
      }
    }

  }

  highest=max(combinations)

  return(highest)

}
