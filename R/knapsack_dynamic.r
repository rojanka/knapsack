#' @title A Dynamic Programming Solution for the Knapsack Problem
#' @param x a \code{data.frame} with two columns, "w" and "v", indicating the weight and value of items respectively
#' @param W the maximum weight that the knapsack can hold
#' @return a \code{list} containing two elements: \code{value} and \code{elements}, representing the maximum value that can be realised with the given knapsack and the indices of the items to be packed in the knapsack for realising maximum value
#' @export

knapsack_dynamic <- function(x, W){
  stopifnot(inherits(x,"data.frame"))
  stopifnot(W>0)


  x$origIndex=1:nrow(x)
  combinations=matrix(data=FALSE,nrow=length(x$v),ncol=W)

  lastRow<-rep(list(),W)
  thisRow<-rep(list(),W)

  x=x[order(x$w),]

  for(i in 1:length(x$v)){
    if(i>1){
      lastRow=thisRow
      thisRow=rep(list(),W)
    }

    for(j in 1:W){
      if (i==1){ #first row (special case treated separately)
        if (j<x$w[1]){
          combinations[1,j]=0
          thisRow[[j]]=c()
        }else{
          combinations[1,j]=x$v[1]
          thisRow[[j]]=c(i)
        }
      }else{ #not the first row
        if (j<x$w[i]){
          combinations[i,j] = combinations[i-1,j]
          thisRow[[j]]=lastRow[[j]]
        }else{
          diff = j - x$w[i]
          if (diff==0){

            if (combinations[i-1,j]>x$v[i]){
              combinations[i,j]=combinations[i-1,j]
              thisRow[[j]]=lastRow[[j]]
            }else{
              combinations[i,j] = x$v[i]
              thisRow[[j]]=c(i)
            }
            #combinations[i,j] = max(combinations[i-1,j],x$v[i])
          }else{

            if (combinations[i-1,j]>(x$v[i]+combinations[i-1,diff])){
              combinations[i,j]=combinations[i-1,j]
              thisRow[[j]]=lastRow[[j]]
            }else{
              combinations[i,j] = x$v[i]+combinations[i-1,diff]
              thisRow[[j]]=c(i,lastRow[[diff]])
            }

            #combinations[i,j] = max(combinations[i-1,j],x$v[i]+combinations[i-1,diff])
          }
        }
      }
    }

  }

  highest=max(combinations)

  highest=combinations[length(x$v),W]
  highestComb=thisRow[[W]]

  returnList=list(value=highest,elements=x$origIndex[highestComb])
  return(returnList)

}
