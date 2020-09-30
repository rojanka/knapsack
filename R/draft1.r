#knapsack

set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

x = knapsack_objects[1:8,]



#bruteforce
knapsack_brute_force <- function(x, W){
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

knapsack_int <- function(x, W){
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


a = Sys.time()
knapsack_brute_force(x = knapsack_objects[1:18,], W = 3500)
print(Sys.time()-a)

a = Sys.time()
knapsack_int(x = knapsack_objects[1:18,], W = 3500)
print(Sys.time()-a)



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

a = Sys.time()
knapsack_dynamic(x = knapsack_objects[1:500,], W = 3500)
print(Sys.time()-a)



greedy_knapsack <- function(x, W){
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


greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)

set.seed(42)
n <- 1000000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

a = Sys.time()
greedy_knapsack(x = knapsack_objects, W = 3500000)
print(Sys.time()-a)
