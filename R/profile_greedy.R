#' @import profvis

set.seed(42)
n <- 1000000
x <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

W <- 2000

profvis::profvis({greedy_knapsack(x, W)})
profvis::profvis({another_greedy_knapsack(x, W)})