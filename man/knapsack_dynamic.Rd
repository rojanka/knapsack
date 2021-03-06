% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/knapsack_dynamic.R
\name{knapsack_dynamic}
\alias{knapsack_dynamic}
\title{A Dynamic Programming Solution for the Knapsack Problem}
\usage{
knapsack_dynamic(x, W)
}
\arguments{
\item{x}{a \code{data.frame} with two columns, "w" and "v", indicating the weight and value of items respectively}

\item{W}{the maximum weight that the knapsack can hold}
}
\value{
a \code{list} containing two elements: \code{value} and \code{elements}, representing the maximum value that can be realised with the given knapsack and the indices of the items to be packed in the knapsack for realising maximum value
}
\description{
A Dynamic Programming Solution for the Knapsack Problem
}
