## gaSelectionMethod {GA}
# Parent Selection

#' Parent Selection
# '
#' @param population a matrix
#' @param fitnessVec a vector of length n or a matrix with n rows
#'
#' @return returns the selected parents
#' @param population a matrix
#'
#' @examples
#'
#' TBD


################################################## Selection #################################################

gaSelection <- function(methodFun, methodArgs) {
  return(do.call(methodFun, args = methodArgs))
}


################################################## Selection #################################################
##### Selection-select potential parents from initial population
## Linear Rank Selection
## For a population with size N, the best solution, the one with highest fitness has rank N,
## the second best rank N-1, and the worst rank 1, etc
gaLRselection <- function(population, fitnessVec, eliteRate){
  N <- dim(population)[1]
  n <- dim(population)[1] - floor(dim(population)[1]*eliteRate)
  ## fitnessVec is a vector of all the fitness values for current generation
  rank <- rank(fitnessVec, ties.method = "min")[1:N] ## return corresponding rank for each fitness value
  denom <- N*(N+1)/2
  prob <- rank/denom
  sel <- sample(1:N, size = n, prob = prob, replace = TRUE)
  output <- list(population = population[sel,,drop=FALSE],
                 fitness = fitnessVec[sel])
  ## selected chromosomes with size N and corresponding fitness value
  return (output)
}

## Nonlinear Rank Selection (Exponential Rank Selection)
## almost the same as Linear Rank Selection, except the definition of probability
## base: exponential base, in (0,1)
## c is the base
gaExpSelection <- function(population, fitnessVec, eliteRate, c){
  N <- dim(population)[1]
  n <- dim(population)[1] - floor(dim(population)[1]*eliteRate)
  ## fitnessVec is a vector of all the fitness values for current generation
  rank <- rank(fitnessVec, ties.method = "min")
  prob <- c^(N-rank) / sum(c^(N-rank))
  sel <- sample(1:N, size = n, prob = prob, replace = TRUE)
  output <- list(population = population[sel,,drop=FALSE],
                 fitness = fitnessVec[sel])
  return (output)
}

## Roulette Wheel Selection
gaRWselection <- function(population, fitnessVec, eliteRate){
  N <- dim(population)[1]
  n <- dim(population)[1] - floor(dim(population)[1]*eliteRate)
  prob <- abs(fitnessVec) / sum(abs(fitnessVec))
  sel <- sample(1:N, size = n, prob = prob, replace = TRUE)
  output <- list(population = population[sel,,drop=FALSE],
                 fitness = fitnessVec[sel])
  return (output)
}

## Tournament Selection
## k is the number of random selection from population
gaTNselection <- function(population, fitnessVec, eliteRate, k){
  N <- dim(population)[1] - floor(dim(population)[1]*eliteRate)
  selection <- rep(0,N)
  for (i in 1:N){
    s <- sample(1:N, size=k)
    selection[i] <- s[which.max(fitnessVec[s])]
  }
  ## selection is a vector of selected rows
  output <- list(population = population[selection,,drop=FALSE],
                 fitness = fitnessVec[selection])
  return (output)
}
