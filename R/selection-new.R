## select {GA}
# Variable Selection via a Genetic Algorithm

#' Selects regression variables via a genetic algorithm
# '
#' @param x matrix of dimension n * p
#' @param y vector of length n or a matrix with n rows
#' @param model vector specifying the model: the first argument should be "lm" or "glm", and subsequent arguments specify additional arguments into "lm" or "glm"
#' @param fitnessCriteria default "AIC", a string specifying the fitness criterion: "AIC", "BIC", or "TBD"
#' @param pop0 an integer specifying the initial population size, should be even otherwise 1 will be left out in cross
#' @param crossing a numeric vector, c("cross probability", "max number of cross locations on a single gene")
#' @param maxGen an integer specifying the maximum number of GA generations to use
#' @param minGen an integer specifying the number of generations without fitness improvement at which the GA algorithm will stop.
#' @param selectionMethod vectors specifying the genetic selection method and appropriate necessary arguments. See selectMethod help for details.
#' @param a A placeholder.
#' @param ... optional arguments to lm, glm, ect.
#'
#' @return returns an object of class "GA", which is a list containing the following components:
#' @param coefficients a named vector of coefficients
#' @param fitness the maximum value attained of the specified fitness criterion
#' @param generations the number of GA generations
#'
#' @examples
#'
#' TBD






################################################## Selection #################################################
##### Selection-select potential parents from initial population
## Linear Rank Selection
## For a population with size N, the best solution, the one with highest fitness has rank N, 
## the second best rank N-1, and the worst rank 1, etc
gaLRselection <- function(population, fitnessVec){
  N <- dim(population)[1]
  ## fitnessVec is a vector of all the fitness values for current generation
  rank <- rank(fitnessVec, ties.method = "min") ## return corresponding rank for each fitness value
  denom <- N*(N+1)/2
  prob <- rank/denom
  sel <- sample(1:N, size = N, prob = prob, replace = TRUE)
  output <- list(population = population[sel,,drop=FALSE],
                 fitness = fitnessVec[sel])
  ## selected chromosomes with size N and corresponding fitness value
  return (output)
}

## Nonlinear Rank Selection (Exponential Rank Selection)
## almost the same as Linear Rank Selection, except the definition of probability
## base: exponential base, in (0,1)
## c is the base
gaExpSelection <- function(population, fitnessVec, c){
  N <- dim(population)[1]
  ## fitnessVec is a vector of all the fitness values for current generation
  rank <- rank(fitnessVec, ties.method = "min")
  prob <- c^(N-rank) / sum(c^(N-rank))
  sel <- sample(1:N, size = N, prob = prob, replace = TRUE)
  output <- list(population = population[sel,,drop=FALSE],
                 fitness = fitnessVec[sel])
  return (output)
}

## Roulette Wheel Selection
gaRWselection <- function(population, fitnessVec){
  N <- dim(population)[1]
  prob <- abs(fitnessVec) / sum(abs(fitnessVec))
  sel <- sample(1:N, size = N, prob = prob, replace = TRUE)
  output <- list(population = population[sel,,drop=FALSE],
                 fitness = fitnessVec[sel])
  return (output)
}

## Tournament Selection
## k is the number of random selection from population
gaTNselection <- function(population, fitnessVec, k){
  selection <- rep(0,N)
  N <- dim(population)[1]
  for (i in 1:N){
    s <- sample(1:N, size=k)
    selection[i] <- s[which.max(fitnessVec[s])]
  }
  ## selection is a vector of selected rows
  output <- list(population = population[selection,,drop=FALSE],
                 fitness = fitnessVec[selection])
  return (output)
}

############################################## Survivor Selection ##############################################
Newpopulation <- function(offspringPop, offFitness, Pop.switch, replace.rate){
  replacePop <- Pop.switch[,-1]
  replaceFit <- Pop.switch[,1] ## fitness
  N <- dim(offspringPop)[1] ## population size
  Num.replace <- floor(N*replace.rate) ## number of individuals to be replaced
  ordInd <- tail(order(offFitness, decreasing = TRUE), Num.replace)
  offspringPop[ordInd,] <- replacePop
  offFitness[ordInd] <- replaceFit
  return (list(population=offspringPop))
}











