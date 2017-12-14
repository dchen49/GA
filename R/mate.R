## mate {GA}
# Parent Selection

#' Returns the parents population and the corresponding fitness values
#'
#' @param methodFun a function of selection method, can be gaLRselection-linear selection,
#' gaExpSelection-nonlinear selection, gaRWselection-Roulette Wheel selection,
#' gaTNselection-Tournament selection
#' @param methodArgs a list with elements as arguments of selection method function, where first element
#' is population, second as fitness value, third as  elite rate. If the chosen selection method is exponential
#' ranking selection, then one more argument as exponential base must be provided; if the chosen selection
#' method is tournament selection, then one more argument as number of random selections must be provided
#'
#' @examples
#' numVar <- 6
#' N <- 50
#' population <- matrix(rbinom(numVar*N, 1, prob = 0.5), N, numVar)
#' fitnessVec <- seq(15, 50, length.out=50)
#' c <- 0.5
#' k <- 5
#' eliteRate <- 0.05
#' mate(gaLRselection, list(population, fitnessVec, eliteRate))
#' mate(gaExpSelection, list(population, fitnessVec, eliteRate, c))
#' mate(gaRWselection, list(population, fitnessVec, eliteRate))
#' mate(gaTNselection, list(population, fitnessVec, eliteRate, k))


################################################## Selection #################################################

mate <- function(methodFun, methodArgs) {
  if(missing(methodFun)) { stop("A selection method must be provided") }
  if(class(methodFun)!="character") {stop("Selection method must be one of 'gaLRselection', 'gaExpSelection', 'gaRWselection', or 'gaTNselection'")}
  if(!is.list(methodArgs)) { stop("Method arguments should be inside a list")  }
  if(!is.vector(methodArgs[[2]])) { stop("Fitness values should be a vector") }
  if(!is.matrix(methodArgs[[1]])) { stop("Population should be a matrix") }
  if(length(methodArgs)==2) { stop("A elite rate must be provided") }
  if(methodArgs[[3]] < 0 | methodArgs[[3]] > 1) { stop("The elite rate must be between 0 and 1") }
  if(methodFun  == "gaExpSelection") { if(length(methodArgs)==3) { stop("The exponential base for nonlinear rank selection must be provided") } }
  if(methodFun == "gaExpSelection") { if(methodArgs[[4]]< 0 | methodArgs[[4]] > 1) { stop("The exponential base c must be between 0 and 1") } }
  if(methodFun == "gaTNselection") { if(length(methodArgs)==3) { stop("Number of random selection must be provided for tournament selection") } else if (!methodArgs[[4]]%%1==0) {
    stop("Number of random selections must be an integer")
  } else if (methodArgs[[4]]>(dim(methodArgs[[1]])[1] - floor(dim(methodArgs[[1]])[1]*methodArgs[[3]]))) {
    stop("Number of random selections cannot exceed shrinked population size")
  } }
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
