## selection {GA}
# Parents Selection for generating offspring

#' \code{selection} returns the parents population and the corresponding fitness values
#'
#' @param population a matrix
#' @param fitnessVec a vector of length n
#' @param selectMethod a function of selection method, can be gaLRselection-linear selection,
#' gaExpSelection-nonlinear selection, gaRWselection-Roulette Wheel selection,
#' gaTNselection-Tournament selection
#' @param c a constant specifies the exponential base in nonlinear ranking selection
#' @param k number of random selection from population in tournament selection
#'
#' @examples
#' numVar <- 6
#' N <- 50
#' population <- matrix(rbinom(numVar*N, 1, prob = 0.5), N, numVar)
#' fitnessVec <- seq(15, 50, length.out=50)
#' c <- 0.5
#' k <- 5
#' eliteRate <- 0.05
#' selection(population, fitnessVec, selectMethod=gaLRselection, eliteRate)
#' selection(population, fitnessVec, selectMethod=gaExpSelection, eliteRate, c)
#' selection(population, fitnessVec, selectMethod=gaRWselection, eliteRate)
#' selection(population, fitnessVec, selectMethod=gaTNselection, eliteRate, k)
#'
#' TBD


###################################### Selection Functions Wrapping ############################################
selection <- function(population, fitnessVec, eliteRate, selectMethod, c=NULL, k=NULL){
  if(missing(selectMethod)) { stop("A selection method must be provided") }
  if(!is.function(selectMethod)) { stop("Selection method is not a function") }
  if(!is.vector(fitnessVec)) { stop("Fitness values should be a vector") }
  if(!is.matrix(population)) { stop("Population should be a matrix") }
  if(missing(eliteRate)) { stop("A elite rate must be provided") }
  if(eliteRate < 0 | eliteRate > 1) { stop("The elite rate must be between 0 and 1") }
  if( (!is.null(c))&(!is.null(k)) ) { stop("Cannot use tournament and nonlinear rank selection at the same time") }

  ######### choose user-specified selection methods
  ## extract function name as string
  selectM <- as.character(substitute(selectMethod))
  if (selectM == "gaLRselection") {
    output <- gaLRselection(population, fitnessVec, eliteRate)
  } else if (selectM == "gaExpSelection") {
    if(is.null(c)) { stop("The exponential base for nonlinear rank selection must be provided") }
    if(c < 0 | c > 1) { stop("The exponential base c must be between 0 and 1") }

    output <- gaExpSelection(population, fitnessVec, eliteRate, c)
  } else if (selectM == "gaRWselection") {
    output <- gaRWselection(population, fitnessVec, eliteRate)
  } else if (selectM == "gaTNselection") {
    if(is.null(k)) { stop("Number of random selection must be provided for tournament selection") }
    if(!k%%1==0) { stop("Number of random selections must be an integer") }
    if (k>dim(population)[1]) { stop("Number of random selections cannot exceed population size") }
    output <- gaTNselection(population, fitnessVec, eliteRate, k)
  }
  return (output)
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








