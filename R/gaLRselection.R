## gaSelectionMethod {GA}
# Linear Ranking Selection

#' In Linear Ranking selection method, individuals are first sorted according to their fitness value
#' and then the ranks are assigned to them. Best individual gets rank ‘N’ and the worst one gets rank ‘1’.
#' The selection probability is then assigned linearly to the individuals according to their ranks.
#'
#' @param population, a matrix of size n*p
#' @param fitnessVec, a vector with length n
#' @param eliteRate, a proportion between zero and one
#'
#' @examples
#' numVar <- 6
#' N <- 50
#' population <- matrix(rbinom(numVar*N, 1, prob = 0.5), N, numVar)
#' fitnessVec <- seq(15, 50, length.out=50)
#' eliteRate <- 0.05
#' gaLRselection(population, fitnessVec, eliteRate)
#'


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
