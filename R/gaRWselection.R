## gaSelectionMethod {GA}
# Roulette Wheel Selection

#' In Roulette Wheel Selection method, the chromosomes are selected based on their probabilities
#' that are proportional to their fitness value. whole population in partitioned on the wheel and
#' each sector represents an individual. The proportion of individual’s fitness to the total fitness
#' values of whole population decides the probability of selection of that individual in the next generation.
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
#' gaRWselectio(population, fitnessVec, eliteRate)
#'



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
