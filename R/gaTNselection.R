## gaSelectionMethod {GA}
# Tournament Selection

#' In Tournament selection method, ‘n’ individuals are chosen at random from the entire population. These
#' individuals compete against each other. The individual with the highest fitness value wins and gets
#' selected for further processing of Genetic Algorithm.
#'
#' @param population, a matrix of size n*p
#' @param fitnessVec, a vector with length n
#' @param eliteRate, a proportion between zero and one
#' @param k, number of random selections
#'
#' @examples
#' numVar <- 6
#' N <- 50
#' population <- matrix(rbinom(numVar*N, 1, prob = 0.5), N, numVar)
#' fitnessVec <- seq(15, 50, length.out=50)
#' eliteRate <- 0.05
#' k <- 5
#' gaTNselection(population, fitnessVec, eliteRate, k)
#'




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
