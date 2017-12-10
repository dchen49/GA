## gaSelectionMethod {GA}
# Nonlinear Ranking Selection

#' In nonlinear Ranking selection method, individuals are first sorted according to their fitness value
#' and then the ranks are assigned to them. Best individual gets rank ‘N’ and the worst one gets rank ‘1’.
#' The selection probabilities here are exponentially weighted. The base of the exponent is c.
#'
#' @param population, a matrix of size n*p
#' @param fitnessVec, a vector with length n
#' @param eliteRate, a proportion between zero and one
#' @param c, exponential base
#'
#' @examples
#' numVar <- 6
#' N <- 50
#' population <- matrix(rbinom(numVar*N, 1, prob = 0.5), N, numVar)
#' fitnessVec <- seq(15, 50, length.out=50)
#' eliteRate <- 0.05
#' c <- 0.5
#' gaExpSelection(population, fitnessVec, eliteRate, c)
#'





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
