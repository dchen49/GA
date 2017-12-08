## selection {GA}
# Survivor Selection after Evolution

#' Survivor Selection
# '
#' @param offspringPop a matrix
#' @param offFitness a vector of length n
#' @param c a constant specifies the exponential base in nonlinear ranking selection
#' @param k number of random selection from population in tournament selection
#' @param Pop.switch a matrix, where the first column is fitness, all other columns are genotypes
#' @param replace.rate a constant, proportion of offsprings to be replaced
#'
#' @examples
#' offspringPop <- matrix(rbinom(6*100, 1, prob = 0.5), 100, 6)
#' offFitness <- seq(15, 100, length.out=100)
#' replace.rate <- 0.1
#' Pop.switch <- cbind(seq(2, 50, length.out=10), matrix(rbinom(6*10, 1, prob = 0.5), 10, 6))
#' Newpopulation(offspringPop, offFitness, Pop.switch, replace.rate)
#'
#' TBD



############################################## Survivor Selection ##############################################
Newpopulation <- function(offspringPop, offFitness, Pop.switch, replace.rate){
  if(!is.matrix(offspringPop)) { stop("Population should be a matrix") }
  if(!is.vector(offFitness)) { stop("Fitness values should be a vector") }
  if(!is.matrix(Pop.switch)) { stop("Original Population should be a matrix") }
  if((dim(Pop.switch)[2]-dim(offspringPop)[2])!=1) { stop("Either dimension of offspring population or Pop.switch is incorrect") }
  if(replace.rate < 0 | replace.rate > 1) { stop("The replace.rate must be between 0 and 1") }

  replacePop <- Pop.switch[,-1]
  replaceFit <- Pop.switch[,1] ## fitness
  N <- dim(offspringPop)[1] ## population size
  Num.replace <- floor(N*replace.rate) ## number of individuals to be replaced
  ordInd <- tail(order(offFitness, decreasing = TRUE), Num.replace)
  offspringPop[ordInd,] <- replacePop
  offFitness[ordInd] <- replaceFit
  return (list(population=offspringPop))
}
