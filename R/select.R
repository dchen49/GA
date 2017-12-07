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


select <- function(x, y, model="glm", fitnessCriteria = "AIC", pop0 = 100, mutation = .1, crossing = c(.5, 1), maxGen = 100, minGen = 5, selectionMethod = "gaTNselection", ...) {
  # clean & process inputs
  # named columns for x, y and x same # of rows, x and y are vectors/matrices, check for NA
  # vector arguments get broken apart
  x <- as.tibble(read.table(file = "data/baseball.dat", header = TRUE))[, -1]
  y <- as.tibble(read.table(file = "data/baseball.dat", header = TRUE))[, 1]
  etcetera <- match.arg

  # break apart "model" argument
  if (length(model) > 1) {
    modelParams <- model[-1]
    model <- modelParams[1]
    modelParams <- paste(modelParams, sep = ",")
  }

  # break apart SelectionMethod

  # generate initial population
  population <- as.tibble(matrix(rbinom(pop0*ncol(x), 1, .5), ncol = ncol(x), dimnames = list(1:pop0, colnames(x))))

  # generate GA object: GA[generation][fitness, elites, tbd]
  GA <- rep_len(list(), length.out = maxGen)
  names(GA) <- sapply(1:maxGen, FUN = function(n) paste0('gen', n))

  # GA iterations
  gen <- 1
  while(gen < maxGen) { # fix inputs
    fitness <- apply(population, 1, regress, x = x, y = y, model = model, fitnessCriteria = fitnessCriteria, ...)

    # Identify unique elite genotypes
    fittest <- which(fitness %in% unique(fitness)[order(unique(fitness), decreasing = TRUE)[1:min(10, floor(length(fitness/20)))]])
    elites <- unique(cbind("fitness" = fitness[fittest], population[fittest, ]))

    GA[[counter]] <- list("fitness" = fitness, "elites" = elites[order(elites$fitness, decreasing = TRUE), ], "fitMax" = max(fitness), "tbd" = "tbd")

    # check stopping criteria
    if (gen > minGen) {
      fitHistory <- sapply(gen-minGen:gen, FUN = function(i) {
        max(GA[[i]]$fitMax)
      })
      Stop <- sum(sum(outer(fitHistory, fitHistory, "-"))) <= .Machine$double.eps
    }

    if (Stop == TRUE) break

    # population selection
    selectionMethods <- c()

    population <-

    population <- evolve(population, mutation.prob=0.1, crossing.prob=0.5, num.cross.locations=1)
    gen = gen + 1
  }

  return(GA)
}
