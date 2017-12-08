## select {GA}
# Variable Selection via a Genetic Algorithm

#' Selects regression variables via a genetic algorithm
# '
#' @param x matrix of dimension n * p
#' @param y vector of length n or a matrix with n rows
#' @param model a list: first entry specifying either"lm" or "glm", and subsequent optional entries specifying additional arguments into lm.fit() or glm.fit(): default "glm"
#' @param fitMetric "AIC", "BIC", or "RSS": default "AIC"
#' @param maxGen an integer specifying the maximum number of GA generations to use: default 100
#' @param minGen an integer specifying the number of generations without fitness improvement at which the GA algorithm will stop: default 5
#' @param gaMethod a list: The first entry must be one of ('tournament', 'linearRank', 'expRank','roulette') with other entries specifying required additional arguments as needed. See gaMethod help for details.
#' @param pop an integer specifying the size of the genotype population. If odd, 1 random individual will be left out of crossing: default 100
#' @param pMutate a real value between 0 and 1 specifying the probability of an allele mutation: default .1
#' @param crossing a numeric vector, c("cross probability", "max number of cross locations on a single gene"): default (.8, 1)
#' @param a A placeholder.
#' @param a A placeholder.
#' @param a A placeholder.
#' @param a A placeholder.
#' @param a A placeholder.
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


select <- function(x, y, model="glm", fitMetric = "AIC", pop = 100, pMutate = .05, crossing = c(.8, 1), maxGen = 100, minGen = 5, gaMethod = "gaTNselection", ...) {
  # clean & process inputs
  # named columns for x, y and x same # of rows, x and y are vectors/matrices, check for NA
  # vector arguments get broken apart
  x <- as.tibble(read.table(file = "data/baseball.dat", header = TRUE))[, -1]
  y <- as.tibble(read.table(file = "data/baseball.dat", header = TRUE))[, 1]
  etcetera <- match.arg

  # check and break apart "model" argument
  if (!is.list(model) | !sum(model %in% c('lm', 'glm')))
    stop("model must be a list specifying either 'lm' or 'glm' and subsequent optional entries specifying additional arguments for either lm.fit() or glm.fit()")
  else if (length(model) > 1) {
    modelParams <- model[-(model %in% c('lm', 'glm'))]
    model <- model[model %in% c('lm', 'glm')]
  }

  # check and break apart "gaMethod"
  if (!is.list(gaMethod) | !sum(gaMethod %in% method))
    stop("gaMethod must be a list, specifying one of ('tournament', 'linearRank', 'expRank','roulette') and an additional required parameter for exponential ranking and tournament selection.")
  method <- c('tournament', 'linearRank', 'expRank','roulette')
  methodFun <- c('gaTNselection', 'gaLRselection', 'gaExpSelection', 'gaRWselection')
  gaMethodArgs <- fitness

  if (length(gaMethod) > 1) {
    gaMethodArgs <- gaMethod[-(gaMethod %in% method)]
  }

  # generate initial population
  population <- as.tibble(matrix(rbinom(pop*ncol(x), 1, .5), ncol = ncol(x), dimnames = list(1:pop, colnames(x))))

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
