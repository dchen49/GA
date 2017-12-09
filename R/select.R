## select {GA}
# Variable Selection via a Genetic Algorithm

#' Selects regression variables via a genetic algorithm
# '
#' @param x matrix of dimension n * p
#' @param y vector of length n or a matrix with n rows
#' @param model a list: first entry specifying either"lm" or "glm", and subsequent named entries specifying additional arguments into lm.fit() or glm.fit(): default "glm"
#' @param fitMetric "AIC", "BIC", or "RSS": default "AIC"
#' @param maxGen an integer specifying the maximum number of GA generations to use: default 100
#' @param minGen an integer specifying the number of generations without fitness improvement at which the GA algorithm will stop: default 5
#' @param gaMethod a list: The first entry must be one of ('TN', 'LR', 'ER','RW') with other entries specifying required additional arguments as needed. See gaMethod help for details.
#' @param pop an integer specifying the size of the genotype population. If odd, 1 random individual will be left out of crossing: default 100
#' @param pMutate a real value between 0 and 1 specifying the probability of an allele mutation: default .1
#' @param crossParams a numeric vector, c("cross probability", "max number of cross locations on a single gene"): default (.8, 1)
#' @param eliteRate Proportion of total population of elites who pass into the next generation unchanged
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

select <- function(x, y, model=list("glm"), fitMetric = "AIC", maxGen = 100L, minGen = 5L, gaMethod = list("LR"),  pop = 100L, pMutate = .05, crossParams = c(.8, 1L), eliteRate = .05, ...) {
  # clean & process inputs

  # define needed objects
  fitness <- vector(mode = "numeric", length = pop)
  # generate initial population
  population <- as.tibble(matrix(rbinom(pop*ncol(x), 1, .5), ncol = ncol(x), dimnames = list(1:pop, colnames(x))))
  # generate GA object: GA[generation][fitness, elites, tbd]
  GA <- rep_len(list(), length.out = maxGen)
  names(GA) <- sapply(1:maxGen, FUN = function(n) paste0('gen', n))

  # check x and y: type, same row dimension, no NAs,
  if (class(x)!="matrix" | !(typeof(x) %in% c("integer", "double")))
    stop("x must be a matrix of numerical values")
  if (!(class(y) %in% c("numeric", "matrix")))
    stop("y must be a matrix or vector of numerical values")
  if (!(nrow(x) %in% c(length(y), nrow(y))))
    stop("x and y must have the same number of rows")
  if (sum(is.na(x), is.na(y))!=0)
      stop("x and y must not have missing values.")

  # check and break apart "model" argument
  if (!is.list(model) | !sum(model %in% c('lm', 'glm')))
    stop("model must be a list specifying either 'lm' or 'glm' and subsequent optional entries specifying additional arguments for either lm.fit() or glm.fit()")
  else if (length(model) > 1) {
    modelParams <- model[-(model %in% c('lm', 'glm'))]
    model <- model[model %in% c('lm', 'glm')]
  }

  # check fitmetric
  if (!(fitMetric %in% c("AIC", "BIC")) && !is.function(fitMetric))
    stop("fitMetric must be 'AIC', 'BIC', or a function that takes a lm or glm object and outputs a single value that should be maximized")

  # check maxGen, minGen, and pop
  if (!is.integer(maxGen) | !is.integer(minGen) | median(c(1, minGen, maxGen))!=minGen)
    stop("minGen and maxGen must be positive integers with maxGen greater than minGen")
  if (!(is.integer(pop)) | pop < 1)
    stop("pop must be a positive integer")

  # check and break apart "gaMethod" into selectionFun and methodArgs
  method <- c('TN', 'LR', 'ER','RW')
  methodFuns <- c('gaTNselection', 'gaLRselection', 'gaExpSelection', 'gaRWselection')
  if (!is.list(gaMethod) | (c('TN', 'ER') %in% gaMethod && length(gaMethod)!=2) |
      (c('LR', 'RW') %in% gaMethod && length(gaMethod)!=1) | sum(gaMethod %in% method)!=1) {
    stop("gaMethod must be a list, specifying one of ('TN', 'LR', 'ER', 'RW') and for ER or TN selection, the additional required parameter.")
  }

  methodFun <- methodFuns[which(method %in% gaMethod)]

  if (methodFun=="gaTNselection") {
    if (!is.integer(gaMethod[[2]]) | gaMethod[[2]] > pop | length(gaMethod[[2]])!=1) {
      stop("gaMethod for 'TN' must additionally include an integer between 1 and the population size to specify the number of selection tournaments")
      } else methodArgs <- list("population" = population, "fitnessVec" = fitness, "eliteRate" = eliteRate, "k" = gaMethod[[2]])
    } else if (methodFun=="gaExpSelection") {
      if (!is.numeric(gaMethod[[2]]) | length(gaMethod[[2]])!=1) {
        stop("gaMethod for 'ER' must additionally include an number to specify the exponential base")
    } else methodArgs <- list(population, fitness, eliteRate, gaMethod[[2]])
  } else methodArgs <- list(population, fitness, eliteRate)

  # check pMutate and crossParams
  if (!(is.numeric(pMutate)) | median(c(0, pMutate, 1))!=pMutate)
    stop("pMutate must be a number between 0 and 1")
  if (!is.numeric(crossParams) | length(crossParams)!=2 | median(c(0, crossParams[1], 1))!=crossParams[1] |
      !(as.integer(crossParams[2])==crossParams[2]) | median(c(1, crossParams[2], pop))!=crossParams[2])
    stop("crossParams must be a numeric vector of length 2. The first term specifying a probability between 0 and 1 and the second a positive integer")

  # GA iterations
  gen <- 1
  while(gen < maxGen) { # fix inputs
    fitness <- apply(population, 1, regress, x = x, y = y, model = model, fitnessCriteria = fitMetric, ...)

    # Identify unique elite genotypes
    ordFit <- order(fitness, decreasing = TRUE)
    elites <- population[head(ordFit, max(0, floor(length(fitness)*eliteRate))), ]

    GA[[gen]] <- list("fitness" = fitness, "elites" = cbind(fitness[head(ordFit, max(0, floor(length(fitness)*eliteRate)))], elites), "fitMax" = max(fitness), "tbd" = "tbd")

    # check stopping criteria
    Stop = FALSE
    if (gen > minGen) {
      fitHistory <- sapply(gen-minGen:gen, FUN = function(i) {
        max(GA[[i]]$fitMax)
      })
      Stop <- sum(sum(outer(fitHistory, fitHistory, "-"))) <= .Machine$double.eps
    }
    if (Stop == TRUE) break

    # population selection
    population <- gaSelection(methodFun, methodArgs)[[1]]

    population <- evolve(population, pMutate, crossParams[1], crossParams[2])

    population <- rbind(elites, population)
    gen = gen + 1
  }
  print(GA) # identify final optimal candidates
  return(GA)
}
