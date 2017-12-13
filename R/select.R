## select {GA}
# Variable Selection via a Genetic Algorithm

#' Selects Regression Variables using a Genetic Algorithm
#'
#' @param x matrix of dimension n * p
#' @param y vector of length n or a matrix with n rows
#' @param model list - default "lm" : one of ("lm", "glm") and an optional character string specifying arguments into lm.fit() or glm.fit()
#' @param fitMetric default "AIC": one of ("AIC", "BIC", "RSS") or a function that takes a regression object and outputs a single number to be maximized
#' @param maxGen default 200: integer specifying the maximum number of GA generations to use
#' @param minGen default 50: integer specifying the number of generations without fitness improvement at which the GA algorithm will stop
#' @param gaMethod list - default 'LR': one of ('TN', 'LR', 'ER','RW') and an additional numrical argument as needed. See gaSelection for details.
#' @param pop default 100: integer specifying the size of the genotype pool.
#' @param pMutate default .1: real number between 0 and 1 specifying the probability of an allele mutation
#' @param crossParams numeric - default (.8, 1): c("cross probability", "max number of cross locations on a single gene")
#' @param eliteRate default .1: Proportion of highest fitness genotypes that pass into the next generation unchanged.
#'
#' @return returns a list of 4 components: optimum, fitPlot, fitStats, and GA
#' \itemize{
#' \item{\strong{optimum:} a list of properties of the genotype acheiving max fitness}{
#' \itemize{
#' \item \emph{\strong{variables:}} the recommended set of regression variables
#' \item \emph{\strong{fitness:}} the achieved fitness metric
#' \item \emph{\strong{fitModel:}} the regression object returned by using the recommended variables
#' }}
#' \item{\strong{fitPlot:} a plot of the mean, median, and maximum fitness over the generations}
#' \item{\strong{fitStats:} a tibble of the values used to generate the plot}
#' \item{\strong{GA:} a list of data associated with each generation of the genetic algorithm}{
#' \itemize{
#' \item \emph{\strong{fitness:}} the fitness measures of the current generation
#' \item \emph{\strong{elites:}} the fitness values and genotypes with the highest fitness
#' \item \emph{\strong{fitMax:}} the highest fitness value
#' }}
#' }
#'
#' @examples tbd
#' @seealso \code{\link{regress}}
#' \code{\link{gaSelection}}
#' \code{\link{evolve}}
#' @export

select <- function(x, y, model=list("lm"), fitMetric = "AIC", maxGen = 200L, minGen = 50L, gaMethod = list("TN", 5),  pop = 100L, pMutate = .1, crossParams = c(.8, 1L), eliteRate = 0.1, ...) {


  ######################################## DEFINE NECESSARY OBJECTS ########################################\


  fitness <- vector(mode = "numeric", length = pop)

  population <- matrix(rbinom(pop*ncol(x), 1, .5), ncol = ncol(x), dimnames = list(1:pop, colnames(x)))

  # generate GA object: GA[[generation]][fitness, elites, fitMax]
  GA <- rep_len(list(), length.out = maxGen)
  names(GA) <- sapply(1:maxGen, FUN = function(n) paste0('gen', n))


  ######################################## CHECK AND FORMAT ARGUMENTS ########################################


  # x and y: type, same row dimension, no NAs, etc.
  if (!is.matrix(x) | !(typeof(x) %in% c("integer", "double")))
    stop("x must be a matrix of numerical values")
  if (!(class(y) %in% c("numeric", "matrix", "integer")))
    stop("y must be a matrix or vector of numerical values")
  if (!(nrow(x) %in% c(length(y), nrow(y))))
    stop("x and y must have the same number of rows")
  if (sum(is.na(x), is.na(y))!=0)
      stop("x and y must not have missing values.")
  if (dim(x)[2] > dim(x)[1])
    warning("Number of dimensions is large compared to the sample size and may adversely affect model fitting")

  # model: list specifying "lm" or "glm" and string specifying additional arguments -> model & modelParams
  modelParams <- NULL
  if (!is.list(model) | !sum(model %in% c('lm', 'glm')) | !(length(model) %in% c(1,2)) ) {
    stop("model must be a list including either 'lm' or 'glm' and optionally a string specifying additional arguments for the specified .fit function")
    } else if (length(model) > 1) {
      if (!is.character(model[[2]]) ) {
        stop("additional argument must be a string specifying additional arguments for the specified lm.fit or glm.fit function")
      } else {
        modelParams <- as.character(model[-(model %in% c('lm', 'glm'))])
        model <- model[model %in% c('lm', 'glm')]
      }
    }

  # fitMetric: "AIC" or "BIC", or a function taking in an lm/glm object and outputting a singel number to be maximized
  if (!(fitMetric %in% c("AIC", "BIC")) && !is.function(fitMetric))
    stop("fitMetric must be 'AIC', 'BIC', or a function that takes a lm or glm object and outputs a single value that should be maximized")

  # maxGen, minGen, and pop: positive integers, maxGen > minGen
  if (!is.integer(maxGen) | !is.integer(minGen) | median(c(1, minGen, maxGen))!=minGen)
    stop("minGen and maxGen must be positive integers with maxGen greater than minGen")
  if (!(is.integer(pop)) | pop < 1)
    stop("pop must be a positive integer")

  # gaMethod: one of 'TN', 'LR', 'ER','RW'; and a numeric argument as appropriate -> methodFun & methodArgs
  method <- c('TN' = 'gaTNselection', 'LR' = 'gaLRselection',
                 'ER' = 'gaExpSelection', 'RW' = 'gaRWselection')
  if (!is.list(gaMethod) | (c('TN', 'ER') %in% gaMethod && length(gaMethod)!=2) |
      (c('LR', 'RW') %in% gaMethod && length(gaMethod)!=1) | sum(gaMethod %in% names(method))!=1) {
    stop("gaMethod must be a list, specifying one of ('TN', 'LR', 'ER', 'RW') and for ER or TN selection, the additional required parameter.")
  }
  methodFun <- method[which(names(method) %in% gaMethod)]
  if (methodFun=="gaTNselection") {
    if ((gaMethod[[2]]!=as.integer(gaMethod[[2]])) | gaMethod[[2]] > pop | length(gaMethod[[2]])!=1) {
      stop("gaMethod for 'TN' must additionally include an integer between 1 and the population size to specify the number of selection tournaments")
    } else methodArgs <- list("pop" = population, "fit" = fitness, "eliteRate" = eliteRate, "k" = gaMethod[[2]])
  } else if (methodFun=="gaExpSelection") {
    if (!is.numeric(gaMethod[[2]]) | length(gaMethod[[2]])!=1) {
      stop("gaMethod for 'ER' must additionally include an number to specify the exponential base")
    } else methodArgs <- list("pop" = population, "fit" = fitness, "eliteRate" = eliteRate, "c" = gaMethod[[2]])
  } else methodArgs <- list("pop" = population, "fit" = fitness, "eliteRate" = eliteRate)

  # pMutate & crossParams: proper probabilities & integer number of crosses
  if (!(is.numeric(pMutate)) | median(c(0, pMutate, 1))!=pMutate)
    stop("pMutate must be a number between 0 and 1")
  if (!is.numeric(crossParams) | length(crossParams)!=2 | median(c(0, crossParams[1], 1))!=crossParams[1] |
      !(as.integer(crossParams[2])==crossParams[2]) | median(c(1, crossParams[2], ncol(x)))!=crossParams[2])
    stop("crossParams must be a numeric vector of length 2. The first term specifying a probability between 0 and 1 and the second a positive integer")


  ######################################## GA ITERATIONS ########################################


  gen <- 1
  Stop = FALSE

  while(gen < maxGen) {
    # Regression
    fitness <- apply(population, 1, regress, x = x, y = y, model = model, fitnessCriteria = fitMetric)

    # Identify unique elite genotypes
    eliteFits <- head(order(fitness, decreasing = TRUE), max(0, ceiling(length(fitness)*eliteRate)))
    elites <- population[eliteFits, ]

    # update GA object
    GA[[gen]] <- list("fitness" = fitness, "elites" = cbind("fitness" = fitness[eliteFits], elites))

    # check stopping criteria
    if (gen > minGen) {
      fitHistory <- sapply((gen-minGen+1):gen, FUN = function(i) {
        GA[[i]]$fitMax - GA[[i-1]]$fitMax
      })
      Stop <- abs(sum(fitHistory)) <= .Machine$double.eps
    }
    if (Stop == TRUE) break

    # population selection
    methodArgs[c("pop", "fit")] <- list(population, fitness)
    population <- gaSelection(methodFun, methodArgs)[[1]]

    # offspring generation
    population <- evolve(population, pMutate, crossParams[1], crossParams[2])
    population <- rbind(elites, population)

    gen = gen + 1
  }


  ######################################## GA OUTPUT  ########################################

  GA <- GA[1:gen]

  fitStats <- t(sapply(1:length(GA), FUN = function(i) {
    c("Generation" = i,
      "Mean" = mean(GA[[i]]$fitness),
      "Median" = median(GA[[i]]$fitness),
      "Maximum" = GA[[i]]$fitMax)
  }))

   ggplot2::ggplot(tidyr::gather(tibble::as.tibble(fitStats), key = "Statistic", value = "Value", c(Mean, Median, Maximum))) +
     ggplot2::geom_point(ggplot2::aes(x = Generation, y = Value, colour = Statistic)) -> fitPlot

  fittest <- GA[[gen]]$elites[1, ]
  if (model=="glm") {
    fitModel <- eval(parse(text = paste0("glm.fit(cbind(x[, which(fittest[-1]==1)], 1), y, ", modelParams,")")))
    class(fitModel) <- "glm"
  } else {
    fitModel <- eval(parse(text = paste0("lm.fit(cbind(x[, which(fittest[-1]==1)], 1), y, ", modelParams,")")))
    class(fitModel) <- "lm"
  }

  fittest <- list("variables" = names(fittest)[fittest==1], 'fitness' = fittest[1], "fitModel" = fitModel)

  return(list("optimum" = fittest, "fitPlot" = fitPlot, "fitStats" = fitStats, "GA" = GA))
}

