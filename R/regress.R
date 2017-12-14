## regress {GA}
# Regression on variable subset

#' Runs regression using a subset of variables dictated by the genotype
# '
#' @param genotype array of length p made up of 0s and 1s; represents a single member of the population
#' @param x matrix of dimension n * p
#' @param y vector of length n or a matrix with n rows
#' @param model default "glm", a string either "lm" or "glm"
#' @param fitnessCriteria default "AIC", a string specifying the fitness criterion: "AIC", "BIC", or an attribute of a fitted lm or glm model (must be single numeric value)
#' @param modelParams default NULL, a string specifying additional arguments into lm.fit() or glm.fit()
#' @examples
#' numVar <- 10
#' dataSize <- 50
#' genotype <- array(rbinom(numVar, 1, prob = 0.5))
#' x <- matrix(1:dataSize*numVar, dataSize, numVar)
#' y <- array(1:dataSize)
#' regress(genotype, x, y, model =  "lm", fitnessCriteria = "AIC")

regress <- function(genotype, x, y, model="glm", fitnessCriteria="AIC", modelParams=NULL) {

  #Check that user correctly inputs a lm() or glm() model
  if (model != "glm" && model != "lm") stop("model must specify either glm or lm")

  #Fit model based on user input model type; adds in model params as necessary
  #Ater fitting, confirm that model creation completed without error
  if (model=="glm") {
    fitModel <- eval(parse(text = paste0("try(glm.fit(cbind(x[, which(genotype==1)], 1), y, ", modelParams,"))")))
    if("try-error" %in% class(fitModel)) stop(fitModel[1])
    else class(fitModel) <- "glm"
  } else if (model=="lm") {
    fitModel <- eval(parse(text = paste0("try(lm.fit(cbind(x[, which(genotype==1)], 1), y, ", modelParams,"))")))
    if("try-error" %in% class(fitModel)) stop(fitModel[1])
    else class(fitModel) <- "lm"
  }

  #Calculate fitness criteria based on user specified criteria
  #Confirm that fitnessCriteria is correct by checking that calculation completed without error
  performance <- try (-eval(parse(text = paste0(fitnessCriteria, "(fitModel)"))), silent = TRUE)
  if("try-error" %in% class(performance)) stop("Fitness criteria function is not correct.")
  return(performance)
}

myFun <- function(obj) {
  return(sum(obj$residuals))
}
