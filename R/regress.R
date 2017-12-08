## regress {GA}
# Regression on variable subset

#' Runs regression using a subset of variables dictated by the genotype
# '
#' @param genotype array of length p made up of 0s and 1s; represents a single member of the population
#' @param x matrix of dimension n * p
#' @param y vector of length n or a matrix with n rows
#' @param model vector specifying the model: the first argument should be "lm" or "glm", and subsequent arguments specify additional arguments into "lm" or "glm"
#' @param fitnessCriteria default "AIC", a string specifying the fitness criterion: "AIC", "BIC", or "TBD"

regress <- function(genotype, x, y, model="glm", fitnessCriteria="AIC") {
  
  if (model != "glm" | "lm") stop("model must specify either glm or lm")
  
  #Fit model based on user input model type, lm() or glm()
  #Apply fit to each row (each member of population)
  if (model=="glm") {
    fitModel <- glm.fit(cbind(x[, which(genotype==1)], 1), y)
    class(fitModel) <- "glm"
  }
  else if (model=="lm") {
    fitModel <- lm.fit(cbind(x[, which(genotype==1)], 1), y)
    class(fitModel) <- "lm"
  }
  
  #Calculate fitness criteria based on user specified criteria
  
  if (fitnessCriteria=="AIC") {
    performance <- -(AIC(fitModel))
  }
  else if (fitnessCriteria=="BIC") {
    performance <- -(BIC(fitModel))
  }
  else {
    performance <- eval(parse(paste0("fitModel$",fitnessCriteria)))
  }
  
  return(performance)
}
