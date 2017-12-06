#Input descriptions:
# x is an array of length p, where p is the number of coefficients;
#   each array represents one member of the population.
# y is the observation values.
# model is either glm or lm.
# fitnessCriteria is the metric that determines how good the model is.

regress <- function(x, y, model="glm", fitnessCriteria="AIC") {
  
  if (model != "glm" | "lm") stop("model must specify either glm or lm")
  
  #Fit model based on user input model type, lm() or glm()
  #Apply fit to each row (each member of population)
  if (model=="glm") {
    fitModel <- glm.fit(t(as.matrix(append(x[which(x==1)], 1))), y)
    class(fitModel) <- "glm"
  }
  else if (model=="lm") {
    fitModel <- lm.fit(t(as.matrix(append(x[which(x==1)], 1))), y)
    class(fitModel) <- "lm"
  }
  
  #Calculate fitness criteria based on user input criteria;
  #If criteria is not AIC or BIC, then it must be an
  #existing feature of the fitted model
  
  if (fitnessCriteria=="AIC") {
    performance <- -(AIC(fitModel))
  }
  else if (fitnessCriteria=="BIC") {
    performance <- -(BIC(fitModel))
  }
  else {
    performance <- fitModel$fitnessCriteria
  }
  
  return(performance)
}
