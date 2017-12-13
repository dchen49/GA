##Tests combinations of fitness criteria and model for fixed other inputs
##Outputs two matrices, one of max fitness value and one of # iterations to reach that value

input <- read.table("data/baseball.dat", header = TRUE)
x <- matrix(unlist(input[, 2:28]), 337, 27)
y <- matrix(as.numeric(input[, 1]))

maxFits <- matrix(0, 4, 4)
maxIters <- matrix(0, 4, 4)

method <- list(list('TN', 5), list('LR'), list('ER', 0.5), list('RW'))
fit <- c("AIC", "BIC")

for (i in 1:4) {
  for (j in 1:2) {
      trial <- select(x, y, model = list("lm"), fitMetric = fit[j], maxGen = 500L, minGen = 50L,
             gaMethod = method[[i]], pop = 500L, pMutate = 0.1, crossParams = c(0.8, 1L), eliteRate = 0.1)
      iters <- length(trial$GA)
      bestFit <- eval(parse(text = paste0("trial$GA$gen", iters, "$fitMax")))
      maxFits[i,j] <- bestFit
      maxIters[i,j] <- iters
  }
  for (j in 3:4) {
      trial <- select(x, y, model = list("glm"), fitMetric = fit[j-2], maxGen = 500L, minGen = 50L,
              gaMethod = method[[i]], pop = 500L, pMutate = 0.1, crossParams = c(0.8, 1L), eliteRate = 0.1)
      iters <- length(trial$GA)
      bestFit <- eval(parse(text = paste0("trial$GA$gen", iters, "$fitMax")))
      maxFits[i,j] <- bestFit
      maxIters[i,j] <- iters
  }
}

colnames(maxFits) <- c("lm, AIC", "lm, BIC", "glm, AIC", "glm, BIC")
rownames(maxFits) <- c("TN", "LR", "ER", "RW")

colnames(maxIters) <- c("lm, AIC", "lm, BIC", "glm, AIC", "glm, BIC")
rownames(maxIters) <- c("TN", "LR", "ER", "RW")
