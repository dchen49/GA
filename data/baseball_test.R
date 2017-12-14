##Tests combinations of fitness criteria and model for fixed other inputs
##Outputs two matrices, one of max fitness value and one of # iterations to reach that value

# x <- as.matrix(read.table("data/baseball.dat", header = TRUE)[,-1])
# y <- as.matrix(read.table("data/baseball.dat", header = TRUE)[,1])
#
#
# maxFits <- matrix(0, 4, 4)
# maxIters <- matrix(0, 4, 4)
# numVars2 <- matrix(0, 4, 4)
# trace <- list(rep(0, 16))
#
# method <- list(list('TN', 5), list('LR'), list('ER', 0.5), list('RW'))
# fit <- c("AIC", "BIC")
#
# for (i in 1:4) {
#   for (j in 1:2) {
#       print(paste(c("lm",fit[j], method[[i]][[1]])))
#       trial <- GA::select(x, y, model = list("lm"), fitMetric = fit[j], maxGen = 500L, minGen = 50L,
#              gaMethod = method[[i]], pop = 500L, pMutate = 0.1, crossParams = c(0.8, 1L), eliteRate = 0.1)
#       iters <- length(trial$GA)
#       bestFit <- eval(parse(text = paste0("trial$GA$gen", iters, "$elites[1,1]")))
#       maxFits[i,j] <- bestFit
#       maxIters[i,j] <- iters
#       # print(trial$optimum$variables)
#       numVars[i,j] <- length(trial$optimum$variables)
#       trace[[(i-1)*4+j]] <- trial
#
#   }
#   for (j in 3:4) {
#       print(paste(c("glm",fit[j-2], method[[i]][[1]])))
#       trial <- GA::select(x, y, model = list("glm"), fitMetric = fit[j-2], maxGen = 500L, minGen = 50L,
#               gaMethod = method[[i]], pop = 500L, pMutate = 0.1, crossParams = c(0.8, 1L), eliteRate = 0.1)
#       iters <- length(trial$GA)
#       bestFit <- eval(parse(text = paste0("trial$GA$gen", iters, "$elites[1,1]")))
#       maxFits[i,j] <- bestFit
#       maxIters[i,j] <- iters
#       numVars[i,j] <- length(trial$optimum$variables)
#       trace[[(i-1)*4+j]] <- trial
#
#   }
# }
#
# colnames(maxFits) <- c("lm, AIC", "lm, BIC", "glm, AIC", "glm, BIC")
# rownames(maxFits) <- c("TN", "LR", "ER", "RW")
#
# colnames(maxIters) <- c("lm, AIC", "lm, BIC", "glm, AIC", "glm, BIC")
# rownames(maxIters) <- c("TN", "LR", "ER", "RW")
