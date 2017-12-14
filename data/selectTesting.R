
########################### TESTING ON BASEBALL DATA ###########################

# x <- as.matrix(read.table("data/baseball.dat", header = TRUE))[, -1]
# y <- as.matrix(read.table("data/baseball.dat", header = TRUE))[, 1]
# ga1 <- select(x, y)
# ga1[[1]]
# ga1[[2]]
#
#
# ########################### TESTING O N LRDATA ###########################
# x <- as.matrix(read.table("data/LRdataTest"), header = TRUE)[, -1]
# colnames(x) <- sapply(1:30, FUN = function(i) paste0("X", i))
# y <- as.matrix(read.table("data/LRdataTest"), header = TRUE)[, 1]
# ga2 <- select(x, y, eliteRate = .05)
# ga2[[1]]
# ga2[[2]]
# OptModel <- ga2$optimum$fitModel

########################### TESTING ON TOY DATASET ###########################
# # Toy regression whhere only X[, 1:10] are meaningfully correlated with Y
# # Y = mean(X[, 1:10])
# # X[, 1:10] ~ N(500, 4)
# # X[, 11:100] ~ Unif(-1000, 1000)
#
# library(foreach)
# library(parallel)
# library(doParallel)
# library(ggplot2)
#
# registerDoParallel(8)  # change as needed
# time <- system.time(
# gaComparison <- foreach(i=1:100,
#                         .packages = "GA",
#                         .combine = c,
#                         .verbose = TRUE) %dopar% {
#                           dummyX <- cbind(matrix(rnorm(10*250, 500, 2), ncol = 10, dimnames = list(1:250, 1:10)),
#                                           matrix(runif(90*250, -1000, 1000), ncol = 90, dimnames = list(1:250, 11:100)))
#                           dummyY <- apply(dummyX[, 1:10], 1, mean)
#                           variables <- unlist(select(dummyX, dummyY, minGen = 10L)$optimum$variables)
#                         }
# )
# gaComparisonPlot <- ggplot() + stat_count(aes(x = as.integer(gaComparison))) + xlab("Variables")
# save.image(file = "./data/toyDataTest.Rdata")


########################### TESTING ON TOY DATASET ###########################
# # Toy regression whhere only X[, 1:10] are meaningfully correlated with Y
# # Y = mean(X[, 1:10])
# # X[, 1:10] ~ N(500, 4)
# # X[, 11:100] ~ Unif(-1000, 1000)

# library(foreach)
# library(parallel)
# library(doParallel)
#
# # x <- as.matrix(read.table("data/LRdataTest"), header = TRUE)[, -1]
# # colnames(x) <- sapply(1:30, FUN = function(i) paste0("x", i))
# # y <- as.matrix(read.table("data/LRdataTest"), header = TRUE)[, 1]
#
# x <- as.matrix(read.table("data/baseball.dat", header = TRUE))[, -1]
# y <- as.matrix(read.table("data/baseball.dat", header = TRUE))[, 1]
#
# w <- sapply(1:50, FUN = function(i) {z <- select(x, y)$optimum$fitModel$coefficients})
# weights <- rowSums(abs(w))
# names(weights) <- c('avg', 'obp', 'run', 'hit', 'dbl', 'trp', 'hr', 'rbi', 'w', 'sos', 'sbs', 'err', 'fa', 'arb', 'r/so', 'h/so', 'w/so', 'o/e', 'r/e', 'h/e', 'hr/e', '', 'sbsop', 'sbsrns', 'sbshts')
# barplot(weights)
