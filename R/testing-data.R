##################################################### Testing Data ###############################################################
### generate linear regression model with response Y
### Y = X\beta, where X is of size n*p, and follow multivariate normal distribution
### The generate noises \epsilon, with size n*q
### resulting covariates matrix is \tilde{X} of size n*(p+q)
### \tilde{X} = cbind(X, \epsilon)
### We hope that our genetic algorithm can select the first p variables
### Next, we start to test our algorithm

testdata <- new.env()

library(mvtnorm)
library(MASS)
library(pls)

## step1: function to generate X, of size n*p
testdata$variable_matrix <- function(n,p) {
  mu = matrix(0,p,1)
  Sigma = matrix(0,p,p)
  for (i in 1:p){
    for (j in 1:p){
      Sigma[i,j] = 0.5^(abs(i-j))
    }
  }
  vm = mvrnorm(n = n, mu, Sigma)
  return (vm)
}
## generate X with size n*p

## step2: generate response Y
testdata$p <- 20
testdata$X <- testdata$variable_matrix(300,20)
testdata$beta <- rnorm(testdata$p, mean=3, sd=5)
testdata$Y <- testdata$X%*%testdata$beta

## combine X with noise \epsilon
testdata$X.tilde <- cbind(testdata$X, testdata$variable_matrix(300,10)) ## 10 uncorrelated variables

## first column-Y, 2-21: X, 22-31: noise
testdata$data <- cbind(testdata$Y, testdata$X.tilde)
write.csv(testdata$data, file = "./data/LRdataTest.csv",row.names=FALSE)
write.table(testdata$data, file="./data/LRdataTest")

unloadNamespace("mvtnorm")
unloadNamespace("MASS")
unloadNamespace("pls")
rm("testdata")

