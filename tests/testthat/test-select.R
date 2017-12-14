library(testthat)
library(GA)

## generate outputs
########################### GA Algorithm Output from Simulate Data
## How data generated?
## five variables from five different distributions
## compute fitness for all possible genotypes
## whether search the local minimum
## x1-normal, x2-uniform, x3-Poisson, x4-Exp, x5-Binomial
N <- 300
set.seed(100)
X <- cbind(rnorm(N, mean=0, sd=5), runif(N), rpois(N, lambda=1), rexp(N, rate=2), rgamma(N, shape = 10))
colnames(X) <- 1:5
## Y is the average of Normal and Poisson
Y <- apply(X[,c(1,3)], 1, function(x) mean(x))

## all possible genotypes
library(gtools)
library(readr)
x <- c(0,1)
p <- 5
genotype <- permutations(n=2,r=5,v=x,repeats.allowed=T) ## 2^p genotypes in total
## write data (Y,X) as CSV file
data <- cbind(Y,X)
colnames(data) <- c("Y", "x1", "x2", "x3", "x4", "x5")
## write.csv(data, file = "~/simuData.csv",row.names=FALSE)

## generate summary tables
## best fitness-searching all fitness and find the local optima
m11 <- matrix(0,2,2)
## selected genotype-searching all fitness and find the local optima
m12 <- matrix(0,2,2)
## best fitness-select()
m21 <- matrix(0,2,2)
m22 <- matrix(0,2,2)
criterion <- c("AIC", "BIC")
mod <- c("lm", "glm")
for (i in 1:2){
  for (j in 1:2){
    ## search for all genotypes
    fit1 <- apply(genotype, 1, function(x) regress(x,X,Y, model=mod[i], fitnessCriteria=criterion[j]))
    ind <- which(fit1==max(fit1))
    bestfit1 <- fit1[ind]
    m11[i,j] <- bestfit1
    bestGen <- genotype[ind,]
    #pos <- paste(which(bestGen==1), collapse = "")
    m12[i,j] <- paste(bestGen, collapse = "")

    ## use select()
    out2 <- select(X,Y, model=list(mod[i]), fitMetric = criterion[j])
    bestfit2 <- out2$optimum$fitness
    m21[i,j] <- bestfit2
    strg <- paste(out2$optimum$variables, collapse = "")
    num <- as.numeric(unlist(strsplit(strg, split="")))
    au <- rep(0,5)
    au[num] <- 1
    m22[i,j] <- paste(au, collapse = "")
  }
}

## save all optimal genotypes
## change m12 and m22 to matrices with genotypes
colnames(m11) <- c("AIC", "BIC")
rownames(m11) <- c("lm", "glm")
colnames(m12) <- c("AIC", "BIC")
rownames(m12) <- c("lm", "glm")
colnames(m21) <- c("AIC", "BIC")
rownames(m21) <- c("lm", "glm")
colnames(m22) <- c("AIC", "BIC")
rownames(m22) <- c("lm", "glm")

## m11: optimal fitness value for global search
## m12: selected genotype for global search
## m21: optimal fitness value for GA
## m22: selected genotype for GA

context("Testing that select() finds the known global optimum")
test_that("Outputs between global search and GA algorithm are equal", {
  expect_equal(m11, m21)
  expect_equal(m12, m22)
})








