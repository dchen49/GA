library(testthat)
numVar <- 6
N <- 50
crossParams <- c(0.5,2)
pMutate <- 0.1
population <- matrix(rbinom(numVar*N, 1, prob = 0.5), N, numVar)

test_that("selection returns a list with two elements", {
  expect_equal(class(evolve(population, pMutate, crossParams[1], crossParams[2])), "matrix")
  expect_equal(dim(evolve(population, pMutate, crossParams[1], crossParams[2])), c(50,6))
})

## test arguments
test_that("arguments satisfies certain condition and some are not missing", {
  expect_error(evolve(population, 2, crossParams[1], crossParams[2]), "mutation probability must be between 0 and 1", fixed=TRUE)
  expect_error(evolve(population, pMutate, 1.5, crossParams[2]), "crossover probability must be between 0 and 1", fixed=TRUE)
  expect_error(evolve(population, pMutate, crossParams[1], 2.5), "num.cross.locations must be an integer", fixed=TRUE)
  expect_error(evolve(population, pMutate, crossParams[1], 7), "num.cross.locations must be smaller than the number of variables", fixed=TRUE)
})
