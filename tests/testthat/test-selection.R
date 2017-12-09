library(testthat)
numVar <- 6
N <- 50
population <- matrix(rbinom(numVar*N, 1, prob = 0.5), N, numVar)
fitnessVec <- seq(15, 50, length.out=50)
eliteRate <- 0.05
m1 <- selection(population, fitnessVec, selectMethod=gaLRselection, eliteRate)
m2 <- selection(population, fitnessVec, selectMethod=gaExpSelection, eliteRate, c=0.5)
m3 <- selection(population, fitnessVec, selectMethod=gaRWselection, eliteRate)
m4 <- selection(population, fitnessVec, selectMethod=gaTNselection, eliteRate, k=5)

## test the dimension and number of output, class of output
test_that("selection returns a list with two elements", {
  expect_equal(class(selection(population, fitnessVec, selectMethod=gaLRselection, eliteRate)), "list")
  expect_equal(dim(selection(population, fitnessVec, selectMethod=gaLRselection, eliteRate)[[1]]), c(48,6))
  expect_equal(length(selection(population, fitnessVec, selectMethod=gaLRselection, eliteRate)[[2]]), 48)
  expect_equal(class(selection(population, fitnessVec, selectMethod=gaExpSelection, eliteRate, c=0.5)), "list")
  expect_equal(dim(selection(population, fitnessVec, selectMethod=gaExpSelection, eliteRate, c=0.5)[[1]]), c(48,6))
  expect_equal(length(selection(population, fitnessVec, selectMethod=gaExpSelection, eliteRate, c=0.5)[[2]]), 48)
  expect_equal(class(selection(population, fitnessVec, selectMethod=gaRWselection, eliteRate)), "list")
  expect_equal(dim(selection(population, fitnessVec, selectMethod=gaRWselection, eliteRate)[[1]]), c(48,6))
  expect_equal(length(selection(population, fitnessVec, selectMethod=gaRWselection, eliteRate)[[2]]), 48)
  expect_equal(class(selection(population, fitnessVec, selectMethod=gaTNselection, eliteRate, k=5)), "list")
  expect_equal(dim(selection(population, fitnessVec, selectMethod=gaTNselection, eliteRate, k=5)[[1]]), c(48,6))
  expect_equal(length(selection(population, fitnessVec, selectMethod=gaTNselection, eliteRate, k=5)[[2]]), 48)
})

## test whether population and fitness are updated
test_that("population and fitness are changing", {
  expect_false(identical(fitnessVec, m1[[2]]))
  expect_false(identical(fitnessVec, m2[[2]]))
  expect_false(identical(fitnessVec, m3[[2]]))
  expect_false(identical(fitnessVec, m4[[2]]))
  expect_false(identical(population, m1[[1]]))
  expect_false(identical(population, m2[[1]]))
  expect_false(identical(population, m3[[1]]))
  expect_false(identical(population, m4[[1]]))
})

## test arguments
test_that("arguments satisfies certain condition and some are not missing", {
  expect_error(selection(population, fitnessVec), "A selection method must be provided", fixed=TRUE)
  expect_error(selection(population, fitnessVec, selectMethod=2), "Selection method is not a function", fixed=TRUE)
  expect_error(selection(population, fitnessVec, selectMethod=gaLRselection, 0.5, c=0.5, k=5), "Cannot use tournament and nonlinear rank selection at the same time", fixed=TRUE)
  expect_error(selection(population, fitnessVec, selectMethod=gaLRselection), "A elite rate must be provided", fixed=TRUE)
  expect_error(selection(c(1,2,3,4,5), fitnessVec, selectMethod=gaLRselection, 0.5), "Population should be a matrix", fixed=TRUE)
  expect_error(selection(population, matrix(rnorm(4),2,2), selectMethod=gaLRselection, 0.5), "Fitness values should be a vector", fixed=TRUE)
  expect_error(selection(population, fitnessVec, selectMethod=gaLRselection, 1.5), "The elite rate must be between 0 and 1", fixed=TRUE)
  expect_error(selection(population, fitnessVec, selectMethod=gaExpSelection, 0.5, c=1.5), "The exponential base c must be between 0 and 1", fixed=TRUE)
  expect_error(selection(population, fitnessVec, selectMethod=gaExpSelection, 0.5), "The exponential base for nonlinear rank selection must be provided", fixed=TRUE)
  expect_error(selection(population, fitnessVec, selectMethod=gaTNselection, 0.5), "Number of random selection must be provided for tournament selection", fixed=TRUE)
  expect_error(selection(population, fitnessVec, selectMethod=gaTNselection, 0.5, k=0.5), "Number of random selections must be an integer", fixed=TRUE)
  expect_error(selection(population, fitnessVec, selectMethod=gaTNselection, 0.5, k=100), "Number of random selections cannot exceed shrinked population size", fixed=TRUE)
})





## test results
# source("selection-tests.R")
# test_file("selection-tests.R")
