library(testthat)
numVar <- 6
N <- 50
population <- matrix(rbinom(numVar*N, 1, prob = 0.5), N, numVar)
fitnessVec <- seq(15, 50, length.out=50)
m1 <- selection(population, fitnessVec, selectMethod=gaLRselection)
m2 <- selection(population, fitnessVec, selectMethod=gaExpSelection, c=0.5)
m3 <- selection(population, fitnessVec, selectMethod=gaRWselection)
m4 <- selection(population, fitnessVec, selectMethod=gaTNselection, k=5)

## test the dimension and number of output, class of output
test_that("selection returns a list with two elements", {
  expect_equal(class(selection(population, fitnessVec, selectMethod=gaLRselection)), "list")
  expect_equal(dim(selection(population, fitnessVec, selectMethod=gaLRselection)[[1]]), c(50,6))
  expect_equal(length(selection(population, fitnessVec, selectMethod=gaLRselection)[[2]]), 50)
  expect_equal(class(selection(population, fitnessVec, selectMethod=gaExpSelection, c=0.5)), "list")
  expect_equal(dim(selection(population, fitnessVec, selectMethod=gaExpSelection, c=0.5)[[1]]), c(50,6))
  expect_equal(length(selection(population, fitnessVec, selectMethod=gaExpSelection, c=0.5)[[2]]), 50)
  expect_equal(class(selection(population, fitnessVec, selectMethod=gaRWselection)), "list")
  expect_equal(dim(selection(population, fitnessVec, selectMethod=gaRWselection)[[1]]), c(50,6))
  expect_equal(length(selection(population, fitnessVec, selectMethod=gaRWselection)[[2]]), 50)
  expect_equal(class(selection(population, fitnessVec, selectMethod=gaTNselection, k=5)), "list")
  expect_equal(dim(selection(population, fitnessVec, selectMethod=gaTNselection, k=5)[[1]]), c(50,6))
  expect_equal(length(selection(population, fitnessVec, selectMethod=gaTNselection, k=5)[[2]]), 50)
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
  expect_error(selection(population, fitnessVec, selectMethod=gaLRselection, c=0.5, k=5), "Cannot use tournament and nonlinear rank selection at the same time", fixed=TRUE)
  expect_error(selection(c(1,2,3,4,5), fitnessVec, selectMethod=gaLRselection), "Population should be a matrix", fixed=TRUE)
  expect_error(selection(population, matrix(rnorm(4),2,2), selectMethod=gaLRselection), "Fitness values should be a vector", fixed=TRUE)
  expect_error(selection(population, fitnessVec, selectMethod=gaExpSelection, c=1.5), "The exponential base c must be between 0 and 1", fixed=TRUE)
  expect_error(selection(population, fitnessVec, selectMethod=gaExpSelection), "The exponential base for nonlinear rank selection must be provided", fixed=TRUE)
  expect_error(selection(population, fitnessVec, selectMethod=gaTNselection), "Number of random selection must be provided for tournament selection", fixed=TRUE)
  expect_error(selection(population, fitnessVec, selectMethod=gaTNselection, k=0.5), "Number of random selections must be an integer", fixed=TRUE)
  expect_error(selection(population, fitnessVec, selectMethod=gaTNselection, k=100), "Number of random selections cannot exceed population size", fixed=TRUE)
})

## test of survivor selection
offspringPop <- matrix(rbinom(6*100, 1, prob = 0.5), 100, 6)
offFitness <- seq(15, 100, length.out=100)
replace.rate <- 0.1
Pop.switch <- cbind(seq(2, 50, length.out=10), matrix(rbinom(6*10, 1, prob = 0.5), 10, 6))

## test arguments
test_that("test arguments in Newpopulation function", {
  expect_error(Newpopulation(c(1,2,3,4,5), offFitness, Pop.switch, replace.rate), "Population should be a matrix", fixed=TRUE)
  expect_error(Newpopulation(offspringPop, matrix(rnorm(4),2,2), Pop.switch, replace.rate), "Fitness values should be a vector", fixed=TRUE)
  expect_error(Newpopulation(offspringPop, offFitness, c(1,2,3,4,5), replace.rate), "Original Population should be a matrix", fixed=TRUE)
  expect_error(Newpopulation(offspringPop, offFitness, matrix(rbinom(6*10, 1, prob = 0.5), 10, 6), replace.rate), "Either dimension of offspring population or Pop.switch is incorrect", fixed=TRUE)
  expect_error(Newpopulation(Newpopulation(offspringPop, offFitness, Pop.switch, 1.5)), "The replace.rate must be between 0 and 1", fixed=TRUE)
})

## test output
test_that("test output from Newpopulation function", {
  expect_equal(dim(Newpopulation(offspringPop, offFitness, Pop.switch, replace.rate)[[1]]), c(100,6))
})



## test results
# source("selection-tests.R")
# test_file("selection-tests.R")
