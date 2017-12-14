library(testthat)

numVar <- 10
dataSize <- 50

genotype <- array(rbinom(numVar, 1, prob = 0.5))
x <- matrix(1:dataSize*numVar, dataSize, numVar)
y <- array(1:dataSize)

myFun <- function(obj) {
  return(sum(obj$residuals))
}

context("Testing regress() output formats")

#test that output is single numerical values
test_that("output is single numerical value", {
  expect_equal(class(regress(genotype, x, y)), "numeric")
  expect_equal(length(regress(genotype, x, y)), 1)
})

context("Testing regress() function with different models/criteria")

#test that function can handle AIC and BIC as fitness criteria for both lm and glm
test_that("function handles various fitness criteria", {

  #testing for lm
  expect_true(is.numeric(regress(genotype, x, y, model =  "lm", fitnessCriteria = "AIC")))
  expect_true(is.numeric(regress(genotype, x, y, model =  "lm", fitnessCriteria = "BIC")))
  expect_true(is.numeric(regress(genotype, x, y, model =  "lm", fitnessCriteria = "myFun")))
  expect_error(regress(genotype, x, y, model =  "lm", fitnessCriteria = "gibberish"))

  #testing for glm
  expect_true(is.numeric(regress(genotype, x, y, model =  "glm", fitnessCriteria = "AIC")))
  expect_true(is.numeric(regress(genotype, x, y, model =  "glm", fitnessCriteria = "BIC")))
  expect_true(is.numeric(regress(genotype, x, y, model =  "glm", fitnessCriteria = "myFun")))
  expect_error(regress(genotype, x, y, model =  "glm", fitnessCriteria = "gibberish"))
})


#test that function checks for correct model input
test_that("function errors with incorrect model input", {
  expect_error(regress(genotype, x, y, model = "gibberish"))
  expect_error(regress(genotype, x, y, model = 1))
})


