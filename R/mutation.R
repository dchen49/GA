###mutation code(works!!)

mutation <- function(population, mutation.prob = 0.1){
  index <- replicate(dim(population)[2],expr = runif(dim(population)[1])) < mutation.prob
  index2 <- population == 1
  population[which((index + index2) == 2)] <- 0
  index3 <- population == 0
  population[which((index + index3) == 2)] <- 1
  return(population)
}
