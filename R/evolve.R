################################################# Evolution ##################################################
##### Mating
## parents selection

## create a mating matrix with two columns as parents

##num.cross.locations = 1
singlecrossover <- function(parents, crossing.prob = 0.5, num.cross.locations = 1){
  n <- dim(parents)[2] #number of variables
  children <- matrix(0, nrow = 2, ncol = n)
  crossoverPoint <- sample(seq(1.5, n, by = 1), size=1)
  children[1,] <- c(parents[1,(1:(crossoverPoint-0.5))], parents[2, (crossoverPoint+0.5):n])
  children[2,] <- c(parents[2,1:(crossoverPoint-0.5)], parents[1,(crossoverPoint+0.5):n])
  return (children)
}

multiplecrossover <- function(parents, crossing.prob = 0.5, num.cross.locations = 2){
  n <- dim(parents)[2] #number of variables
  children <- matrix(0, nrow = 2, ncol = n)
  parents <- cross(parents, num.cross.locations = 2)
  return(parents)
}

##dim(parents)[2] is number of variables
##cross only works when number of cross location is greater than one
cross <- function(parents, num.cross.locations=2){
  crossoverPoint <- sort(sample(seq(1.5, dim(parents)[2], by = 1), size=num.cross.locations))
  if (num.cross.locations %% 2 == 0){
    for (i in seq(1, num.cross.locations, by = 2)){
      crosspart <- parents[1,(crossoverPoint[i]+0.5):(crossoverPoint[i+1]-0.5)]
      parents[1,(crossoverPoint[i]+0.5):(crossoverPoint[i+1]-0.5)] <- parents[2,(crossoverPoint[i]+0.5):(crossoverPoint[i+1]-0.5)]
      parents[2,(crossoverPoint[i]+0.5):(crossoverPoint[i+1]-0.5)] <- crosspart
    }
  }else{
    for (i in seq(1, (num.cross.locations-1), by = 2)){
      crosspart <- parents[1,(crossoverPoint[i]+0.5):(crossoverPoint[i+1]-0.5)]
      parents[1,(crossoverPoint[i]+0.5):(crossoverPoint[i+1]-0.5)] <- parents[2,(crossoverPoint[i]+0.5):(crossoverPoint[i+1]-0.5)]
      parents[2,(crossoverPoint[i]+0.5):(crossoverPoint[i+1]-0.5)] <- crosspart
    }
    crosspart <- parents[1,(crossoverPoint[num.cross.locations]+0.5):dim(parents)[2]]
    parents[1, (crossoverPoint[num.cross.locations]+0.5):dim(parents)[2]] <- parents[2,(crossoverPoint[num.cross.locations]+0.5):dim(parents)[2]]
    parents[2,(crossoverPoint[num.cross.locations]+0.5):dim(parents)[2]] <- crosspart
  }
  return(parents)
}


###mutation code(works!!)

mutation <- function(population, mutation.prob = 0.1){
  index <- replicate(dim(population)[2],expr = runif(dim(population)[1])) < mutation.prob
  index2 <- population == 1
  population[which((index + index2) == 2)] <- 0
  index3 <- population == 0
  population[which((index + index3) == 2)] <- 1
  return(population)
}


##dim(population)[2] is number of variables
evolve <- function(population, mutation.prob=0.1, crossing.prob=0.5, num.cross.locations=1){
  ## dim(population)[1] is population size
  numMating <- floor(dim(population)[1]/2) ## number of pairs
  Nnew <- 2*numMating
  mating <- matrix(sample(1:Nnew, size=Nnew), nrow = 2) ##populate 7 and 2 be parents
  parents <- population[mating,]
  offspring <- matrix(nrow = Nnew, ncol = dim(population)[2])
  if (num.cross.locations == 1){
    for (i in seq(1, Nnew, by = 2)){#e.g numMating = 25 pairs 50single
      if (crossing.prob > runif(1)){
        ## row number of parents in population
        offspring[i:(i+1),] <- singlecrossover(parents[i:(i+1),], crossing.prob = 0.5, num.cross.locations = 1)
      }else{
        offspring[i:(i+1),] <- parents[i:(i+1),]
      }
    }
  }else{
    for (i in seq(1, Nnew, by = 2)){
      if (crossing.prob > runif(1)){
        ## row number of parents in population
        offspring[i:(i+1),] <- multiplecrossover(parents[i:(i+1),], crossing.prob = 0.5, num.cross.locations = 2)
      }else{
        offspring[i:(i+1),] <- parents[i:(i+1),]
      }
    }
  }
  population <- rbind(offspring, population[-mating,])
  ##Next part is for mutation
  population <- mutation(population, mutation.prob = 0.1)
  output <- list(population = population)
  return(output)
}

