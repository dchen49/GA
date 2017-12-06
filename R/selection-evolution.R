##### Import Data
baseballData <- read.table("/Users/Cindy/Documents/Berkeley-Statistics/STA243-Computational-Statistics/Homework/final project/data/baseball.dat",
                           header = TRUE)
## log(salary) is the response
Y <- log(baseballData$salary) ## reponse
X <- as.matrix(baseballData[,2:dim(baseballData)[2]]) ## predictors

##### Fitness Function
## Function 1 - lm
## selFeature is a string with binary values with length nv, where nv is the total number of variables
## eg, 01001, 10110, etc
## covariates matrix: X
## response: Y
fitnessLM <- function(selFeature){
  feature <- which(selFeature == 1)
  x <- cbind(1,X[, feature]) ## x is the design matrix, with the first column as all ones for the intercept
  LMmod <- lm.fit(x,Y)
  class(LMmod) <- "lm"
  return (-AIC(LMmod))
} ## try to minimize AIC, equivalently, maximize -AIC



## Function 2 - glm
## familyFun is in class "family" defined in stats package, eg, gaussian()
fitnessGLM <- function(selFeature, familyFun){
  feature <- which(selFeature == 1)
  x <- cbind(1,X[, feature]) ## x is the design matrix, with the first column as all ones for the intercept
  GLMmod <- glm.fit(x,Y,familyFun)
  aic <- GLMmod$aic
  return (-aic)
}
## fitnessGLM(BinaryString) return negative AIC value

## Initial Population
numVar <- dim(X)[2]
N <- 50
population <- matrix(rbinom(numVar*N, 1, prob = 0.5), N, numVar) 
## matrix with size n*p, in which each row is a chromosome

fitness <- fitnessLM
################################################## Selection #################################################
##### Selection-select potential parents from initial population
## Linear Rank Selection
## For a population with size N, the best solution, the one with highest fitness has rank N, 
## the second best rank N-1, and the worst rank 1, etc
gaLRselection <- function(population){
  N <- dim(population)[1]
  ## first compute the individual fitness value for each chromosome in the population
  fitnessVec <- apply(population, 1, function(x) fitnessLM(x))
  ## fitnessVec is a vector of all the fitness values for current generation
  rank <- rank(fitnessVec, ties.method = "min") ## return corresponding rank for each fitness value
  denom <- N*(N+1)/2
  prob <- rank/denom
  sel <- sample(1:N, size = N, prob = prob, replace = TRUE)
  output <- list(population = population[sel,,drop=FALSE],
                 fitness = fitnessVec[sel])
  ## selected chromosomes with size N and corresponding fitness value
  return (output)
}

## Nonlinear Rank Selection (Exponential Rank Selection)
## almost the same as Linear Rank Selection, except the definition of probability
## base: exponential base, in (0,1)
## c is the base
gaExpSelection <- function(population, c){
  N <- dim(population)[1]
  ## first compute the individual fitness value for each chromosome in the population
  fitnessVec <- apply(population, 1, function(x) fitnessLM(x))
  ## fitnessVec is a vector of all the fitness values for current generation
  rank <- rank(fitnessVec, ties.method = "min")
  prob <- c^(N-rank) / sum(c^(N-rank))
  sel <- sample(1:N, size = N, prob = prob, replace = TRUE)
  output <- list(population = population[sel,,drop=FALSE],
                 fitness = fitnessVec[sel])
  return (output)
}

## Roulette Wheel Selection
gaRWselection <- function(population){
  N <- dim(population)[1]
  ## first compute the individual fitness value for each chromosome in the population
  fitnessVec <- apply(population, 1, function(x) fitnessLM(x))
  prob <- abs(fitnessVec) / sum(abs(fitnessVec))
  sel <- sample(1:N, size = N, prob = prob, replace = TRUE)
  output <- list(population = population[sel,,drop=FALSE],
                 fitness = fitnessVec[sel])
  return (output)
}

## Tournament Selection
## k is the number of random selection from population
gaTNselection <- function(population, k){
  selection <- rep(0,N)
  N <- dim(population)[1]
  ## first compute the individual fitness value for each chromosome in the population
  fitnessVec <- apply(population, 1, function(x) fitnessLM(x))
  for (i in 1:N){
    s <- sample(1:N, size=k)
    selection[i] <- s[which.max(fitnessVec[s])]
  }
  ## selection is a vector of selected rows
  output <- list(population = population[selection,,drop=FALSE],
                 fitness = fitnessVec[selection])
  return (output)
}


## selection-with survivor rate
## fitness is a function
Newselect <- function(population, fitness, survivor.rate, survivor.scaling){
  popSize <- dim(population)[1]
  p <- dim(population)[2]
  ## if input "fitness" is not a function, return Error: "fitness is not a function"
  if (class(population) != "matrix") stop("population is not in matrix form")
  if (class(fitness) != "function") stop("fitness is not a function")
  if (!is.numeric(survivor.rate)) stop("survivor.rate should be numeric")
  if (survivor.rate > 1) stop("survivor.rate cannnot exceed one")
  if (!is.numeric(survivor.scaling)) stop("survivor.scaling should be numeric")
  if (survivor.scaling > 1) stop("survivor.scaling cannot exceed one")
  fitness <- apply(population, 1, function(x) fitness(x)) ## fitness of given population
  num.survivor <- floor(popSize*survivor.rate)
  ordInd.top <- order(fitness, decreasing = TRUE)[1:num.survivor]
  pop.select <- population[ordInd.top,]
  fitness.select <- fitness[ordInd.top] ## ordered fitness of survivors
  remaining <- floor(popSize*(1-survivor.scaling)) ## remaining individuals generated by random
  remaining.pop <- matrix(rbinom(p*remaining, 1, prob = 0.5), remaining, p) ## random population 
  ## fitness of remaining random population
  remaining.fitness <- apply(remaining.pop, 1, function(x) fitness(x))
  Numadd.to <- floor(popSize*survivor.scaling) - num.survivor ## number of duplicates of survivors
  if (Numadd.to %% num.survivor == 0) {
    r <- 0
    numdup <- Numadd.to/num.survivor + 1
    dup <- do.call(rbind, replicate(numdup, pop.select, simplify=FALSE))
    dup.fitness <- rep(fitness.select, numdup) ## duplicated fitness value
  }
  else {
    r <- Numadd.to %% num.survivor 
    numdup <- Numadd.to%/%num.survivor + 1
    dup <- do.call(rbind, replicate(numdup, pop.select, simplify=FALSE))
    dup.fitness <- rep(fitness.select, numdup) ## duplicated fitness value
  }
  
  if (r==0) {
    population <- rbind(dup, remaining.pop)
    fitness <- c(dup.fitness, remaining.fitness)
  }
  else {
    population <- rbind(dup, pop.select[1:r,], remaining.pop)
    fitness <- c(dup.fitness, fitness.select[1:r], remaining.fitness)
  }
  return (list(population = population, fitness=fitness))
}

################################################# Evolution ##################################################
##### Mating 
## parents selection

## create a mating matrix with two columns as parents

##num.cross.locations = 1
singlecrossover <- function(parents, crossing.prob = 0.5, num.cross.locations = 1){
  n <- dim(population)[2] #number of variables
  children <- matrix(0, nrow = 2, ncol = n)
  crossoverPoint <- sample(seq(1.5, n, by = 1), size=1)
  children[1,] <- c(parents[1,(1:(crossoverPoint-0.5))], parents[2, (crossoverPoint+0.5):n])
  children[2,] <- c(parents[2,1:(crossoverPoint-0.5)], parents[1,(crossoverPoint+0.5):n])
  #childrenFitness <- apply(children, 1, function(x) fitnessLM(x))
  #output <- cbind(children, childrenFitness)
  return (children)
}

multiplecrossover <- function(parents, crossing.prob = 0.5, num.cross.locations = 2){
  n <- dim(parents)[2] #number of variables
  children <- matrix(0, nrow = 2, ncol = n)
  parents <- switch(parents, num.cross.locations = 2)
  return(parents)
}

##dim(parents)[2] is number of variables 
##switch only works when number of cross location is an even number
switch <- function(parents, num.cross.locations=2){
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


##testing switch function
debug(switch)
parent2 <- rbind(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1), c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
parent2
switch(parent2, num.cross.locations = 2) ##num.cross.location can not be larger than n<- the number of variable
switch(parent2, num.cross.locations = 3)
undebug(switch)


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
evolve <- function(population, fitness, mutation.prob=0.1, crossing.prob=0.5, num.cross.locations=1){
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
        #offspring <- rbind(offspring, parent[i:(i+1),])
      }
    }
  }else{
    for (i in seq(1, Nnew, by = 2)){
      if (crossing.prob > runif(1)){
        ## row number of parents in population
        offspring[i:(i+1),] <- multiplecrossover(parents[i:(i+1),], crossing.prob = 0.5, num.cross.locations = 2)
      }else{
        offspring[i:(i+1),] <- parents[i:(i+1),]
        #offspring <- rbind(offspring, parent[i:(i+1),])
      }
    }
  }
  population <- rbind(offspring, population[-mating,])
  ##Next part is for mutation
  population <- mutation(population, mutation.prob = 0.1)
  population_Fit <- apply(population, 1, function(x) fitness(x))
  output <- list(population = population, population_Fit = population_Fit)
  return(output)
}




mutation(population, mutation.prob = 0.7)


################################################# Offspring-Parent ###########################################

## If the replacement rate is provided, we use the given replacement rate; otherwise, use the default rate
## computed in the function
## ... : replacement rate

## if replacement rate is provided
##### replace the least 5%(fitness) population in the initial population with the top 5% population 
##### in the offspring
##### Survivor Selection
gaUpdatePop <- function(offspringMatrix, offspringFitness, parentMatrix, parentFitness, replace.rate){
  N <- length(parentFitness)
  ## first sort the fitness in descending order 
  childrenSort <- order(offspringFitness, decreasing = TRUE)[1:(floor(replace.rate*N))]
  PopSort <- tail(order(parentFitness, decreasing = TRUE), floor(replace.rate*N))
  parentMatrix[PopSort,] <- offspringMatrix[childrenSort,]
  parentFitness[PopSort] <- offspringFitness[childrenSort]
  population <- parentMatrix
  fitness <- parentFitness
  output <- list(population = population, fitness = fitness)
  return (output)
}

############################################### Update Population ############################################
Newpopulation <- function(parentMatrix, parentFitness, offspringMatrix, offspringFitness, replace.rate=NULL){
  ## no replacement rate provided
  if (is.null(replace.rate)) {
    ## offspring in descending order
    order.IndexC <- order(offspringFitness, decreasing = TRUE)
    order.fitnessC <- offspringFitness[order.IndexC]
    order.children <- offspringMatrix[order.IndexC,]
    
    ## parent in ascending order
    order.IndexP <- order(parentFitness, decreasing = TRUE)
    order.fitnessP <- parentFitness[order.IndexP]
    order.parent <- parentMatrix[order.IndexP,]
    
    ## take differences of offspring fitnesses and parent fitnesses
    diff.fitness <-  order.fitnessC - order.fitnessP
    threshold <- which(diff.fitness < 0)[1] 
    ## if threshold>1, threshold-1 corresponds to the position for switching
    if (threshold == 1) {
      population <- parentMatrix
      fitness <- parentFitness 
    } else {
      switch <- threshold - 1
      parentMatrix[(order.IndexP[1:switch]),] <- offspringMatrix[(order.IndexC[1:switch]),]
      parentFitness[(order.IndexP[1:switch])] <- offspringFitness[(order.IndexC[1:switch])]
      population <- parentMatrix
      fitness <- parentFitness
    }
    out <- list(population=population, fitness=fitness)
  } else {
    out <- gaUpdatePop(offspringMatrix, offspringFitness, parentMatrix,parentFitness, replace.rate)
  }
  return (out)
}

## use this newpopulation for the next generation, new loop

