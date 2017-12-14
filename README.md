# Stat 243 Final Project
## Variable Selection via Genetic Algorithm

### Work in Progress
* David Chen
* Qi Chen
* Emily Suter
* Xinyi(Cindy) Zhang

### Select() Outline

* Select (y, x, tbd, ...)
  * initialize(p = initial population size, n = ncol(x))
      * make a p x n matrix of random Bernoullis
        * -> population = p x n matrix
  * While (count < max iterations)
    * regress (x, y, model, fitness function, ...) # apply(population, regress)
      * Performs regression # lm.fit(Y, cbind(1s, x[, which(genotype == 1)]))
      * Calculate fitness
        * -> regressOutput: vector of fitness metrics
    * Survivor selection??
      * Remove bad fitness model
      * Keep good fitness models from previous generation and current generation
    * Stop (regressOutput, fitness)
      * Test that the current generation fitness improves upon the last by some threshold
      * update fitnessNew -> fitness
      * -> Boolean: TRUE if improvement insignificant
    * count++
    * if (Stop==true) Break
    * cbind(population, fitness)
    * selection (population, fitness, survivor percentage, survivor scaling, population size)
      * selects high fitness genotypes, amplifies via scaling, fills in the rest
      * selection strategy #2 tournament?
      * selection strategy #3 
        * -> population = populationNew, p x n matrix of next gen. genotypes
    * evolve(population, mutation prob, crossing prob, # of cross locations)
      * crossing(population, crossing prob, # of cross locations)
        * assign pairs
        * decide whether or not to cross
          * number of crosses
          * crossing locations
            * -> population = populationCrossed
      * mutate(population, mutation prob)
        * picks locations (maybe number)
          * -> population = populationCrossedMutated
    * choice(population, fitness)
      * return all unique genotypes associated with max fitness value
