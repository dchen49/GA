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
      * Performs regression
      * Calculate fitness
        * -> regressOutput: vector of fitness metrics
    * stop (regressOutput, fitness)
      * Test that the current generation fitness improves upon the last by some threshold
        * -> Boolean: TRUE if improvement insignificant
    * if (Stop==true) Break
    * mate (population, fitness, survivor percentage, selection method/params, population size)
      * selects high fitness genotypes via selection method
        * -> elite population
    * evolve(population, mutation prob, crossing prob, # of cross locations)
      * crossing(population, crossing prob, # of cross locations)
        * assign pairs
        * decide whether or not to cross, and crossing locations
            * -> population = populationCrossed
      * mutate(population, mutation prob)
        * picks locations and changes values
          * -> population = populationCrossedMutated
    * counter +1
    * pass new population back into while loop!

