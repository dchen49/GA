# ##################################################### Testing Data ###############################################################
# ### generate linear regression model with response Y
# ### Y = X\beta, where X is of size n*p, and follow multivariate normal distribution
# ### The generate noises \epsilon, with size n*q
# ### resulting covariates matrix is \tilde{X} of size n*(p+q)
# ### \tilde{X} = cbind(X, \epsilon)
# ### We hope that our genetic algorithm can select the first p variables
# ### Next, we start to test our algorithm
#
#
# ## step1: function to generate X, of size n*p
# variable_matrix <- function(n,p) {
#   mu = matrix(0,p,1)
#   Sigma = matrix(0,p,p)
#   for (i in 1:p){
#     for (j in 1:p){
#       Sigma[i,j] = 0.5^(abs(i-j))
#     }
#   }
#   vm = mvtnorm::rmvnorm(n = n, mu, Sigma)
#   return (vm)
# }
# ## generate X with size n*p
#
# ## step2: generate response Y
# p <- 20
# X <- variable_matrix(300,20)
# beta <- rnorm(p, mean=3, sd=5)
# Y <- X%*%beta
#
# ## combine X with noise \epsilon
# X.tilde <- cbind(X, variable_matrix(300,10)) ## 10 uncorrelated variables
#
# ## first column-Y, 2-21: X, 22-31: noise
# data <- cbind(Y, X.tilde)
# write.csv(data, file = "./data/LRdataTest.csv",row.names=FALSE)
# write.table(data, file="./data/LRdataTest")
