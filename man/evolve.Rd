\name{evolve}
\alias{evolve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
evolve(population, mutation.prob = 0.1, crossing.prob = 0.5, num.cross.locations = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{population}{
\code{matrix of dimension n * p}
}
  \item{mutation.prob}{
\code{mutation probability}
}
  \item{crossing.prob}{
\code{crossover probability}
}
  \item{num.cross.locations}{
\code{number of locations where crossover happens, defalut value is one}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{singlecrossover}}
\code{\link{multiplecrossover}}
\code{\link{mutation}}
}
\examples{
data <- matrix(sample(c(0, 1), 150, replace = TRUE), ncol = 10)
evolve(data)

## The function is currently defined as
function (population, mutation.prob = 0.1, crossing.prob = 0.5,
    num.cross.locations = 1)
{
    numMating <- floor(dim(population)[1]/2)
    Nnew <- 2 * numMating
    mating <- matrix(sample(1:Nnew, size = Nnew), nrow = 2)
    parents <- population[mating, ]
    offspring <- matrix(nrow = Nnew, ncol = dim(population)[2])
    if (num.cross.locations == 1) {
        for (i in seq(1, Nnew, by = 2)) {
            if (crossing.prob > runif(1)) {
                offspring[i:(i + 1), ] <- singlecrossover(parents[i:(i +
                  1), ])
            }
            else {
                offspring[i:(i + 1), ] <- parents[i:(i + 1),
                  ]
            }
        }
    }
    else {
        for (i in seq(1, Nnew, by = 2)) {
            if (crossing.prob > runif(1)) {
                offspring[i:(i + 1), ] <- multiplecrossover(parents[i:(i +
                  1), ], num.cross.locations = 2)
            }
            else {
                offspring[i:(i + 1), ] <- parents[i:(i + 1),
                  ]
            }
        }
    }
    population <- rbind(offspring, population[-mating, ])
    population <- mutation(population, mutation.prob = 0.1)
    output <- list(population = population)
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
