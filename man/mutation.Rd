\name{mutation}
\alias{mutation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%
}
\description{
This function will randomly change 1 to 0 or 0 to 1. The default probability of mutation is 0.1. The mutation function is used in evolve function.
}
\usage{
mutation(population, mutation.prob = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{population}{
\code{population matrix of dimension n * p}
}
  \item{mutation.prob}{
\code{mutation probability}
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
Qi Chen
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{evolve}}
}
\examples{
data <- matrix(sample(c(0, 1), 1000, replace = TRUE), ncol = 20)
mutation(data)



## The function is currently defined as
function (population, mutation.prob = 0.1)
{
    index <- replicate(dim(population)[2], expr = runif(dim(population)[1])) <
        mutation.prob
    index2 <- population == 1
    population[which((index + index2) == 2)] <- 0
    index3 <- population == 0
    population[which((index + index3) == 2)] <- 1
    return(population)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
