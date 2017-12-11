\name{gaRWselection}
\alias{gaRWselection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Roulette Wheel Selection
}
\description{
In Roulette Wheel Selection method, the chromosomes are selected based on their probabilities
that are proportional to their fitness value. whole population in partitioned on the wheel and
each sector represents an individual. The proportion of individual's fitness to the total fitness
values of whole population decides the probability of selection of that individual in the next generation.
}
\usage{
gaRWselection(population, fitnessVec, eliteRate)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{population}{
a matrix of size n*p
}
  \item{fitnessVec}{
a vector with length n
}
  \item{eliteRate}{
a proportion between zero and one
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
numVar <- 6
N <- 50
population <- matrix(rbinom(numVar*N, 1, prob = 0.5), N, numVar)
fitnessVec <- seq(15, 50, length.out=50)
eliteRate <- 0.05
gaRWselectio(population, fitnessVec, eliteRate)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
