\name{gaTNselection}
\alias{gaTNselection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tournament Selection
}
\description{
In Tournament selection method, 'n' individuals are chosen at random from the entire population. These
individuals compete against each other. The individual with the highest fitness value wins and gets
selected for further processing of Genetic Algorithm.
}
\usage{
gaTNselection(population, fitnessVec, eliteRate, k)
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
  \item{k}{
number of random selections
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
N <- 50
population <- matrix(rbinom(numVar*N, 1, prob = 0.5), N, numVar)
fitnessVec <- seq(15, 50, length.out=50)
eliteRate <- 0.05
k <- 5
gaTNselection(population, fitnessVec, eliteRate, k)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

