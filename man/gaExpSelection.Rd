\name{gaExpSelection}
\alias{gaExpSelection{GA}}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Exponential Ranking Selection
}
\description{
In nonlinear Ranking selection method, individuals are first sorted according to their fitness value
and then the ranks are assigned to them. Best individual gets rank 'N' and the worst one gets rank '1'.
The selection probabilities here are exponentially weighted. The base of the exponent is c.
}
\usage{
gaExpSelection(population, fitnessVec, eliteRate, c)
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
  \item{c}{
exponential base
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
c <- 0.5
gaExpSelection(population, fitnessVec, eliteRate, c)


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

