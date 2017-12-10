\name{evolve}
\alias{evolve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Mutation and Genetic Crossing
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
\code{\link{singlecrossover}}{\cr}
\code{\link{multiplecrossover}}{\cr}
\code{\link{mutation}}{\cr}
}
\examples{
data <- matrix(sample(c(0, 1), 150, replace = TRUE), ncol = 10)
evolve(data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
